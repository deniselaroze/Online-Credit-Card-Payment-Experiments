###### Setup ######
# Libraries
rm(list=ls())
library(gridExtra)
library(dplyr)
library(tidyr)
library(stargazer)
library(reshape2)
library(htmltools)
library(htmlTable)
library(kableExtra)
library(knitr)
library(ggplot2)
library(scales)
library(readxl)
library(tibble)
library(lmtest)
library(sandwich)
# Location
file_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file_path)
# Open data
df <- read.csv("processed_data/ProcessedData.csv")


##### Refactor in order ########
df$condition <- factor(df$condition, levels = c(
  "Control",
  "SB",
  "Table",
  "SliderLow",
  "SliderHigh"))
df$Income <- factor(df$Income, levels = c("Under 25k", "[25k to 45k)", "[45k to 65k)", "[65k to 95k)", "[95k to 135k)", "[135k to 250k)", "Over 250k"))
df$Education <- factor(df$Education, levels = c("LessThanHS", "HighSchool", "SomeCollege", "Bachelors", "Advanced"))
df$UsualPayment <- factor(df$UsualPayment, levels = c("LessThanMin", "OnlyMin", "BetweenMinAndBalance", "FullBalance"))
df$Age_range <- factor(df$Age_range, levels = c("18 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "80 to 100"))
##### Overall Sample ######
overall_balance <- list()
overall_balance[["Condition"]] <- table(df$condition)
overall_balance[["Sex"]] <- table(df$Sex)
overall_balance[["Age Range"]] <- table(df$Age_range)
overall_balance[["UsualPayment"]] <- table(df$UsualPayment)
overall_balance[["Education"]] <- table(df$Education)
overall_balance[["Income"]] <- table(df$Income)
overall_balance[["CorrectFL"]] <- table(df$CorrectFL)
overall_balance[["CorrectLateFees"]] <- table(df$CorrectLateFees)
overall_balance[["CorrectFLplusLateFees"]] <- table(df$CorrectFLplusLateFees)
##### Demographics balance per condition ######
create_pivot_tables <- function(df, columns, groupby = "condition", normalize = FALSE) {
  pivot_tables <- list()
  for (col in columns) {
    pivot_table <- df %>%
      group_by(!!sym(col), !!sym(groupby)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      spread(key = !!sym(groupby), value = count, fill = 0)
    
    if (normalize) {
      pivot_table <- pivot_table %>%
        mutate(across(colnames(pivot_table)[-1], ~ round(. / sum(.), 2)))
    }
    
    pivot_tables[[col]] <- pivot_table
  }
  return(pivot_tables)
}

columns <- c("Sex", "Age_range","UsualPayment", "Income", "Education", "CorrectFL","CorrectLateFees")
balance_demo_tables <- create_pivot_tables(df, columns)

##### Summary results by demographic #######
# List of columns to group by
columns <- c("Sex", "Age_range", "UsualPayment", "Income", "Education", "CorrectFL", "CorrectLateFees")

# Function to create summary table for each column
create_summary_table <- function(col) {
  df %>%
    group_by(!!sym(col)) %>%
    summarize(
      Mean_Percentage = round(mean(Payment_Percentage),2),
      Mean_Min = round(mean(Min),2),
      Mean_Partial = round(mean(Partial),2),
      Mean_Total = round(mean(Total),2)
    )
}

# Create a named list of summary tables
summary_result_demo <- setNames(
  lapply(columns, create_summary_table),
  paste(columns)
)

##### P-values pairwise comparison ########
# No normal distribution
# Yes independence
# Homogeneity in variance? -> Yes
## bartlett.test(Payment_Percentage ~ condition, data = df)

# Pairwise
pairwise_ttest <- list()
method <- "BH"
pairwise_ttest[["Percentage"]] <- pairwise.t.test(df$Payment_Percentage, df$condition, p.adjust.method = method)
pairwise_ttest[["Min"]] <- pairwise.t.test(df$Min, df$condition, p.adjust.method = method)
pairwise_ttest[["Partial"]] <- pairwise.t.test(df$Partial, df$condition, p.adjust.method = method)
pairwise_ttest[["Total"]] <- pairwise.t.test(df$Total, df$condition, p.adjust.method = method)

#pairwise_wilcox <- pairwise.wilcox.test(df$Payment_Percentage, df$condition, p.adjust.method = "BH")

##### Main Regressions ######
main_regression_table <- function(df, regularize, controls = FALSE, as_factor = FALSE, robust_errors = TRUE, extension = "html"){
  
  if(regularize==TRUE){
    df$Payment <- df$Payment/1678
  }
  
  if(controls==TRUE){
    
    df$Income <- factor(df$Income, 
                        levels = c("Under 25k", "[25k to 45k)", "[45k to 65k)", "[65k to 95k)", "[95k to 135k)", "[135k to 250k)", "Over 250k"),
                        ordered = TRUE)
    
    df$Education <- factor(df$Education, 
                          levels = c("LessThanHS", "HighSchool", "SomeCollege", 
                                     "Bachelors", "Advanced"), ordered = TRUE)
    
    if(as_factor==FALSE){
        df$Income <- as.numeric(df$Income)-1
        df$Education <- as.numeric(df$Education)-1
    }
    
    r_payment <- lm(Payment ~ condition + Age + Sex + Income + Education, data = df)
    r_lessmin <- lm(LessMin ~ condition + Age + Sex + Income + Education, data = df)
    r_min <- lm(Min ~ condition + Age + Sex + Income + Education, data = df)
    r_partial <- lm(Partial ~ condition + Age + Sex + Income + Education, data = df)
    r_total <- lm(Total ~ condition + Age + Sex + Income + Education, data = df)
    
  } 
  else{
    r_payment <- lm(Payment ~ condition, data = df)
    r_lessmin <- lm(LessMin ~ condition, data = df)
    r_min <- lm(Min ~ condition, data = df)
    r_partial <- lm(Partial ~ condition, data = df)
    r_total <- lm(Total ~ condition, data = df)
  }
  
  if(robust_errors==TRUE){
      # Function that Put robust errors on the ols regs
      get_robust_se <- function(lm_obj) {
        coeftest_obj <- coeftest(lm_obj, vcov = vcovHC(lm_obj, type="HC0"))
        se <- coeftest_obj[, "Std. Error"]
        pval <- coeftest_obj[, "Pr(>|t|)"]
        return(se)
      }
      t <- capture.output({
        stargazer(r_payment, r_min, r_partial, r_total,
                  type = extension, 
                  style = "default", 
                  column.labels = c("Payment", "Min", "Partial", "Total"),
                  ci = TRUE, ci.level = 0.95, # Include confidence intervals
                  p.auto = TRUE, # Include p-values
                  report = "vcsp*", # Include variance, coefficients, standard errors, t-stats, p-values
                  out = NULL,
                  se = list(
                    get_robust_se(r_payment),
                    get_robust_se(r_min),
                    get_robust_se(r_partial),
                    get_robust_se(r_total)))
      })
  }
  else{
      t <- capture.output({
        stargazer(r_payment, r_min, r_partial, r_total,
                  type = extension, 
                  style = "default", 
                  column.labels = c("Payment", "Min", "Partial", "Total"),
                  ci = TRUE, ci.level = 0.95, # Include confidence intervals
                  p.auto = TRUE, # Include p-values
                  report = "vcsp*", # Include variance, coefficients, standard errors, t-stats, p-values
                  out = NULL)
      })
  }
  
  return(t)
}

main_reg_table <- main_regression_table(df, TRUE)
main_reg_table_controls_numeric <- main_regression_table(df, TRUE, controls = TRUE, as_factor = FALSE)
main_reg_table_controls_factor <- main_regression_table(df, TRUE, controls = TRUE, as_factor = TRUE)
main_reg_table_over45 <- main_regression_table(df = df[df$Age>=45,], TRUE)

##### Regresions with single controls ######
get_regressions <- function(df, controls, regularize = TRUE, extension = "html") {
  
    # Set baselines
    df$UsualPayment <- relevel(df$UsualPayment, ref = "BetweenMinAndBalance")
    df$Education <- relevel(df$Education, ref = "SomeCollege")
    df$Income <- relevel(df$Income, ref = "[65k to 95k)")
    df$Age_range <- relevel(df$Age_range, ref = "30 to 40")
    
    if(regularize==TRUE){
      df$Payment <- df$Payment/1678
    }
    
    # Create the regression models
    r_nocontrols <- lm(Payment ~ condition, data = df)
    r_sex <- lm(Payment ~ condition + Sex, data = df)
    r_age <- lm(Payment ~ condition + Age_range, data = df)
    r_usual <- lm(Payment ~ condition + UsualPayment, data = df)
    r_education <- lm(Payment ~ condition + Education, data = df)
    r_income <- lm(Payment ~ condition + Income, data = df)
    
    # Output the summary using stargazer
    table_no_controls <- capture.output({
      stargazer(r_nocontrols,
              type = extension,
              dep.var.labels = "Payment",
              ci = TRUE, ci.level = 0.95, # Include confidence intervals
              p.auto = TRUE, # Include p-values
              report = "vcsp*") # Include variance, coefficients, standard errors, t-stats, p-values
    })
    
    # Generate HTML for the consolidated regression table using stargazer and suppress the console output
    table_single_controls <- capture.output({
      stargazer(r_nocontrols, r_sex, r_usual, r_education, r_income, r_age,
                type = extension, 
                style = "default", 
                title = "Single Controls",
                column.labels = c("No Controls", "Sex", "UsualPayment", "Education", "Income", "AgeRange"),
                dep.var.labels = "Payment",
                out = NULL)
    })
    
    if(controls == TRUE){
        return(table_single_controls)
    }
    else{
    return(table_no_controls)
    }
}

# HTML
table_no_controls <- get_regressions(df = df, controls = FALSE)
table_single_controls <- get_regressions(df = df, controls = TRUE)
# over 45
table_oldpeople_no_controls <- get_regressions(df = df[df$Age>=45,], controls = FALSE)
table_oldpeople_single_controls <- get_regressions(df = df[df$Age>=45,], controls = TRUE)

##### Financial literacy heterogenety ########
# Install and load necessary libraries
install.packages("corrplot")
library(corrplot)

# Create a subset of the data frame with the binary columns
df_binary <- df[, c("CorrectFL1", "CorrectFL2", "CorrectFL3", "CorrectLateFees1", "CorrectLateFees2")]

# Calculate the correlation matrix using Pearson correlation
corr_matrix <- cor(df_binary, method = "pearson")

# Plot the correlation matrix as a heatmap
corrplot(corr_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.8, cl.pos = "n", number.digits = 2)





table(df$CorrectFL)
low_fl <- main_regression_table(df[df$CorrectFL<3,],regularize = TRUE)
high_fl <- main_regression_table(df[df$CorrectFL==3,],regularize = TRUE)
low_lf <- main_regression_table(df[df$CorrectLateFees<2,],regularize = TRUE)
high_lf <- main_regression_table(df[df$CorrectLateFees==2,],regularize = TRUE)
##### Use of UI #######
# Use
df_gadgets <- df[df$condition %in% c("SliderLow", "SliderHigh", "Table"), ]
table_usegadget <- create_pivot_tables(df_gadgets, c("UseGadget"), normalize = TRUE)

# Payment x Use
table_paymentxgadget <- df_gadgets %>%
      group_by(condition, UseGadget) %>%
      summarise(mean_payment = round(mean(Payment),2),
            sd_payment = round(sd(Payment),2),
            n = n())

table_payment <- df %>%
  group_by(condition) %>%
  summarise(mean_payment = round(mean(Payment),2),
            sd_payment = round(sd(Payment),2),
            n = n())

# Viewed values
df_sliders <- df[df$condition %in% c("SliderLow", "SliderHigh"), ]
table_exploredvalues <- create_pivot_tables(df_sliders, c("sliderExploredValues"), normalize = TRUE)

##### Results by explored values#######

# Slider low: explored value results
subdf <- df[df$condition=="SliderLow",]
subdf <- subdf %>%
  mutate(
    sliderExploredValuesRange = case_when(
      sliderExploredValues == 1 ~ "only $58 (no move)",
      sliderExploredValues <= 7 ~ "from $100 to $600",
      sliderExploredValues <= 17 ~ "from $700 to $1600 (flat zone)",
      sliderExploredValues == 18 ~ "all ticks",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("only $58 (no move)", "from $100 to $600", "from $700 to $1600 (flat zone)", "all ticks"))
  )
sliderlow_results_by_exploredvalues <- subdf %>%
  group_by(sliderExploredValuesRange) %>%
  summarize(
    Mean_Payment = round(mean(Payment_Percentage),2),
    Mean_Min = round(mean(Min),2),
    Mean_Partial = round(mean(Partial),2),
    Mean_Total = round(mean(Total),2),
    n = n()
  )

# Slider high: explored value results
subdf <- df[df$condition=="SliderHigh",]
subdf <- subdf %>%
  mutate(
    sliderExploredValuesRange = case_when(
      sliderExploredValues == 1 ~ "only $1678 (no move)",
      sliderExploredValues <= 11 ~ "from $1600 to $700 (flat zone)",
      sliderExploredValues <= 17 ~ "from $600 to $100",
      sliderExploredValues == 18 ~ "all ticks",
      TRUE ~ NA_character_
    ) %>%
    factor(levels = c("only $1678 (no move)", "from $1600 to $700 (flat zone)", "from $600 to $100", "all ticks"))
  )

sliderhigh_results_by_exploredvalues <- subdf %>%
  group_by(sliderExploredValuesRange) %>%
  summarize(
    Mean_Payment = round(mean(Payment_Percentage),2),
    Mean_Min = round(mean(Min),2),
    Mean_Partial = round(mean(Partial),2),
    Mean_Total = round(mean(Total),2),
    n = n()
  )


##### Slider values graph ########
# Assuming df is your data frame with the extracted data
slider_info <- data.frame(
  Payment = c(58, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 
              1100, 1200, 1300, 1400, 1500, 1600, 1678),
  Time_to_Pay = c(3*12, 1*12 + 7, 8, 5, 4, 3, 2, 2, 2, 
                  1, 1, 1, 1, 1, 1, 1, 1, 0),
  Total_Payment = c(2133, 1911, 1789, 1751, 1733, 1721, 1713, 1707, 1702, 
                    1698, 1695, 1692, 1689, 1686, 1684, 1682, 1680, 1678),
  Savings = c(1896, 2118, 2240, 2278, 2296, 2308, 2316, 2322, 2327, 2331, 
              2334, 2337, 2340, 2343, 2345, 2347, 2349, 2351)
)

# Plot 1: Total Payment and Savings
plot1 <- ggplot(slider_info, aes(x = Payment)) +
  geom_line(aes(y = Total_Payment, color = "Total Payment"), size = 1) +
  geom_point(aes(y = Total_Payment, color = "Total Payment"), shape = 4) +
  geom_line(aes(y = Savings, color = "Savings"), size = 1) +
  geom_point(aes(y = Savings, color = "Savings"), shape = 4) +
  labs(title = "Total Payment and Savings", y = "$USD") +
  scale_y_continuous(limits = c(1600, 2400)) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Time to Pay
plot2 <- ggplot(slider_info, aes(x = Payment)) +
  geom_line(aes(y = Time_to_Pay, color = "Time to Pay"), size = 1) +
  geom_point(aes(y = Time_to_Pay, color = "Time to Pay"), shape = 4) +
  labs(title = "Time to Pay", y = "months") +
  scale_y_continuous(limits = c(0, 40)) +
  theme_minimal() +
  theme(legend.position = "none")

# Create a combined legend
legend <- ggplot(slider_info, aes(x = Payment)) +
  geom_line(aes(y = Total_Payment, color = "Total Payment"), size = 1) +
  geom_point(aes(y = Total_Payment, color = "Total Payment"), shape = 4) +
  geom_line(aes(y = Savings, color = "Savings"), size = 1) +
  geom_point(aes(y = Savings, color = "Savings"), shape = 4) +
  geom_line(aes(y = Time_to_Pay, color = "Time to Pay"), size = 1) +
  geom_point(aes(y = Time_to_Pay, color = "Time to Pay"), shape = 4) +
  labs(color = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

# Extract the legend
legend_grob <- ggplotGrob(legend)$grobs[[which(sapply(ggplotGrob(legend)$grobs, function(x) x$name) == "guide-box")]]

# Arrange the plots and legend
p <- grid.arrange(plot1, plot2, legend_grob, ncol = 2, nrow = 2, heights = c(10, 1))
# Save the plot as a PNG file
ggsave("imgs/slider.png", plot = p, width = 8, height = 5, units = "in")

####### HTML Report ######
# Create HTML content
html_content <- tagList(
  tags$html(
    tags$head(
      tags$title("Report")
    ),
    tags$h1("Overall Balance"),
    tags$body(
      lapply(names(overall_balance), function(table_name) {
        table <- overall_balance[[table_name]]
        tagList(
          tags$h3(table_name),
          HTML(kable(table, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive")))
        )
      }),
      
      tags$h1("Balance Demographics x Condition"),
      lapply(names(balance_demo_tables), function(table_name) {
        table <- balance_demo_tables[[table_name]]
        tagList(
          tags$h3(table_name),
          HTML(kable(table, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive")))
        )
      }),
      
      tags$h1("Results grouped by demographic"),
      lapply(names(summary_result_demo), function(table_name) {
        table <- summary_result_demo[[table_name]]
        tagList(
          tags$h3(table_name),
          HTML(kable(table, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive")))
        )
      }),
      
      tags$h1("Use of gadgets (table/slider)"),
      tags$h3("Use"),
      HTML(kable(table_usegadget, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      tags$h3("Payment x Use"),
      HTML(kable(table_paymentxgadget, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      tags$h3("Mean payment by condition"),
      HTML(kable(table_payment, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      tags$h3("Slider: Explored values "),
      tags$body("How many of the 18 ticks the person viewed (58, 100, ..., 1600, 1678)"),
      HTML(kable(table_exploredvalues, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      
      tags$h1("Slider/Table data"),
      tags$body("This are the values that the slider and table show."),
      HTML(kable(slider_info, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      # Add saved ggplot plot here
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/slider.png", width = "60%")
      ),
      
      tags$h1("Results by slider explored values"),
      tags$h3("SliderLow"),
      HTML(kable(sliderlow_results_by_exploredvalues, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      tags$h3("SliderHigh"),
      HTML(kable(sliderhigh_results_by_exploredvalues, format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
      
      tags$h1("Pairwise p-values (Benjamini & Hochberg (1995)"),
      tags$h3("t-test"),
      lapply(names(pairwise_ttest), function(table_name) {
        table <- pairwise_ttest[[table_name]]
        tagList(
          tags$h3(table_name),
          HTML(kable(
            format(round(as.data.frame(table$p.value),3), scientific = FALSE),
            format = 'html') %>% kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "responsive"))),
        )
      }),
      
      tags$h1("Regression (robust errors White)"),
      
      tags$h3("No Controls"),
      HTML(main_reg_table),
      
      tags$h3("Controls, Numeric"),
      HTML(main_reg_table_controls_numeric),
      
      tags$h3("Controls, Factor"),
      HTML(main_reg_table_controls_factor),
      
      tags$h3("No Controls, Over 45"),
      HTML(main_reg_table_over45),
      
      tags$h1("Het Financial Literacy and Late Fees"),
      tags$h3("low financial literacy"),
      HTML(low_fl),
      tags$h3("high financial literacy"),
      HTML(high_fl),
      tags$h3("low latefees"),
      HTML(low_lf),
      tags$h3("high latefees"),
      HTML(high_lf),
      
#      tags$h2("Single Controls"),
#      tags$h3("Base Levels"),
#      tags$ul(
#        tags$li("UsualPayment: BetweenMinAndBalance"),
#        tags$li("Education: SomeCollege"),
#        tags$li("Income: [65k to 95k)"),
#      ),
#      HTML(table_single_controls)
    )
  )
)

# Write the HTML content to a file
save_html(html_content, file = "report.html")




