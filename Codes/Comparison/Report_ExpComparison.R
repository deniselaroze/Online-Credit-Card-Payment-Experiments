###### Setup & data ######
# Libraries
rm(list=ls())
library(dplyr)
library(htmltools)
library(tidyverse)
library(stargazer)
library(kableExtra)
library(knitr)
library(viridis)
library(MatchIt)
library(patchwork)
library(htmlTable)
library(ggplot2)
library(scales)
library(readxl)
library(tibble)
library(gridExtra)
library(lmtest)
library(sandwich)
library(htmltools)
library(htmlTable)
# Location
git_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(git_path)
# Open data
dfn <- read.csv("Exp_Netquest/processed_data/ProcessedData.csv")
dfp <- read.csv("Exp_Prolific/processed_data/ProcessedData.csv")


###### Syntetic Netquest - Propensity Score #######
# dfn$Study = 1
# dfp$Study = 0
# 
# df <- rbind(dfn[,columns],dfp[,columns])
# 
# columns <- c("Study", "condition", 
#                       "Payment", "Payment_Range", "Payment_Percentage",
#                       "LessMin","Min","Partial", "Total", 
#                       "Age", "Sex", "Age_range",
#                       "Education","Income", "CorrectFL")
# 
# 
# 
#  # Specify the variables you want to match on
# formula <- Study ~ condition + Age_range + Sex + CorrectFL
# 
# # Perform the matching
# matched <- matchit(formula, data = df, method = "nearest", ratio = 1)
# # Extract the matched sample
# matched_df <- match.data(matched)
# # Separate the matched sample from the large dataframe
# syntetic_dfn <- matched_df[matched_df$Study == 0, ]
# 
# # Check duplicates
# columns_to_exclude <- c("distance", "weights", "subclass")
# dup_rows <- duplicated(syntetic_dfn[, !(names(syntetic_dfn) %in% columns_to_exclude)])
# sum(dup_rows)
# rm(matched_df, matched)

###### Syntetic Netquest - Exact match #######
print("Creating syntetic netquest dataframe")
selected_columns <- c("condition", "Sex", "Age_range", "CorrectFL")
             #"Education","Income", "CorrectFL")


# Randomize rows of dfn and dfp
dfn <- dfn[sample(nrow(dfn)), ]
dfp <- dfp[sample(nrow(dfp)), ]

# Initialize used_rows and synthetic_df
used_rows <- c()
synthetic_dfn <- data.frame()

# Loop to match rows and create synthetic_df
for (i in 1:nrow(dfn)) {
  if(i%%30==0) print(round(i/nrow(dfn),2))
  for (j in 1:nrow(dfp)) {
    if (j %in% used_rows) next
    
    # Check if all selected columns are equal
    if (all(dfn[i, selected_columns] == dfp[j, selected_columns])) {
      synthetic_dfn <- rbind(synthetic_dfn, dfp[j, ])
      used_rows <- c(used_rows, j)
      break
    }
  }
}

###### Merge studies #########
dfp$Study <- "Prolific"
dfn$Study <- "Netquest"
synthetic_dfn$Study <- "SynteticNetquest"

columns <- c("Study", "condition", 
             "Payment", "Payment_Range", "Payment_Percentage",
             "LessMin","Min","Partial", "Total", 
             "Age", "Sex", "Age_range",
             "Education", "Income", "CorrectFL")
df <- rbind(dfn[,columns],dfp[,columns],synthetic_dfn[,columns])
##### Age histogram ########
# Calculate means
means <- df %>%
  group_by(Study) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE))

# Create histogram with mean lines
plot_age <- ggplot(df, aes(x = Age, fill = Study)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  labs(title = "Histogram of Age by Study",
       x = "Age",
       y = "Count") +
  theme_minimal() +
  xlim(18, 95) +
  geom_vline(data = means, aes(xintercept = mean_age, color = Study),
             linetype = "dotted", size = 1)

##### Other variables barplot ########

# Function to create proportional bar plot
create_proportional_barplot <- function(data, variable, proportion=TRUE) {
  # Ensure variable is a factor with proper levels
  data[[variable]] <- factor(data[[variable]])
  
  # Calculate counts and proportions
  counts <- data %>%
    group_by(Study, !!sym(variable)) %>%
    summarise(count = n()) %>%
    mutate(prop = count / sum(count))
  
  if (proportion) {
    # Create the plot
    ggplot(counts, aes(x = Study, y = prop, fill = !!sym(variable))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(title = paste("Proportional Bar Plot of", variable, "by Study"),
           x = "Study",
           y = "Proportion",
           fill = variable) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired")
  }
  else{
    ggplot(counts, aes(x = Study, y = count, fill = !!sym(variable))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(title = paste("Number of", variable, "by Study"),
           x = "Study",
           y = "Proportion",
           fill = variable) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired")
    
  }
}

# condition
plot_condition <- create_proportional_barplot(df, "condition", proportion = FALSE)

# Age_range
plot_agerange <- create_proportional_barplot(df, "Age_range")

# Sex
plot_sex <- create_proportional_barplot(df, "Sex")

# Education
df$Education <- factor(df$Education, levels = c("LessThanHS", "HighSchool", "SomeCollege", "Bachelors", "Advanced"))
plot_edu <-create_proportional_barplot(df, "Education")

# Income
df$Income <- factor(df$Income, levels = c("Under 25k", "[25k to 45k)", "[45k to 65k)", "[65k to 95k)", "[65k to 100k)", "[95k to 135k)", "[100k to 150k)",  "[135k to 250k)", "[150k to 250k)", "Over 250k"))
plot_income <- create_proportional_barplot(df, "Income")

# FL
df$CorrectFL <- as.factor(df$CorrectFL)
plot_FL <- create_proportional_barplot(df, "CorrectFL")


# Save the plot as a PNG file
ggsave("Comparison/imgs/plot_condition.png", plot =plot_condition , width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_age.png", plot = plot_age , width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_agerange.png", plot = plot_agerange , width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_sex.png", plot = plot_sex , width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_FL.png", plot = plot_FL, width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_edu.png", plot = plot_edu , width = 8, height = 5, units = "in")
ggsave("Comparison/imgs/plot_income.png", plot = plot_income, width = 8, height = 5, units = "in")

##### Main regressions ######

main_regression_table <- function(df, controls = FALSE, as_factor = FALSE, robust_errors = TRUE, interaction = FALSE, extension = "html"){
  
  df$Payment <- df$Payment/1678
  
  if(controls==TRUE){
    df$Age_range <- relevel(as.factor(df$Age_range), ref = "40 to 50")
    df$Sex <- relevel(as.factor(df$Sex), ref = "Male")
    df$CorrectFL <- relevel(as.factor(df$CorrectFL), ref = "0")
    
    if(as_factor==FALSE){
      df$Income <- as.numeric(df$Income)-1
      df$Education <- as.numeric(df$Education)-1
    }
    
    if(interaction==FALSE){
        r_payment <- lm(Payment ~ condition + Age_range + Sex + CorrectFL, data = df)
        r_lessmin <- lm(LessMin ~ condition + Age_range + Sex + CorrectFL, data = df)
        r_min <- lm(Min ~ condition + Age_range + Sex + CorrectFL, data = df)
        r_partial <- lm(Partial ~ condition + Age_range + Sex + CorrectFL, data = df)
        r_total <- lm(Total ~ condition + Age_range + Sex + CorrectFL, data = df)
    } else{
        r_payment <- lm(Payment ~ condition + Age_range + Sex + CorrectFL + condition:Age_range + condition:Sex + condition:CorrectFL, data = df)
        r_lessmin <- lm(LessMin ~ condition + Age_range + Sex + CorrectFL + condition:Age_range + condition:Sex + condition:CorrectFL, data = df)
        r_min <- lm(Min ~ condition + Age_range + Sex + CorrectFL + condition:Age_range + condition:Sex + condition:CorrectFL, data = df)
        r_partial <- lm(Partial ~ condition + Age_range + Sex + CorrectFL + condition:Age_range + condition:Sex + condition:CorrectFL, data = df)
        r_total <- lm(Total ~ condition + Age_range + Sex + CorrectFL + condition:Age_range + condition:Sex + condition:CorrectFL, data = df)
      
    }
    
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

nocontrols_p <- main_regression_table(df[df$Study=="Prolific",], controls = FALSE, as_factor = TRUE, robust_errors = TRUE)
nocontrols_n <- main_regression_table(df[df$Study=="Netquest",], controls = FALSE, as_factor = TRUE, robust_errors = TRUE)
nocontrols_sn <- main_regression_table(df[df$Study=="SynteticNetquest",], controls = FALSE, as_factor = TRUE, robust_errors = TRUE)

controls_p <- main_regression_table(df[df$Study=="Prolific",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = FALSE)
controls_n <- main_regression_table(df[df$Study=="Netquest",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = FALSE)
controls_sn <- main_regression_table(df[df$Study=="SynteticNetquest",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = FALSE)

interactions_p <- main_regression_table(df[df$Study=="Prolific",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = TRUE)
interactions_n <- main_regression_table(df[df$Study=="Netquest",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = TRUE)
interactions_sn <- main_regression_table(df[df$Study=="SynteticNetquest",], controls = TRUE, as_factor = TRUE, robust_errors = TRUE, interaction = TRUE)
####### HTML Report ######
# Create HTML content
html_content <- tagList(
  tags$html(
    tags$head(
      tags$title("Experiment Comparisons")
    ),
    tags$body(
      
      tags$h3("Main differences in the experiments:"),
      tags$ol(
        tags$li("Slider: continous in netquest, discrete in prolific (18 ticks)"),
        tags$li("Min value in slider: $38 in netquest, $58 in prolific (in both SliderLow started in $58)"),
        tags$li("Age: older in netquest"),
        tags$li("Sex: 75% women in netquest, 50% in prolific"),
        tags$li("Financial literacy: lower in netquets"),
        tags$li("Education: lower in netquets"),
      ),
      
      tags$h3("Synthetic Netquest"),
      tags$p("A subsample from the Prolific dataset where participants match the same demographic distributions as those in the Netquest experiment."),
      tags$strong("Would Synthetic Netquest data produce the same results as Netquest?"),
      tags$p("The synthetic dataframe is created by sampling observations from the Prolific experiment without replacement. The process is as follows:"),
      tags$ol(
        tags$li("Select a row from the Netquest dataframe."),
        tags$li("Find a matching row in the Prolific dataframe that has identical values in the specified columns (*selected_columns*) to the selected Netquest row."),
        tags$li("Add the matching Prolific row to the synthetic dataframe. The selected Prolific row cannot be used again in the sampling process.")
      ),
      tags$p(paste0("Selected columns: ", paste(selected_columns, collapse = ", "))),
      
      tags$h1("Demographics by dataframe"),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_condition.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_age.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_agerange.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_sex.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_FL.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_edu.png", width = "60%")
      ),
      tags$div(
        style = "display: flex; justify-content: center;",
        tags$img(src = "imgs/plot_income.png", width = "60%")
      ),
      
      tags$h1("Regression No Controls (robust errors white)"),
      tags$h3("Netquest"),
      HTML(nocontrols_n),
      tags$h3("Prolific"),
      HTML(nocontrols_p),
      tags$h3("Synthetic Netquest"),
      HTML(nocontrols_sn),
      
      tags$h1("Regression controls"),
      tags$strong("Baseline: Male, [40 to 50), FL=0"),
      tags$h3("Netquest"),
      HTML(controls_n),
      tags$h3("Prolific"),
      HTML(controls_p),
      tags$h3("Synthetic Netquest"),
      HTML(controls_sn),
      
      tags$h1("Regression controls + interactions"),
      tags$strong("Baseline: Male, [40 to 50), FL=0"),
      tags$h3("Netquest"),
      HTML(interactions_n),
      tags$h3("Prolific"),
      HTML(interactions_p),
      tags$h3("Synthetic Netquest"),
      HTML(interactions_sn),
    )
  )
)



# Write the HTML content to a file
save_html(html_content, file = "Comparison/reportComparison.html")