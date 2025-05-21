library(data.table)
library(knitr)
library(kableExtra)
library(tidyverse)
library(broom)
library(stargazer)
library(gridExtra)
library(stringr)
library(sandwich)
library(lmtest)
library(scales) 

setwd("/Users/elisaduranmicco/Dropbox/2103 - ANID - FONDEF Sernac/11.Pago/replication")

# open data
data <- read.csv("data/FinalData.csv") #2503
data <- as.data.table(data)

# Balance Table

Deno <-  data %>% 
  group_by(condition) %>%
  summarise(counts = n())

age <- data %>%
  group_by(condition) %>%
  summarise_at(vars(Age), list(mean,sd))

anova_age <- aov(Age ~ condition, data)
summary(anova_age)

data$Sex[data$Sex == "Female, Female"] <- "Female"
sex0 <- table(data$Sex, data$condition)
sex <-cbind(100*sex0[,1]/503, 100*sex0[,2]/501, 100*sex0[,3]/495, 100*sex0[,4]/501 ,100*sex0[,5]/503)

data$female <-  ifelse(data$Sex == "Female", 1, 0)
chisq.test(table(data$condition, data$female))

kable(sex,
      col.names = c("Sex", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      # "latex",
      digits = c(2, 2, 2, 2, 2))

data$Demo_Income <- factor(data$Demo_Income, levels = c("$24,999 or under", "$25,000 to $44,999", "$45,000 to $64,999", "$65,000 to $94,999", "$95,000 to $134,999", "$135,000 to $249,999", "$250,000 or over"))
inc0 <- table(data$Demo_Income, data$condition)
inc <- cbind(100*inc0[,1]/503, 100*inc0[,2]/501, 100*inc0[,3]/495, 100*inc0[,4]/501 ,100*inc0[,5]/503)

kable(inc,
      col.names = c("Income", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      #     "latex",
      digits = c(2, 2, 2, 2, 2))

data$inc1 <-  ifelse(data$Demo_Income == "$24,999 or under", 1, 0)
chisq.test(table(data$condition, data$inc1))

data$inc2 <-  ifelse(data$Demo_Income == "$25,000 to $44,999", 1, 0)
chisq.test(table(data$condition, data$inc2))

data$inc3 <-  ifelse(data$Demo_Income == "$45,000 to $64,999", 1, 0)
chisq.test(table(data$condition, data$inc3))

data$inc4 <-  ifelse(data$Demo_Income == "$65,000 to $94,999", 1, 0)
chisq.test(table(data$condition, data$inc4))

data$inc5 <-  ifelse(data$Demo_Income == "$95,000 to $134,999", 1, 0)
chisq.test(table(data$condition, data$inc5))

data$inc6 <-  ifelse(data$Demo_Income == "$135,000 to $249,999", 1, 0)
chisq.test(table(data$condition, data$inc6))

data$inc7 <-  ifelse(data$Demo_Income == "$250,000 or over", 1, 0)
chisq.test(table(data$condition, data$inc7))

data$Demo_Education <- factor(data$Demo_Education, levels = c("Less than a high school diploma or equivalent", "High school graduate", "Some college but no degree", "Bachelor's degree", "Advanced degree (Master's degree, professional degree, or doctoral degree)"))
educ0 <- table(data$Demo_Education , data$condition)
educ <- cbind(100*educ0[,1]/503, 100*educ0[,2]/501, 100*educ0[,3]/495, 100*educ0[,4]/501 ,100*educ0[,5]/503)

kable(educ,
      col.names = c("Education", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      #      "latex",
      digits = c(2, 2, 2, 2, 2,2))

data$edu1 <-  ifelse(data$Demo_Education == "Less than a high school diploma or equivalent", 1, 0)
chisq.test(table(data$condition, data$edu1))
data$edu2 <-  ifelse(data$Demo_Education == "High school graduate", 1, 0)
chisq.test(table(data$condition, data$edu2))
data$edu3 <-  ifelse(data$Demo_Education == "Some college but no degree", 1, 0)
chisq.test(table(data$condition, data$edu3))
data$edu4 <-  ifelse(data$Demo_Education == "Bachelor's degree", 1, 0)
chisq.test(table(data$condition, data$edu4))
data$edu5 <-  ifelse(data$Demo_Education == "Advanced degree (Master's degree, professional degree, or doctoral degree)", 1, 0)
chisq.test(table(data$condition, data$edu5))

fl <- subset(data, is.na(data$FL)==FALSE) %>%
  group_by(condition) %>%
  summarise_at(vars(FL), list(mean,sd))

anova_fl <- aov(FL ~ condition, data)
summary(anova_fl)

# Figure: Distribution of repayment amounts

plot_data <- data %>%
  group_by(y0, condition) %>%
  summarise(count = n()) %>%
  group_by(condition) %>%
  mutate(percentage = count / sum(count))

plot_data1 <- subset(plot_data, plot_data$condition %in% c("SliderHigh", "SliderLow", "Table"))
plot_data1 <- subset(plot_data1, plot_data1$y0 < 1678)

plot_data1$y0[plot_data1$y0 == 102.01] <- 102

plot_data2 <- subset(plot_data, plot_data$condition %in% c("SliderHigh", "SliderLow", "Table"))
plot_data2 <- subset(plot_data2, plot_data2$y0 == 1678)

# Modify condition labels
plot_data1$condition <- factor(plot_data1$condition,
                               levels = c("SliderHigh", "SliderLow", "Table"),
                               labels = c("Slider-High", "Slider-Low", "Reference Table"))

plot_data2$condition <- factor(plot_data2$condition,
                               levels = c("SliderHigh", "SliderLow", "Table"),
                               labels = c("Slider-High", "Slider-Low", "Reference Table"))

# Create plots with modified legend (no title)
p1 <- ggplot(plot_data1, aes(x = as.factor(y0), y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Payment Amount",
    y = "Percentage (%)",
    fill = NULL  # This completely removes the legend title space
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank()  # Ensures no space is reserved for title
  )

p2 <- ggplot(plot_data2, aes(x = as.factor(y0), y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Payment Amount",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

grid.arrange(
  p1, p2,
  ncol = 2,
  widths = c(3, 1)
)

#Table: Linear Regression
data$Min <- ifelse(data$y0==34, 1, 0)
data$Partial <- ifelse(data$y0>34 & data$y0<1678 , 1, 0)
data$Total <- ifelse(data$y0==1678 , 1, 0)

r_payment <- lm(share ~ condition, data = data)
r_min <- lm(Min ~ condition, data = data)
r_partial <- lm(Partial ~ condition, data = data)
r_total <- lm(Total ~ condition, data = data)

coeftest(r_payment, vcov = vcovHC(r_payment, type = "HC1"))
coeftest(r_min, vcov = vcovHC(r_min, type = "HC1"))
coeftest(r_partial, vcov = vcovHC(r_partial, type = "HC1"))
coeftest(r_total, vcov = vcovHC(r_total, type = "HC1"))

stargazer(r_payment, r_min, r_partial, r_total, 
          type = "text")

# Table: Logit 
get_robust_se_logit <- function(glm_obj) {
  coeftest_obj <- coeftest(glm_obj, vcov = vcovHC(glm_obj, type = "HC0"))
  se <- coeftest_obj[, "Std. Error"]
  return(se)
}

# First Decision
logit01 <- glm(Min ~ condition, data = data, family = binomial(link = "logit"))
logit02 <- glm(Partial ~ condition, data = data, family = binomial(link = "logit"))
logit03 <- glm(Total ~ condition, data = data, family = binomial(link = "logit"))

stargazer(logit01, logit02, logit03,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se_logit(logit01),
            get_robust_se_logit(logit02),
            get_robust_se_logit(logit03)
          ))

# Figure: final payment and final refernce for sliders
dataSliderH <- subset(data, data$condition == "SliderHigh")
dataSliderH$slider <- ifelse(dataSliderH$sliderCounts == "{\"58\":0,\"100\":0,\"200\":0,\"300\":0,\"400\":0,\"500\":0,\"600\":0,\"700\":0,\"800\":0,\"900\":0,\"1000\":0,\"1100\":0,\"1200\":0,\"1300\":0,\"1400\":0,\"1500\":0,\"1600\":0,\"1678\":1}" , "No", "Yes")

ggplot(dataSliderH, aes(y=y0, x=as.numeric(sliderValue), color=slider)) +
  geom_point(alpha = 1/3)+
  xlab("Slider Value")+
  ylab("Payment Amount")+
  theme_minimal()+
  guides(color = guide_legend(title = "Use slider:")) 

dataSliderL <- subset(data, data$condition == "SliderLow")
dataSliderL$slider <- ifelse(dataSliderL$sliderCounts == "{\"58\":1,\"100\":0,\"200\":0,\"300\":0,\"400\":0,\"500\":0,\"600\":0,\"700\":0,\"800\":0,\"900\":0,\"1000\":0,\"1100\":0,\"1200\":0,\"1300\":0,\"1400\":0,\"1500\":0,\"1600\":0,\"1678\":0}" , "No", "Yes")

ggplot(dataSliderL, aes(y=y0, x=as.numeric(sliderValue), color=slider)) +
  geom_point(alpha = 1/3)+
  xlab("Slider Value")+
  ylab("Payment Amount")+
  theme_minimal()+
  guides(color = guide_legend(title = "Use slider:")) 

# Table: Heterogeneity
# Financial literacy
data_LC <- subset(data, is.na(data$FL)==FALSE) 
model1_LC <- lm(Total ~ condition * FL, data = data_LC)
coeftest(model1_LC, vcov = vcovHC(model1_LC, type = "HC1"))

# Late Fees
data_LF <- subset(data, is.na(data$LFees)==FALSE) 
model1_LF <- lm(Total ~ condition * LFees, data = data_LF)
coeftest(model1_LF, vcov = vcovHC(model1_LF, type = "HC1"))

#Debt Aversion
data_DA <- subset(data, !(data$DA == "NA"))
model1_DA <- lm(Total ~ condition * DA , data = data_DA)
coeftest(model1_DA, vcov = vcovHC(model1_DA, type = "HC1"))

# male 
data$male <- ifelse(data$Sex == "Male", 1, 0)
model1_male <- lm(Total ~ condition * male, data = data)
coeftest(model1_male, vcov = vcovHC(model1_male, type = "HC1"))

# income
model1_abc1 <- lm(Total ~ condition * ses, data = data)
coeftest(model1_abc1, vcov = vcovHC(model1_abc1, type = "HC1"))

# Explicitly specify dplyr's select if there are conflicts
select <- dplyr::select

# Function to extract model results with robust SEs
extract_model <- function(model) {
  model_summary <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  # Convert to tidy data frame
  result <- data.frame(
    term = rownames(model_summary),
    estimate = model_summary[,1],
    std.error = model_summary[,2],
    p.value = model_summary[,4],
    stringsAsFactors = FALSE
  ) %>% 
    mutate(
      term = gsub("conditionSB", "Statement Balance", term),
      term = gsub("conditionSliderHigh", "Slider-High", term),
      term = gsub("conditionSliderLow", "Slider-Low", term),
      term = gsub("conditionTable", "Reference Table", term),
      term = gsub("FL", "X", term),
      term = gsub("LFees", "X", term),
      term = gsub("DA", "X", term),
      term = gsub("male", "X", term),
      term = gsub("ses", "X", term),
      term = gsub(":", " × ", term),
      formatted = sprintf("%.3f%s", estimate, 
                          ifelse(p.value < 0.01, "***",
                                 ifelse(p.value < 0.05, "**",
                                        ifelse(p.value < 0.1, "*", "")))),
      std.error = sprintf("(%.3f)", std.error)
    )
  
  return(result)
}

# Extract all model results
results <- list(
  "Financial Literacy (1)" = extract_model(model1_LC),
  "Late Fees (2)" = extract_model(model1_LF),
  "Debt Aversion (3)" = extract_model(model1_DA),
  "Gender (Male) (4)" = extract_model(model1_male),
  "SES (5)" = extract_model(model1_abc1)
)

# Create a unified table
create_table <- function(results) {
  # Define the terms we want to show (modify as needed)
  terms_to_keep <- c(
    "Statement Balance", "Slider-High", "Slider-Low", "Reference Table", 
    "X", "Statement Balance × X", "Slider-High × X", "Slider-Low × X", "Reference Table × X"
  )
  
  # Initialize empty data frame
  final_table <- data.frame(Term = terms_to_keep, stringsAsFactors = FALSE)
  
  # Add each model's results
  for (model_name in names(results)) {
    model_df <- results[[model_name]] %>% 
      filter(term %in% terms_to_keep) %>% 
      select(term, formatted, std.error)  
    
    # Merge with final table
    final_table <- final_table %>% 
      left_join(model_df, by = c("Term" = "term"))
    
    # Rename columns to match model name
    colnames(final_table)[(ncol(final_table)-1):ncol(final_table)] <- 
      paste0(model_name, c("_coef", "_se"))
  }
  
  return(final_table)
}

# Generate the table
regression_table <- create_table(results)

# Replace NA with empty strings
regression_table[is.na(regression_table)] <- ""

coef_table <- select(regression_table, "Term", "Financial Literacy (1)_coef", "Late Fees (2)_coef", "Debt Aversion (3)_coef", "Gender (Male) (4)_coef", "SES (5)_coef")
kable(coef_table)

se_table <- select(regression_table, "Term", "Financial Literacy (1)_se", "Late Fees (2)_se", "Debt Aversion (3)_se", "Gender (Male) (4)_se", "SES (5)_se")
kable(se_table)

# Table: Repayment behavior 
data %>%
  group_by(condition) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(condition) %>%
  mutate(percentage = 100*y0/1678)

data %>%
  group_by(condition, pay) %>%
  summarise(count = n()) %>%
  group_by(condition) %>%
  mutate(percentage = 100*count / sum(count))

dataSliderH  %>%
  group_by(slider) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(slider) %>%
  mutate(percentage = 100*y0/1678)

dataSliderH %>%
  group_by(slider, pay) %>%
  summarise(count = n()) %>%
  group_by(slider) %>%
  mutate(percentage = 100*count / sum(count))

dataSliderL  %>%
  group_by(slider) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(slider) %>%
  mutate(percentage = 100*y0/1678)

dataSliderL %>%
  group_by(slider, pay) %>%
  summarise(count = n()) %>%
  group_by(slider) %>%
  mutate(percentage = 100*count / sum(count))

dataTable <- subset(data, data$condition == "Table")

dataTable  %>%
  group_by(tableClickButton) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(tableClickButton) %>%
  mutate(percentage = 100*y0/1678)

dataTable %>%
  group_by(tableClickButton, pay) %>%
  summarise(count = n()) %>%
  group_by(tableClickButton) %>%
  mutate(percentage = 100*count / sum(count))

