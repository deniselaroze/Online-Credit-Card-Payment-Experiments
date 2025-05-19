###### Setup ######
# Libraries
rm(list=ls())
library(gridExtra)
library(dplyr)
library(xtable)
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

# Demo_Education
df <- df %>%
  mutate(Demo_Education = case_when(
    Demo_Education == "Advanced degree (Master's degree, professional degree, or doctoral degree)" ~ "Advanced degree",
    Demo_Education == "Bachelor's degree" ~ "Bachelor's degree",
    Demo_Education == "High school graduate" ~ "High school graduate",
    Demo_Education == "Less than a high school diploma or equivalent" ~ "Less than high school diploma",
    Demo_Education == "Some college but no degree" ~ "Some college but no degree",
    TRUE ~ "Other"  # default case
  ))
df$Demo_Education <- factor(df$Demo_Education,
                            levels = c("Less than high school diploma",
                                       "High school graduate",
                                       "Some college but no degree",
                                       "Bachelor's degree",
                                       "Advanced degree"))

# Demo Income
df$Demo_Income <- gsub("\\$", "\\\\$", df$Demo_Income)  # Add two backslashes before $
df$Demo_Income <- factor(df$Demo_Income, levels = c(
  "\\$24,999 or under",
  "\\$25,000 to \\$44,999",
  "\\$45,000 to \\$64,999",
  "\\$65,000 to \\$94,999",
  "\\$95,000 to \\$134,999",
  "\\$135,000 to \\$249,999",
  "\\$250,000 or over"
))

######## Balance table ########

# Create balance table
balance_table <- df %>%
  group_by(condition) %>%
  summarise(
    Observations = n(),
    Age = round(mean(Age, na.rm = TRUE), 2)
  ) %>%
  mutate(condition = as.character(condition)) %>%
  bind_rows(
    df %>%
      summarise(
        condition = 'Overall',
        Observations = n(),
        Age = round(mean(Age, na.rm = TRUE), 2)
      )
  )

# Function to calculate percentages for a categorical variable
calc_percentage <- function(df, condition_col, variable) {
  df %>%
    group_by(!!sym(condition_col), !!sym(variable)) %>%
    summarise(n = n()) %>%
    group_by(!!sym(condition_col)) %>%
    mutate(percent = round(n / sum(n) * 100, 2)) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = !!sym(variable), values_from = percent) %>%
    replace(is.na(.), 0)
}

# Calculate percentages for Sex, Demo_Education, and Demo_Income
sex_percent <- calc_percentage(df, 'condition', 'Sex')
education_percent <- calc_percentage(df, 'condition', 'Demo_Education')
income_percent <- calc_percentage(df, 'condition', 'Demo_Income')

# Add Overall percentages
sex_percent <- bind_rows(
  sex_percent,
  df %>%
    count(Sex) %>%
    mutate(percent = round(n / sum(n) * 100, 2)) %>%
    select(-n) %>%
    pivot_wider(names_from = Sex, values_from = percent) %>%
    replace(is.na(.), 0) %>%
    mutate(condition = 'Overall')
)

education_percent <- bind_rows(
  education_percent,
  df %>%
    count(Demo_Education) %>%
    mutate(percent = round(n / sum(n) * 100, 2)) %>%
    select(-n) %>%
    pivot_wider(names_from = Demo_Education, values_from = percent) %>%
    replace(is.na(.), 0) %>%
    mutate(condition = 'Overall')
)

income_percent <- bind_rows(
  income_percent,
  df %>%
    count(Demo_Income) %>%
    mutate(percent = round(n / sum(n) * 100, 2)) %>%
    select(-n) %>%
    pivot_wider(names_from = Demo_Income, values_from = percent) %>%
    replace(is.na(.), 0) %>%
    mutate(condition = 'Overall')
)

# Combine all the tables
balance_table <- balance_table %>%
  left_join(sex_percent, by = 'condition') %>%
  left_join(education_percent, by = 'condition') %>%
  left_join(income_percent, by = 'condition')

# Add "Sex", "Education", and "Income" columns with empty rows
balance_table <- balance_table %>%
  mutate(`\\textbf{Sex}` = "", .before = first(names(sex_percent)[-1])) %>%
  mutate(`\\textbf{Education}` = "", .before = first(names(education_percent)[-1])) %>%
  mutate(`\\textbf{Income}` = "", .before = first(names(income_percent)[-1]))

# To character
balance_table <- balance_table %>%
  mutate(across(everything(), as.character))

# Transpose the balance table
balance_table_t <- balance_table %>%
  pivot_longer(-condition, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = condition, values_from = Value) %>%
  rename(
    "\\makecell[l]{Control}" = Control,
    "\\makecell[l]{Statement \\\\ Balance}" = SB,
    "\\makecell[l]{Table}" = Table,
    "\\makecell[l]{Slider \\\\ Low}" = SliderLow,
    "\\makecell[l]{Slider \\\\ High}" = SliderHigh,
    "\\makecell[l]{Overall}" = Overall
  )

# export
tex_file <- "paper_tables/balance_table.tex"
balance_table_t %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "") %>%
  save_kable(file = tex_file)