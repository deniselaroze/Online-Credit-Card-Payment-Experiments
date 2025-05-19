###### Setup ######
# Libraries
rm(list=ls())
library(dplyr)
library(htmltools)
library(tidyverse)
library(kableExtra)
library(knitr)
library(htmlTable)
library(ggplot2)
library(scales)
library(readxl)
library(tibble)
# Location
file_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file_path)
# Open data
df <- read.csv("processed_data/CleanData.csv")

###### Consolidate Choice and Choice_Other (textbox) ######

# Check for and combine choice columns into "Choice"
df <- df %>%
  rowwise() %>%
  mutate(Choice = list(na.omit(c(C_Min.58, T1_Min.58Slider, T2_Min.SB, T3_Min.SBSlider)))) %>%
  mutate(Choice = ifelse(length(Choice) == 1, unlist(Choice), "ERROR"))
check_choice <- df[,c("Choice","condition", "C_Min.58", "T1_Min.58Slider", "T2_Min.SB", "T3_Min.SBSlider")]

# Check for and combine _TEXT columns into "Choice_Other"
df <- df %>%
  rowwise() %>%
  mutate(Choice_Other = list(na.omit(c(C_Min.58_3_TEXT, T1_Min.58Slider_6_TEXT, T2_Min.SB_3_TEXT, T3_Min.SBSlider_6_TEXT)))) %>%
  mutate(Choice_Other = ifelse(length(Choice_Other) <= 1, unlist(Choice_Other), "ERROR"))
check_choice_other <- df[,c("Choice","condition", "C_Min.58_3_TEXT", "T1_Min.58Slider_6_TEXT", "T2_Min.SB_3_TEXT", "T3_Min.SBSlider_6_TEXT")]

# Clean memory
rm(check_choice,
   check_choice_other)

###### Create Payment columns ########
# Payment column
df <- df %>%
  mutate(Payment = case_when(
    Choice == "Statement balance" ~ 1678,
    Choice == "Minimum payment" ~ 34,
    Choice == "Other amount ($)" ~ as.numeric(Choice_Other),
    TRUE ~ NA_real_  # for any other cases
  ))
check_payment <- df[,c("Choice","Choice_Other", "Payment")]

# Payment range
df <- df %>%
  mutate(Payment_Range = cut(Payment,
                             breaks = c(0, 34, 35, 1678, 1679),
                             labels = c("Less than 34", "34", "34 to 1677", "1678"),
                             right = FALSE))
check_payment_range <- df[,c("Payment","Payment_Range")]

# Payment percentage
df$Payment_Percentage <- round(df$Payment / 1678,4)

# Clean memory
rm(check_payment,
   check_payment_range)


###### Dummies #####

# Condition
df <- df %>%
  mutate(condition = recode(condition,
                            "control" = "Control",
                            "treat1" = "SliderLow",
                            "treat2" = "SB",
                            "treat3" = "SliderHigh"))

df$SB <- ifelse(df$condition == "SB", 1, 0)
df$SliderLow <- ifelse(df$condition == "SliderLow", 1, 0)
df$SliderHigh <- ifelse(df$condition == "SliderHigh", 1, 0)

# Payment
df$LessMin <- ifelse(df$Payment < 34, 1, 0)
df$Min <- ifelse(df$Payment == 34, 1, 0)
df$Partial <- ifelse(df$Payment > 34 & df$Payment<1678, 1, 0)
df$Total <- ifelse(df$Payment == 1678, 1, 0)

# Check
summary(df$LessMin + df$Min + df$Partial + df$Total)

###### Age range #####
# Change name of column age
df$Age <- df$age
df <- df %>%
  select(-age)

# Step 1: Define the age ranges
age_breaks <- c(18, 20, 30, 40, 50, 60, 70, 80, 100)
age_labels <- c("18 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "80 to 100")

# Step 2: Create the age range column using cut
df$Age_range <- cut(df$Age, breaks = age_breaks, labels = age_labels, right = FALSE)

# Check the result
table(df$Age_range)

###### Verbose columns ######
# Sex
df$Sex <- ifelse(df$s==1, "Male", ifelse(df$s==2, "Female", NA))

# Income
# Its household income
value_mapping <- c(
  "Less than $10,000" = 1,
  "$10,000 - $14,999" = 2,
  "$15,000 - $24,999" = 3,
  "$25,000 - $34,999" = 4,
  "$35,000 - $44,999" = 5,
  "$45,000 - $64,999" = 6,
  "$65,000 - $79,999" = 7,
  "$80,000 - $99,999" = 8,
  "$100,000 - $124,999" = 9,
  "$125,000 - $149,999" = 10,
  "$150,000 - $174,999" = 11,
  "$175,000 - $199,999" = 12,
  "$200,000 - $249,999" = 13,
  "$250,000 or over" = 14,
  "I prefer not to answer" = 98
)
inverse_mapping <- setNames(names(value_mapping), as.character(value_mapping))
df$g_verbose <- inverse_mapping[as.character(df$g)]
# Income as prolific experiment levels
df <- df %>%
  mutate(Income = case_when(
    g_verbose == "Less than $10,000" ~ "Under 25k",
    g_verbose == "$10,000 - $14,999" ~ "Under 25k",
    g_verbose == "$15,000 - $24,999" ~ "Under 25k",
    g_verbose == "$25,000 - $34,999" ~ "[25k to 45k)",
    g_verbose == "$35,000 - $44,999" ~ "[25k to 45k)",
    g_verbose == "$45,000 - $64,999" ~ "[45k to 65k)",
    g_verbose == "$65,000 - $79,999" ~ "[65k to 100k)",
    g_verbose == "$80,000 - $99,999" ~ "[65k to 100k)",
    g_verbose == "$100,000 - $124,999" ~ "[100k to 150k)",
    g_verbose == "$125,000 - $149,999" ~ "[100k to 150k)",
    g_verbose ==  "$150,000 - $174,999" ~ "[150k to 250k)",
    g_verbose ==  "$175,000 - $199,999" ~ "[150k to 250k)",
    g_verbose ==  "$200,000 - $249,999" ~ "[150k to 250k)",
    g_verbose == "$250,000 or over" ~ "Over 250k",
    g_verbose == "I prefer not to answer" ~ "No answer",
    TRUE ~ "Other"  # default case
  ))

# Education
df <- df %>%
  mutate(Education = case_when(
    Demographics1 == "Advanced degree (Master's degree, professional degree, or doctoral degree)" ~ "Advanced",
    Demographics1 == "Bachelor's degree" ~ "Bachelors",
    Demographics1 == "High school graduate" ~ "HighSchool",
    Demographics1 == "Less than a high school diploma or equivalent" ~ "LessThanHS",
    Demographics1 == "Some college but no degree" ~ "SomeCollege",
    TRUE ~ "Other"  # default case
  ))

###### Late Fees #######

df$CorrectFL1 <- ifelse(df$FL1 == "More than $102", 1, 0)
df$CorrectFL2 <- ifelse(df$FL2 == "Less than today", 1, 0)
df$CorrectFL3 <- ifelse(df$FL3 == "False", 1, 0)
df$CorrectFL <- df$CorrectFL1 + df$CorrectFL2 + df$CorrectFL3

###### SliderCounts/Use #######
# Todo

###### Save #####
write.csv(df, "processed_data/ProcessedData.csv", row.names = FALSE)
