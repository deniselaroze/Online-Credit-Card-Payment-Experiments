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
  mutate(Choice = list(na.omit(c(Control, SB, Table, SliderLow, SliderHigh)))) %>%
  mutate(Choice = ifelse(length(Choice) == 1, unlist(Choice), "ERROR"))
check_choice <- df[,c("Choice","condition", "Control", "SB", "Table", "SliderLow", "SliderHigh")]

# Check for and combine _TEXT columns into "Choice_Other"
df <- df %>%
  rowwise() %>%
  mutate(Choice_Other = list(na.omit(c(Control_3_TEXT, SB_3_TEXT, Table_3_TEXT, SliderLow_6_TEXT, SliderHigh_6_TEXT)))) %>%
  mutate(Choice_Other = ifelse(length(Choice_Other) <= 1, unlist(Choice_Other), "ERROR"))
check_choice_other <- df[,c("Choice","condition", "Control_3_TEXT", "SB_3_TEXT", "Table_3_TEXT", "SliderLow_6_TEXT", "SliderHigh_6_TEXT")]

# Ungroup the dataframe
df <- df %>%
  ungroup()

###### Create Payment columns
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
df$Payment_Percentage <- round(df$Payment / max(df$Payment),4)

# Clean memory
rm(check_choice,
   check_choice_other,
   check_payment,
   check_payment_range)


###### Dummies #####
df$SB <- ifelse(df$condition == "SB", 1, 0)
df$Table <- ifelse(df$condition == "Table", 1, 0)
df$SliderLow <- ifelse(df$condition == "SliderLow", 1, 0)
df$SliderHigh <- ifelse(df$condition == "SliderHigh", 1, 0)

df$LessMin <- ifelse(df$Payment < 34, 1, 0)
df$Min <- ifelse(df$Payment == 34, 1, 0)
df$Partial <- ifelse(df$Payment > 34 & df$Payment<1678, 1, 0)
df$Total <- ifelse(df$Payment == 1678, 1, 0)

summary(df$LessMin + df$Min + df$Partial + df$Total)

###### Age range #####
# Step 1: Define the age ranges
age_breaks <- c(18, 20, 30, 40, 50, 60, 70, 80, 100)
age_labels <- c("18 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "80 to 100")

# Step 2: Create the age range column using cut
df$Age_range <- cut(df$Age, breaks = age_breaks, labels = age_labels, right = FALSE)

# Check the result
table(df$Age_range)

###### Verbose some columns ######
# Income
df <- df %>%
  mutate(Income = case_when(
    Demo_Income == "$24,999 or under" ~ "Under 25k",
    Demo_Income == "$25,000 to $44,999" ~ "[25k to 45k)",
    Demo_Income == "$45,000 to $64,999" ~ "[45k to 65k)",
    Demo_Income == "$65,000 to $94,999" ~ "[65k to 95k)",
    Demo_Income == "$95,000 to $134,999" ~ "[95k to 135k)",
    Demo_Income == "$135,000 to $249,999" ~ "[135k to 250k)",
    Demo_Income == "$250,000 or over" ~ "Over 250k",
    TRUE ~ "Other"  # default case
  ))
# Education
df <- df %>%
  mutate(Education = case_when(
    Demo_Education == "Advanced degree (Master's degree, professional degree, or doctoral degree)" ~ "Advanced",
    Demo_Education == "Bachelor's degree" ~ "Bachelors",
    Demo_Education == "High school graduate" ~ "HighSchool",
    Demo_Education == "Less than a high school diploma or equivalent" ~ "LessThanHS",
    Demo_Education == "Some college but no degree" ~ "SomeCollege",
    TRUE ~ "Other"  # default case
  ))
# Usual payment
df <- df %>%
  mutate(UsualPayment = case_when(
    Demo_UsualPayment == "An amount between the minimum payment and the statement balance" ~ "BetweenMinAndBalance",
    Demo_UsualPayment == "Less than the minimum payment" ~ "LessThanMin",
    Demo_UsualPayment == "Only the minimum payment" ~ "OnlyMin",
    Demo_UsualPayment == "The full statement balance" ~ "FullBalance",
    TRUE ~ "Other"  # default case
  ))

###### SliderCounts/Use #######
aux <- df$sliderCounts
df<- df %>%
  mutate(sliderCounts = map(sliderCounts, ~ if (!is.na(.x)) jsonlite::fromJSON(.x) else NULL)) %>%
  mutate(sliderCounts = map(sliderCounts, ~ if (!is.null(.x)) setNames(.x, paste0(names(.x))) else NULL)) %>%
  unnest_wider(sliderCounts, names_sep = "_")
df$sliderCounts <- aux

# Sum
df <- df %>%
  mutate(sliderCountSum = rowSums(select(., starts_with("sliderCounts_")), na.rm = TRUE))
df$sliderCountSum <- ifelse(is.na(df$sliderCounts), NA, df$sliderCountSum)

# Use
df$sliderUsed <- ifelse(
  is.na(df$sliderCountSum), NA, ifelse(
    df$sliderCountSum==1,"No","Yes"))

# Check for erros
check <- df %>%
  select(starts_with("sliderCount"))
sum(is.na(df$sliderCounts))==sum(is.na(df$sliderCounts_100))
sum(is.na(df$sliderCounts))==sum(is.na(df$sliderCounts_500))
sum(is.na(df$sliderCounts))==sum(is.na(df$sliderCounts_1678))
check <- check %>%
  filter(!is.na(sliderCounts))



###### Use Gadget column ########
# Check validity of conditions
validity_check <- apply(df, 1, function(row) {
  # Both columns cannot be "Yes" or "No" at the same time
  if (!is.na(row["tableClickButton"]) && !is.na(row["sliderUsed"])) {
    return(row["tableClickButton"] != row["sliderUsed"])
  }
  return(TRUE)
})

if (all(validity_check)) {
  # Create the UseGadget column
  df$UseGadget <- apply(df, 1, function(row) {
    if (!is.na(row["tableClickButton"])) {
      return(row["tableClickButton"])
    } else if (!is.na(row["sliderUsed"])) {
      return(row["sliderUsed"])
    } else {
      return(NA)
    }
  })
} else {
  stop("The dataframe does not meet the required conditions.")
}
###### Explored values column #######
df <- df %>%
  rowwise() %>%
  mutate(sliderExploredValues = sum(c_across(starts_with("sliderCounts_")) != 0)) %>%
  ungroup()

df$sliderExploredValues <- ifelse(is.na(df$sliderCounts), NA, df$sliderExploredValues)
###### Late fees and financial lit #######
df$CorrectLateFees1 <- ifelse(df$LateFees1 == "$37", 1, 0)
df$CorrectLateFees2 <- ifelse(df$LateFees2 == "$1,355", 1, 0)
df$CorrectLateFees <- df$CorrectLateFees1 + df$CorrectLateFees2

df$CorrectFL1 <- ifelse(df$FL1 == "More than $102", 1, 0)
df$CorrectFL2 <- ifelse(df$FL2 == "Less than today", 1, 0)
df$CorrectFL3 <- ifelse(df$FL3 == "False", 1, 0)
df$CorrectFL <- df$CorrectFL1 + df$CorrectFL2 + df$CorrectFL3

df$CorrectFLplusLateFees <- df$CorrectLateFees1 + df$CorrectLateFees2+ df$CorrectFL1 + df$CorrectFL2 + df$CorrectFL3


###### Save #####
write.csv(df, "processed_data/ProcessedData.csv", row.names = FALSE)
