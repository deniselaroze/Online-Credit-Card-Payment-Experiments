###### Setup ######
rm(list=ls())
library(dplyr)
library(htmltools)
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
# Load
qualtrics <- read.csv("raw_data/Fondef_Ex3_March 7, 2024_14.55.csv",na.strings=c(''))
ages_netquest <- read_excel("raw_data/Age_Netquest.xlsx")

# Duplicated row in ages: 1 case, same info
ages_netquest <- ages_netquest[!duplicated(ages_netquest), ]

# Select study
qualtrics <- qualtrics[c(-1,-2),]
qualtrics <-  subset(qualtrics, !is.na(qualtrics$ticket))
qualtrics <- qualtrics[qualtrics$Finished>="True",]

# Merge
df = merge(qualtrics, ages_netquest, by = "ticket", all.x = TRUE)
rm(qualtrics, ages_netquest)

# Filter declined consent: 3 cases
df <- df %>%
  filter(Consent2_1 == "Yes" & Consent2_2 == "Yes" & Consent2_3 == "Yes")

# Filter not elegible: 33 cases
df <- df %>%
  filter(df$Eligibility2 != "I do not have, but I would like to have a credit card soon" &
           df$Eligibility2 != "I do not have and have not used a credit card" &
           df$Eligibility2 != "I am not sure if I have used a credit card" &
           df$Eligibility3 != "I do not have or do not use a credit card" &
           df$Eligibility3 != "More than a year ago")

# Filter no condition: 0 cases
df <- df %>% filter(!is.na(condition))

# Filter no age data: 0 cases 
df = df[!is.na(df$age), ]

# Check duplicated ticket: 1 case, keep first 
duplicates_df <- df %>%
  group_by(ticket) %>%
  filter(n() > 1) %>%
  ungroup()
df <- df %>%
  distinct(ticket, .keep_all = TRUE)

# Attrition: 8 cases abandoned just after receiving condition
df <- df %>%
  rowwise() %>%
  mutate(Choice = list(na.omit(c(C_Min.58, T1_Min.58Slider, T2_Min.SB, T3_Min.SBSlider)))) %>%
  mutate(Choice = ifelse(length(Choice) == 1, unlist(Choice), "ERROR"))
df <- df[df$Choice!="ERROR",]
att <- df[df$Choice=="ERROR",]

# Consolidate
df$Study <- "Netquest1000"

# Save
write.csv(df, "processed_data/CleanData.csv", row.names = FALSE)
write.csv(df, "processed_data/Attrition_Log.csv", row.names = FALSE)

