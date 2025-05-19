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
# location
file_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file_path)

###### First100 ######
# Load
qualtrics <- read.csv("raw_data/Fondef_Ex4_June 14, 2024_05.01.csv",na.strings=c(''))
prolific <- read.csv("raw_data/First100/prolific_export_66579a3533a2d2fefa03cfd4.csv",na.strings=c(''))

# Select study
qualtrics <- qualtrics[c(-1,-2),]
qualtrics <- qualtrics %>%
  filter(!is.na(PROLIFIC_PID) & STUDY_ID == "66579a3533a2d2fefa03cfd4")

# Missing demographics: 0 cases
qualtrics_not_in_prolific <- anti_join(qualtrics, prolific, 
                                       by = c("PROLIFIC_PID" = "Participant.id"))

# Merge
df <- merge(qualtrics, prolific,
            by.x = "PROLIFIC_PID", by.y = "Participant.id", 
            all.x = TRUE)

# Delete consent revoked: 1 cases
consent_revoked <- df %>% filter(Nationality == "CONSENT_REVOKED")
df <- df %>% filter(Nationality != "CONSENT_REVOKED")

# Delete condition NA: 0 cases
condition_na <- df %>% filter(is.na(condition))
df <- df %>% filter(!is.na(condition))

# Duplicated: 0 cases
duplicates_qualtrics <- qualtrics %>%
  group_by(PROLIFIC_PID) %>%
  filter(n() > 1) %>%
  ungroup()

# Consolidate
df$Study <- "First100"
df1 <- df

# Remove 
rm(df,
   prolific,
   qualtrics,
   consent_revoked,
   duplicates_qualtrics,
   qualtrics_not_in_prolific,
   condition_na)

###### Check1000 ######
# Load
qualtrics <- read.csv("raw_data/Fondef_Ex4_June 14, 2024_05.01.csv",na.strings=c(''))
prolific <- read.csv("raw_data/Second1000/prolific_export_665f3028396367c4afb4b1d6.csv",na.strings=c(''))

# Select study
qualtrics <- qualtrics[c(-1,-2),]
qualtrics <- qualtrics %>%
  filter(!is.na(PROLIFIC_PID) & STUDY_ID == "665f3028396367c4afb4b1d6")

# Missing demographics: 0 cases
qualtrics_not_in_prolific <- anti_join(qualtrics, prolific, 
                                       by = c("PROLIFIC_PID" = "Participant.id"))

# Merge
df <- merge(qualtrics, prolific,
            by.x = "PROLIFIC_PID", by.y = "Participant.id", 
            all.x = TRUE)

# Delete consent revoked: 16 cases
consent_revoked <- df %>% filter(Nationality == "CONSENT_REVOKED")
df <- df %>% filter(Nationality != "CONSENT_REVOKED")

# Delete condition NA: 1 case, not elegible because no credit card owned
condition_na <- df %>% filter(is.na(condition))
df <- df %>% filter(!is.na(condition))

# Duplicated: 1 case with different responses to demographics -> deleted
duplicates_qualtrics <- df %>%
  group_by(PROLIFIC_PID) %>%
  filter(n() > 1) %>%
  ungroup()
df <- df %>% filter(PROLIFIC_PID != "65aead8916eb9b7185ed25f3")

# Unfinished
not_finished<-df[df$Finished=="False" | is.na(df$Demo_Income),]
df <- df[!(df$Finished=="False" | is.na(df$Demo_Income)),]

# Consolidate
df$Study <- "Second1000"
df2 <- df

# Remove 
rm(df,
   prolific,
   qualtrics,
   consent_revoked,
   duplicates_qualtrics,
   qualtrics_not_in_prolific,
   condition_na)

###### Last1400 ######
# Load
qualtrics <- read.csv("raw_data/Fondef_Ex4_June 14, 2024_05.01.csv",na.strings=c(''))
prolific <- read.csv("raw_data/Last1400/prolific_export_666a3b2efdc8101c4f239206.csv",na.strings=c(''))
prolific$Sex <- ifelse(prolific$Sex=="Female, Female", "Female", prolific$Sex)

# Select study
qualtrics <- qualtrics[c(-1,-2),]
qualtrics <- qualtrics %>%
  filter(!is.na(PROLIFIC_PID) & STUDY_ID == "666a3b2efdc8101c4f239206")

# Missing demographics: 1 case. Drop
qualtrics_not_in_prolific <- anti_join(qualtrics, prolific, 
                                       by = c("PROLIFIC_PID" = "Participant.id"))

# Missing qualtrics and waiting review: 1 case. No payment and rejected
prolific_not_in_qualtrics <- anti_join(prolific, qualtrics,
                                       by = c("Participant.id" = "PROLIFIC_PID"))
prolific_not_in_qualtrics <- prolific_not_in_qualtrics[prolific_not_in_qualtrics$Status=="AWAITING REVIEW",]

# Merge
df <- merge(qualtrics, prolific,
            by.x = "PROLIFIC_PID", by.y = "Participant.id", 
            all.x = TRUE)
df <- df[!is.na(df$Nationality),]

# Delete consent revoked: 27 cases
consent_revoked <- df %>% filter(Nationality == "CONSENT_REVOKED")
df <- df %>% filter(Nationality != "CONSENT_REVOKED")

# Delete condition NA: 2 case, not elegible because no credit card owned
condition_na <- df %>% filter(is.na(condition))
df <- df %>% filter(!is.na(condition))

# Duplicated: 0 case with different responses to demographics
duplicates_qualtrics <- df %>%
  group_by(PROLIFIC_PID) %>%
  filter(n() > 1) %>%
  ungroup()

# Unfinished
not_finished<-df[df$Finished=="False" | is.na(df$Demo_Income),]
df <- df[!(df$Finished=="False" | is.na(df$Demo_Income)),]
  
# Consolidate
df$Study <- "Last1400"
df3 <- df

# Remove 
rm(df,
   prolific,
   qualtrics,
   consent_revoked,
   duplicates_qualtrics,
   qualtrics_not_in_prolific,
   condition_na)

###### Merge and save ######
df <- rbind(df1, df2, df3)

# Age: 1 observation age=113, remove
df$Age <- as.numeric(df$Age)
df <- df[df$Age<100,]

write.csv(df, "processed_data/CleanData.csv", row.names = FALSE)

