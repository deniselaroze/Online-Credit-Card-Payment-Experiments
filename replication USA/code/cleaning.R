library(data.table)
library(tidyverse)

setwd("/Users/elisaduranmicco/Dropbox/2103 - ANID - FONDEF Sernac/11.Pago/replication")

# open data raw
data01 <- read.csv("data/DataRaw.csv") #2508
data01 <- as.data.table(data01)

### Cleaning data

# NA without treatment: 5 cases, only one has a treatment assigned.
not_treated <- data01 %>% filter(is.na(Time_Condition_Page.Submit))
data <- data01 %>% filter(!is.na(Time_Condition_Page.Submit))

### Create variables

# correct typo in Sex
data$Sex[data$Sex == "Female, Female"] <- "Female"

# create financial literacy
data$FL <- ifelse(data$FL1 == "More than $102", 1, 0)
data$FL <- ifelse(data$FL2 == "Less than today", data$FL + 1, data$FL)
data$FL <- ifelse(data$FL3 == "False", data$FL + 1, data$FL)

# Create dependent variables
data$Control_3_TEXT <- as.numeric(as.character(data$Control_3_TEXT))
data$SB_3_TEXT <- as.numeric(as.character(data$SB_3_TEXT))
data$NotUsed_Table_3_TEXT <- as.numeric(as.character(data$NotUsed_Table_3_TEXT))
data$Table_3_TEXT <- as.numeric(as.character(data$Table_3_TEXT))
data$SliderLow_6_TEXT <- as.numeric(as.character(data$SliderLow_6_TEXT))
data$SliderHigh_6_TEXT<- as.numeric(as.character(data$SliderHigh_6_TEXT))

data$y0 <- "NA"

data[ Control == "Minimum payment", y0 := 34]
data[ SB == "Minimum payment", y0 := 34]
data[ Table == "Minimum payment", y0 := 34]
data[ SliderLow == "Minimum payment", y0 := 34]
data[ SliderHigh == "Minimum payment", y0 := 34]

data[ Control_3_TEXT  > 0, y0 := Control_3_TEXT ]
data[ SB_3_TEXT > 0, y0 := SB_3_TEXT]
data[ Table_3_TEXT > 0, y0 := Table_3_TEXT]
data[ SliderLow_6_TEXT > 0, y0 := SliderLow_6_TEXT]
data[ SliderHigh_6_TEXT > 0, y0 := SliderHigh_6_TEXT]

data[ Control == "Statement balance", y0 := 1678]
data[ SB == "Statement balance", y0 := 1678]
data[ Table == "Statement balance", y0 := 1678]
data[ SliderLow == "Statement balance", y0 := 1678]
data[ SliderHigh == "Statement balance", y0 := 1678]

data <- subset(data, !(data$y0=="NA")) 

data$condition <- as.factor(as.character(data$condition))
data$y0 <- as.numeric(as.character(data$y0))
data$share <- 100*data$y0/1678

data$pay <- ifelse(data$y0 == 1678, "SB", ifelse(data$y0 == 34, "min","other"))

data$Min <- ifelse(data$y0==34, 1, 0)
data$Partial <- ifelse(data$y0>34 & data$y0<1678 , 1, 0)
data$Total <- ifelse(data$y0==1678 , 1, 0)

# create satisfaction 
data$satis <- "NA"
data$satis[data$S1_1 == "Extremely unconfident"] <- 1
data$satis[data$S1_1 == "Very unconfident"] <- 2
data$satis[data$S1_1 == "Unconfident"] <- 3
data$satis[data$S1_1 == "Neither confident nor unconfident"] <- 4
data$satis[data$S1_1 == "Confident"] <- 5
data$satis[data$S1_1 == "Very confident"] <- 6
data$satis[data$S1_1 == "Extremely confident"] <- 7

data$satis <- as.numeric(data$satis)

#create late fees
data$LFees <- ifelse((data$LateFees1 == "$37" & data$LateFees2 == "$1,355"), 1, 0)

#create debt aversion
data$DA1 <- ifelse(data$DebtAversion1_1 == "Strongly agree", 1, 7) 
data$DA1[data$DebtAversion1_1 == "Agree"] <- 2
data$DA1[data$DebtAversion1_1 == "Somewhat agree"] <- 3
data$DA1[data$DebtAversion1_1 == "Neither agree nor disagree"] <- 4
data$DA1[data$DebtAversion1_1 == "Somewhat disagree"] <- 5
data$DA1[data$DebtAversion1_1 == "Disagree"] <- 6
data$DA1[data$DebtAversion1_1 == "NA"] <- "NA"

data$DA2 <- ifelse(data$DebtAversion1_2 == "Strongly agree", 7, 1) 
data$DA2[data$DebtAversion1_2 == "Agree"] <- 6
data$DA2[data$DebtAversion1_2 == "Somewhat agree"] <- 5
data$DA2[data$DebtAversion1_2 == "Neither agree nor disagree"] <- 4
data$DA2[data$DebtAversion1_2 == "Somewhat disagree"] <- 3
data$DA2[data$DebtAversion1_2 == "Disagree"] <- 2
data$DA2[data$DebtAversion1_2 == "NA"] <- "NA"

data$DA <- (as.numeric(data$DA1) + as.numeric(data$DA2))/2

#create income
data$ses <- ifelse(data$Demo_Income== "$250,000 or over", 7, 1) 
data$ses[data$Demo_Income == "$135,000 to $249,999"] <- 6
data$ses[data$Demo_Income == "$95,000 to $134,999"] <- 5
data$ses[data$Demo_Income == "$65,000 to $94,999"] <- 4
data$ses[data$Demo_Income == "$45,000 to $64,999"] <- 3
data$ses[data$Demo_Income == "$25,000 to $44,999"] <- 2

write.csv(data, "data/FinalData.csv", row.names = FALSE)
