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