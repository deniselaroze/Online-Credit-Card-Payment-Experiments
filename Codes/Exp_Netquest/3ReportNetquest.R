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

##### Main regressions ######
main_regression_table <- function(df, robust_errors = TRUE, extension = "html"){
  
  df$Payment <- df$Payment/1678
  
  r_payment <- lm(Payment ~ condition, data = df)
  r_lessmin <- lm(LessMin ~ condition, data = df)
  r_min <- lm(Min ~ condition, data = df)
  r_partial <- lm(Partial ~ condition, data = df)
  r_total <- lm(Total ~ condition, data = df)
  
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

reg_male <- main_regression_table(df[df$Sex=="Male",])
reg_female <- main_regression_table(df[df$Sex=="Female",])
reg_lowfl <- main_regression_table(df[df$CorrectFL<3,])
reg_highfl <- main_regression_table(df[df$CorrectFL==3,])
reg_lowedu <- main_regression_table(df[df$Education %in% c("LessThanHS","HighSchool","SomeCollege"),])
reg_highedu <- main_regression_table(df[df$Education %in% c("Bachelors","Advanced"),])
reg_lowage <- main_regression_table(df[df$Age<60,])
reg_highage <- main_regression_table(df[df$Age>=60,])

reg_lowdebtaversion <- main_regression_table(df[df$DebtAversion1_3 %in% c("Neither agree nor disagree","Somewhat disagree", "Disagree", "Strongly disagree"),])
reg_highdebtaversion <- main_regression_table(df[df$DebtAversion1_3 %in% c("Agree", "Somewhat agree", "Strongly agree"),])
####### HTML Report ######
# Create HTML content
html_content <- tagList(
  tags$html(
    tags$head(
      tags$title("Netquest Report")
    ),
    tags$body(
      tags$h1("Regression No Controls (robust errors white)"),
      tags$h3("Sex, Female"),
      HTML(reg_female),
      tags$h3("Sex, Male"),
      HTML(reg_male),
      tags$h3("Low Financial Literacy"),
      HTML(reg_lowfl),
      tags$h3("High Financial Literacy"),
      HTML(reg_highfl),
      tags$h3("Low Education"),
      HTML(reg_lowedu),
      tags$h3("High Education"),
      HTML(reg_highedu),
      tags$h3("Age<60"),
      HTML(reg_lowage),
      tags$h3("Age>=60"),
      HTML(reg_highage),
      tags$h3("Low debt aversion (Neither agree nor disagree or lower)"),
      HTML(reg_lowdebtaversion),
      tags$h3("High debt aversion (Somewhat agree or higher)"),
      HTML(reg_highage),
      
      
    )
  )
)

# Write the HTML content to a file
save_html(html_content, file = "reportNetquest.html")