library(tidyverse)
library(modelsummary)
library(estimatr)
library(data.table)
library(stargazer)
library(lmtest)
library(sandwich)
library(MASS)
library(nnet) #multinomial
library(marginaleffects) # For average marginal effects (AMEs)


# location
setwd("/Users/elisaduranmicco/Dropbox/2103 - ANID - FONDEF Sernac/11.Pago/replication CHI")

data <- read.csv("./data/DataEx1.csv",na.strings=c('')) #1044

### Table A.1: Balance
# Sex (1 male 2 female) 
table(data$s, data$treat)
data$male <-  ifelse(data$s == 1, 1, 0)
chisq.test(table(data$treat, data$male))

# Socio-economic level
table(data$gse, data$treat)
data$abc1 <-  ifelse(data$gse == "ABC1", 1, 0)
chisq.test(table(data$treat, data$abc1))
data$c2 <-  ifelse(data$gse == "C2", 1, 0)
chisq.test(table(data$treat, data$c2))
data$c3 <-  ifelse(data$gse == "C3", 1, 0)
chisq.test(table(data$treat, data$c3))
data$de <-  ifelse(data$gse == "DE", 1, 0)
chisq.test(table(data$treat, data$de))

# Finantial literacy
data %>%
  group_by(treat) %>%
  summarise_at(vars(LC), list(mean, sd))
anova_LC <- aov(LC ~ treat, data = data)
summary(anova_LC)

# Age
edad <- read.csv("./data/edad_exChile.csv", sep = ";", header = FALSE, col.names = c("ticket", "age"))[-(1),]
data04 <- subset(edad, edad$ticket %in% data$ticket) # 2 miss age and 25 repeated
data05 <- as.data.table(subset(data04, duplicated(data04$ticket) == "FALSE"))
data06 <- merge(data, data05, by=c("ticket"))
mean(data05$age, na.rm =TRUE)
sd(data05$age, na.rm =TRUE)
data06  %>%
  group_by(treat) %>%
  summarise_at(vars(age), list(mean, sd))

anova_age <- aov(age ~ treat, data = subset(data06, !is.na(data06$age)))
summary(anova_age)

### Table 1: Descriptive Results 

data %>%
  group_by(treat0, op) %>%
  summarise(count = n()) %>%
  group_by(treat0) %>%
  mutate(percentage = count / sum(count))

data %>%
  group_by(treat0) %>%
  summarise_at(vars(share_p0), list(mean))

data %>%
  group_by(treat, op2) %>%
  summarise(count = n()) %>%
  group_by(treat) %>%
  mutate(percentage = count / sum(count))

data %>%
  group_by(treat) %>%
  summarise_at(vars(share_p1), list(mean))

### Table 2: Linear regression
# Robust errors
get_robust_se <- function(lm_obj) {
  coeftest_obj <- coeftest(lm_obj, vcov = vcovHC(lm_obj, type="HC0"))
  se <- coeftest_obj[, "Std. Error"]
  pval <- coeftest_obj[, "Pr(>|t|)"]
  return(se)
}

get_robust_pv <- function(lm_obj) {
  coeftest_obj <- coeftest(lm_obj, vcov = vcovHC(lm_obj, type="HC0"))
  se <- coeftest_obj[, "Std. Error"]
  pval <- coeftest_obj[, "Pr(>|t|)"]
  return(pval)
}

# First Decision
reg00 <- lm(share_p0 ~ treat0, data = data)
reg01 <- lm(formula = less0 ~ treat0, data = data)
reg02 <- lm(formula = min0 ~ treat0, data = data)
reg03 <- lm(formula = otro0 ~ treat0, data = data)
reg04 <- lm(formula = total0 ~ treat0, data = data)

stargazer(reg00, reg01, reg02, reg03, reg04,
          type = "text", 
          style = "default", 
          se = list(get_robust_se(reg00), 
                    get_robust_se(reg01), 
                    get_robust_se(reg02),
                    get_robust_se(reg03), 
                    get_robust_se(reg04)))

# Second Decision
reg10 <- lm(share_p1 ~ treat00 + treat11 + (treat00*treat11), data = data)
reg11 <- lm(formula = less1 ~ treat00 + treat11 + (treat00*treat11), data = data)
reg12 <- lm(formula = min1 ~ treat00 + treat11 + (treat00*treat11), data = data)
reg13 <- lm(formula = otro1 ~ treat00 + treat11 + (treat00*treat11), data = data)
reg14 <- lm(formula = total1 ~ treat00 + treat11 + (treat00*treat11), data = data)

stargazer(reg10, reg11, reg12, reg13, reg14,
          type = "text", 
          style = "default", 
          se = list(get_robust_se(reg10), 
                    get_robust_se(reg11), 
                    get_robust_se(reg12),
                    get_robust_se(reg13), 
                    get_robust_se(reg14)))

### Table A.2: Second Decision no interaction
reg120 <- lm(share_p1 ~ treat00 + treat11, data = data)
reg121 <- lm(formula = less1 ~ treat00 + treat11, data = data)
reg122 <- lm(formula = min1 ~ treat00 + treat11, data = data)
reg123 <- lm(formula = otro1 ~ treat00 + treat11, data = data)
reg124 <- lm(formula = total1 ~ treat00 + treat11, data = data)

stargazer(reg120, reg121,reg122, reg123, reg124,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se(reg120),
            get_robust_se(reg121),
            get_robust_se(reg122),
            get_robust_se(reg123),
            get_robust_se(reg124)))

### Table A.3: Regresion with controls

data_LC <- subset(data, !(data$LF1. == "NA"))
data_LC$s <- as.factor(data_LC$s)

# First Decision
reg00c <- lm(share_p0 ~ treat0 + s + gse + LC, data = data_LC)
reg01c <- lm(formula = less0 ~ treat0 + s + gse + LC, data = data_LC)
reg02c <- lm(formula = min0 ~ treat0 + s + gse + LC , data = data_LC)
reg03c <- lm(formula = otro0 ~ treat0 + s + gse + LC, data = data_LC)
reg04c <- lm(formula = total0 ~ treat0 + s + gse + LC, data = data_LC)

stargazer(reg00c, reg01c,reg02c, reg03c, reg04c,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se(reg00c),
            get_robust_se(reg01c),
            get_robust_se(reg02c),
            get_robust_se(reg03c),
            get_robust_se(reg04c)))

# Second Decision
reg10c <- lm(share_p1 ~ treat00 + treat11 + (treat00*treat11) + s + gse + LC, data = data_LC)
reg11c <- lm(formula = less1 ~ treat00 + treat11 + (treat00*treat11) + s + gse + LC, data = data_LC)
reg12c <- lm(formula = min1 ~ treat00 + treat11 + (treat00*treat11) + s + gse + LC, data = data_LC)
reg13c <- lm(formula = otro1 ~ treat00 + treat11 + (treat00*treat11) + s + gse + LC, data = data_LC)
reg14c <- lm(formula = total1 ~ treat00 + treat11 + (treat00*treat11) + s + gse + LC, data = data_LC)

stargazer(reg10c, reg11c, reg12c, reg13c, reg14c,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se(reg10c),
            get_robust_se(reg11c),
            get_robust_se(reg12c),
            get_robust_se(reg13c),
            get_robust_se(reg14c)))

### Table A.4: Logit models

### Robust SEs for Logit ###
get_robust_se_logit <- function(glm_obj) {
  coeftest_obj <- coeftest(glm_obj, vcov = vcovHC(glm_obj, type = "HC0"))
  se <- coeftest_obj[, "Std. Error"]
  return(se)
}

# First Decision
logit01 <- glm(less0 ~ treat0, data = data, family = binomial(link = "logit"))
logit02 <- glm(min0 ~ treat0, data = data, family = binomial(link = "logit"))
logit03 <- glm(otro0 ~ treat0, data = data, family = binomial(link = "logit"))
logit04 <- glm(total0 ~ treat0, data = data, family = binomial(link = "logit"))

### Report Log-Odds Coefficients 
stargazer(logit01, logit02, logit03, logit04,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se_logit(logit01),
            get_robust_se_logit(logit02),
            get_robust_se_logit(logit03),
            get_robust_se_logit(logit04)
          ))

# Second Decision 
logit11 <- glm(less1 ~ treat00 + treat11 + treat00:treat11, data = data, family = binomial(link = "logit"))
logit12 <- glm(min1 ~ treat00 + treat11 + treat00:treat11, data = data, family = binomial(link = "logit"))
logit13 <- glm(otro1 ~ treat00 + treat11 + treat00:treat11, data = data, family = binomial(link = "logit"))
logit14 <- glm(total1 ~ treat00 + treat11 + treat00:treat11, data = data, family = binomial(link = "logit"))

stargazer(logit11, logit12, logit13, logit14,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se_logit(logit11),
            get_robust_se_logit(logit12),
            get_robust_se_logit(logit13),
            get_robust_se_logit(logit14)
          ))

### Table A.5: Multinomial model 

### Decision 1
data$op <- relevel(factor(data$op), ref = "total")
model <- multinom(op ~ treat0, data = data)

summary(model)

# Get coefficients and standard errors
coef1 <- summary(model)$coefficients
se1 <- summary(model)$standard.errors

# Calculate z-values and p-values
z1 <- coef1/se1
p1 <- 2*(1 - pnorm(abs(z1)))

# Display results
print("Decision 1 P-values:")
print(p1)

### Decision 2
data$op2 <- relevel(factor(data$op2), ref = "total")
model2 <- multinom(op2 ~ treat00 + treat11 + treat00:treat11, data = data)

# Option 1: Basic summary (non-robust SEs)
summary(model2)

# Get coefficients and standard errors
coef2 <- summary(model2)$coefficients
se2 <- summary(model2)$standard.errors

# Calculate z-values and p-values
z2 <- coef2/se2
p2 <- 2*(1 - pnorm(abs(z2)))

# Display results
print("Decision 2 P-values:")
print(p2)

# For Decision 1 model
ll1 <- logLik(model)
print(paste("Log-likelihood for Decision 1:", ll1))

# For Decision 2 model
ll2 <- logLik(model2)
print(paste("Log-likelihood for Decision 2:", ll2))

### Table A.6: Decision 2 - keep full decision 1

data_full <- data
data_full$share_p1[data_full$share_p0 == 1] <- 1
data_full$less1[data_full$total0 == 1] <- 0
data_full$min1[data_full$total0 == 1] <- 0
data_full$otro1[data_full$total0 == 1] <- 0
data_full$total1[data_full$total0 == 1] <- 1

reg10f <- lm(share_p1 ~ treat00 + treat11 + (treat00*treat11), data = data_full)
reg11f <- lm(formula = less1 ~ treat00 + treat11 + (treat00*treat11), data = data_full)
reg12f <- lm(formula = min1 ~ treat00 + treat11 + (treat00*treat11), data = data_full)
reg13f <- lm(formula = otro1 ~ treat00 + treat11 + (treat00*treat11), data = data_full)
reg14f <- lm(formula = total1 ~ treat00 + treat11 + (treat00*treat11), data = data_full)


row_names <- c("Treatment", "Revolving debt", "Interaction", "Constant")
stargazer(reg10f, reg11f, reg12f, reg13f, reg14f,
          type = "text", 
          style = "default", 
          se = list(get_robust_se(reg10f), 
                    get_robust_se(reg11f), 
                    get_robust_se(reg12f),
                    get_robust_se(reg13f), 
                    get_robust_se(reg14f)))

### Table A.7: Heterogeneity

# LC 
data_LC <- subset(data, !(data$LF1. == "NA"|data$LF2. == "NA"|data$LF3. == "NA"))

model1_LC <- lm(total1 ~ (treat00 + treat11) * LC, data = data_LC)
coeftest(model1_LC, vcov = vcovHC(model1_LC, type = "HC1"))

model2_LC <- lm(total1 ~ (treat00 + treat11 + (treat00*treat11)) * LC, data = data_LC)
coeftest(model2_LC, vcov = vcovHC(model2_LC, type = "HC1"))

# DS 
data_DS <- subset(data, !(data$DS1. == "NA"))
data_DS$DS <- ifelse(data_DS$DS1. == "Compra en la tienda que está más lejos", 1,0)

model1_DS <- lm(total1 ~ (treat00 + treat11) * DS , data = data_DS)
coeftest(model1_DS, vcov = vcovHC(model1_DS, type = "HC1"))

model2_DS <- lm(total1 ~ (treat00 + treat11 + (treat00*treat11)) * DS, data = data_DS)
coeftest(model2_DS, vcov = vcovHC(model2_DS, type = "HC1"))

# PS
data_PS <- subset(data, !(data$PS1._1 == "NA"))
data_PS$PS <- ifelse(data_PS$PS1._1 == "Nada\n1\n", 1, 7) 
data_PS$PS[data_PS$PS1._1 == "2"] <- 2
data_PS$PS[data_PS$PS1._1 == "3"] <- 3
data_PS$PS[data_PS$PS1._1 == "4"] <- 4
data_PS$PS[data_PS$PS1._1 == "5"] <- 5
data_PS$PS[data_PS$PS1._1 == "6"] <- 6

model1_PS <- lm(total1 ~ (treat00 + treat11) * PS , data = data_PS)
coeftest(model1_PS, vcov = vcovHC(model1_PS, type = "HC1"))

model2_PS <- lm(total1 ~ (treat00 + treat11 + (treat00*treat11)) * PS, data = data_PS)
coeftest(model2_PS, vcov = vcovHC(model2_PS, type = "HC1"))

# male 
model1_male <- lm(total1 ~ (treat00 + treat11) * male, data = data)
coeftest(model1_male, vcov = vcovHC(model1_male, type = "HC1"))

model2_male <- lm(total1 ~ (treat00 + treat11 + (treat00*treat11)) * male, data = data)
coeftest(model2_male, vcov = vcovHC(model2_male, type = "HC1"))

# abc1 y c2

data$ses <- data$abc1 + data$c2 
model1_abc1 <- lm(total1 ~ (treat00 + treat11) * ses, data = data)
coeftest(model1_abc1, vcov = vcovHC(model1_abc1, type = "HC1"))

model2_abc1 <- lm(total1 ~ (treat00 + treat11 + (treat00*treat11)) * ses, data = data)
coeftest(model2_abc1, vcov = vcovHC(model2_abc1, type = "HC1"))


### Experiment with liquidity contraints

data2 <- read.csv("./data/DataEx2.csv",na.strings=c('')) #1051

### Table A.8: Descriptive Results 

data2 %>%
  group_by(treat0, op) %>%
  summarise(count = n()) %>%
  group_by(treat0) %>%
  mutate(percentage = count / sum(count))

data2 %>%
  group_by(treat0) %>%
  summarise_at(vars(share_p0), list(mean))

data2 %>%
  group_by(treat, op2) %>%
  summarise(count = n()) %>%
  group_by(treat) %>%
  mutate(percentage = count / sum(count))

data2 %>%
  group_by(treat) %>%
  summarise_at(vars(share_p1), list(mean))

### Table A.9: Linear regression 

# First Decision
regc0 <- lm(share_p0 ~ treat0, data = data2)
regc1 <- lm(formula = less0 ~ treat0, data = data2)
regc2 <- lm(formula = min0 ~ treat0, data = data2)
regc3 <- lm(formula = otro0 ~ treat0, data = data2)
regc4 <- lm(formula = total0 ~ treat0, data = data2)

stargazer(regc0, regc1, regc2, regc3, regc4,
          type = "text", 
          style = "default", 
          se = list(get_robust_se(regc0), 
                    get_robust_se(regc1), 
                    get_robust_se(regc2),
                    get_robust_se(regc3), 
                    get_robust_se(regc4)))

# Second Decision no interaction
regc20 <- lm(share_p1 ~ treat00 + treat11, data = data2)
regc21 <- lm(formula = less1 ~ treat00 + treat11, data = data2)
regc22 <- lm(formula = min1 ~ treat00 + treat11, data = data2)
regc23 <- lm(formula = otro1 ~ treat00 + treat11, data = data2)
regc24 <- lm(formula = total1 ~ treat00 + treat11, data = data2)

stargazer(regc20, regc21,regc22, regc23, regc24,
          type = "text", 
          style = "default", 
          se = list(
            get_robust_se(regc20),
            get_robust_se(regc21),
            get_robust_se(regc22),
            get_robust_se(regc23),
            get_robust_se(regc24)))




