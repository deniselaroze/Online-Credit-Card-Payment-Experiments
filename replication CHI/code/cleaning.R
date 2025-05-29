library(data.table)
library(tidyverse)

# location
setwd("/Users/elisaduranmicco/Dropbox/2103 - ANID - FONDEF Sernac/11.Pago/replication CHI")

data0 <- read.csv("./data/DataRaw.csv",na.strings=c('')) #3200

## CLEANING DATA

data0$prog <- as.numeric(as.character(data0$Progress))
data01 <- as.data.table(subset(data0, data0$treat!="NA")) # 2229 treatment assigned

# eliminate duplicated tickets, keeping the first answer
data02 <- as.data.table(subset(data01, duplicated(data01$ticket) == "FALSE")) #2177

## Select the experiment: (* activate for main experiment, ** activate for experiment with liquidity constraint)
# Main experiment
#data02 <- as.data.table(subset(data02, data02$saldo == "1000000")) # *
# Experiment with contraints
data02 <- as.data.table(subset(data02, data02$saldo == "211520")) # **

# respond both treatments
data <- as.data.table(subset(data02, data02$prog > 21))                  
data <- as.data.table(subset(data, data$P1_time._Click.Count!="NA")) 

### CREATING VARIABLES ###

# Attrition
data02$att <-  ifelse(data02$ticket %in% data$ticket, 0, 1)

# Socio-economic level
data$gse <- "ABC1"
data$gse[data$g == 4] <- "C2"
data$gse[data$g == 5] <- "C3"
data$gse[data$g >= 6] <- "DE"

# Finantial literacy
data$LC1 <- 0
data$LC1[data$LF1.=="Falso"] <- 1
data$LC2 <- 0
data$LC2[data$LF2.=="$100.000 recibidos hoy y luego colocados en una cuenta de ahorros a una tasa del 10% de interés compuesto anualmente"] <- 1
data$LC3 <- 0
data$LC3[data$LF3.=="$ 1.000"] <- 1
data$LC <- data$LC1 + data$LC2 + data$LC3

# treat 0
data$treat0 <- "C"
data[treat == "Tdeu", treat0 := "T"]
data[treat == "Tint", treat0 := "T"]
data$treat00 <-  ifelse(data$treat0 == "C", 0, 1)

# treat 1
data$treat1 <- "I"
data[treat == "Cdeu", treat1 := "D"]
data[treat == "Tdeu", treat1 := "D"]
data$treat11 <-  ifelse(data$treat1 == "I", 0, 1)

### Create Dependent Variables ###
# Time 0 

data$min <- as.numeric(data$min)
data$fac <- as.numeric(data$fac)

data$y0 <- data$min
data[Cnos == "Total a pagar", y0 := fac]
data$Cnosy0 <-  as.numeric(data$Cnos_3_TEXT)
data[Cnosy0 > 0, y0 := Cnosy0]
data$Cy0 <-  as.numeric(data$Cbajo_3_TEXT)
data[Cy0 > 0, y0 := Cy0]
data$op <-  "min"
data[Cnos == "Total a pagar", op := "total"]
data[Cnosy0 > 0, op := "otro"]
data[Cy0 > 0, op := "otro"]

data[Tnos. == "\nTotal a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.\n\n", y0 := fac]
data$Tnosy0 <-  as.numeric(data$Tnos._3_TEXT)
data[Tnosy0 > 0, y0 := Tnosy0]
data$Ty0 <-  as.numeric(data$Tbajo_3_TEXT)
data[Ty0 > 0, y0 := Ty0]
data[Tnos. == "\nTotal a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.\n\n", op := "total"]
data[Tnosy0 > 0, op := "otro"]
data[Ty0 > 0, op := "otro"]

data[y0 < 42304, op := "less"]

data$less0 <-  ifelse(data$op == "less", 1, 0)
data$min0 <-  ifelse(data$op == "min", 1, 0)
data$otro0 <-  ifelse(data$op == "otro", 1, 0)
data$total0 <-  ifelse(data$op == "total", 1, 0)

# Time 1

data$y1 <- data$min

data$Cnos_deu_fac._4_TEXT <- as.numeric(data$Cnos_deu_fac._4_TEXT)
data$Tnos_deu_fac._4_TEXT <- as.numeric(data$Tnos_deu_fac._4_TEXT)
data$Cnos_deu._4_TEXT <- as.numeric(data$Cnos_deu._4_TEXT)
data$Cbajo_deu._4_TEXT <- as.numeric(data$Cbajo_deu._4_TEXT)
data$Tnos_deu._4_TEXT <- as.numeric(data$Tnos_deu._4_TEXT)
data$Tbajo_deu._4_TEXT <- as.numeric(data$Tbajo_deu._4_TEXT)
data$Cnos_deu_min._4_TEXT <- as.numeric(data$Cnos_deu_min._4_TEXT)
data$Cbajo_deu_min._4_TEXT <- as.numeric(data$Cbajo_deu_min._4_TEXT)
data$Tnos_deu_min._4_TEXT <- as.numeric(data$Tnos_deu_min._4_TEXT)
data$Tbajo_deu_min._4_TEXT <- as.numeric(data$Tbajo_deu_min._4_TEXT)
data$Cnos_deu.min._4_TEXT <- as.numeric(data$Cnos_deu.min._4_TEXT)
data$Cbajo_deu.min._4_TEXT <- as.numeric(data$Cbajo_deu.min._4_TEXT)
data$Tnos_deu.min._4_TEXT <- as.numeric(data$Tnos_deu.min._4_TEXT)
data$Tbajo_deu.min._4_TEXT <- as.numeric(data$Tbajo_deu.min._4_TEXT)
data$Cnos_int_fac._4_TEXT <- as.numeric(data$Cnos_int_fac._4_TEXT)
data$Tnos_int_fac._4_TEXT <- as.numeric(data$Tnos_int_fac._4_TEXT)
data$Cnos_int._20_TEXT <- as.numeric(data$Cnos_int._20_TEXT)
data$Cbajo_int._20_TEXT <- as.numeric(data$Cbajo_int._20_TEXT)
data$Tnos_int._20_TEXT <- as.numeric(data$Tnos_int._20_TEXT)
data$Tbajo_int._20_TEXT <- as.numeric(data$Tbajo_int._20_TEXT)
data$Cnos_int_min._4_TEXT <- as.numeric(data$Cnos_int_min._4_TEXT)
data$Cbajo_int_min._4_TEXT <- as.numeric(data$Cbajo_int_min._4_TEXT)
data$Tnos_int_min._4_TEXT <- as.numeric(data$Tnos_int_min._4_TEXT)
data$Tbajo_int_min._4_TEXT <- as.numeric(data$Tbajo_int_min._4_TEXT)
data$Cnos_int.min._4_TEXT <- as.numeric(data$Cnos_int.min._4_TEXT)
data$Cbajo_int.min._4_TEXT <- as.numeric(data$Cbajo_int.min._4_TEXT)
data$Tnos_int.min._4_TEXT <- as.numeric(data$Tnos_int.min._4_TEXT)
data$Tbajo_int.min._4_TEXT <- as.numeric(data$Tbajo_int.min._4_TEXT)

data[Cnos_deu_fac. == "Total a pagar", y1 := fac]
data[Cnos_deu_fac._4_TEXT > 0, y1 := Cnos_deu_fac._4_TEXT]
data[Tnos_deu_fac. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_deu_fac._4_TEXT > 0, y1 := Tnos_deu_fac._4_TEXT]

data[Cnos_deu. == "Total a pagar", y1 := fac]
data[Cnos_deu._4_TEXT > 0, y1 := Cnos_deu._4_TEXT] 
data[Cbajo_deu._4_TEXT > 0, y1 := Cbajo_deu._4_TEXT]
data[Tnos_deu. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_deu._4_TEXT > 0, y1 := Tnos_deu._4_TEXT]
data[Tbajo_deu._4_TEXT > 0, y1 := Tbajo_deu._4_TEXT]

data[Cnos_deu_min. == "Total a pagar", y1 := fac]
data[Cnos_deu_min._4_TEXT > 0, y1 := Cnos_deu_min._4_TEXT]
data[Cbajo_deu_min._4_TEXT > 0, y1 := Cbajo_deu_min._4_TEXT]
data[Tnos_deu_min. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_deu_min._4_TEXT > 0, y1 := Tnos_deu_min._4_TEXT]
data[Tbajo_deu_min._4_TEXT > 0, y1 := Tbajo_deu_min._4_TEXT]

data[Cnos_deu.min. == "Total a pagar", y1 := fac]
data[Cnos_deu.min._4_TEXT > 0, y1 := Cnos_deu.min._4_TEXT]
data[Cbajo_deu.min._4_TEXT > 0, y1 := Cbajo_deu.min._4_TEXT]
data[Tnos_deu.min. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_deu.min._4_TEXT > 0, y1 := Tnos_deu.min._4_TEXT]
data[Tbajo_deu.min._4_TEXT > 0, y1 := Tbajo_deu.min._4_TEXT]

data[Cnos_int_fac. == "Total a pagar", y1 := fac]
data[Cnos_int_fac._4_TEXT > 0, y1 := Cnos_int_fac._4_TEXT]
data[Tnos_int_fac. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_int_fac._4_TEXT > 0, y1 := Tnos_int_fac._4_TEXT]

data[Cnos_int. == "Total a pagar", y1 := fac]
data[Cnos_int._20_TEXT > 0, y1 := Cnos_int._20_TEXT] 
data[Cbajo_int._20_TEXT > 0, y1 := Cbajo_int._20_TEXT]
data[Tnos_int. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_int._20_TEXT > 0, y1 := Tnos_int._20_TEXT]
data[Tbajo_int._20_TEXT > 0, y1 := Tbajo_int._20_TEXT]

data[Cnos_int_min. == "Total a pagar", y1 := fac]
data[Cnos_int_min._4_TEXT > 0, y1 := Cnos_int_min._4_TEXT]
data[Cbajo_int_min._4_TEXT > 0, y1 := Cbajo_int_min._4_TEXT]
data[Tnos_int_min. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_int_min._4_TEXT > 0, y1 := Tnos_int_min._4_TEXT]
data[Tbajo_int_min._4_TEXT > 0, y1 := Tbajo_int_min._4_TEXT]

data[Cnos_int.min. == "Total a pagar", y1 := fac]
data[Cnos_int.min._4_TEXT > 0, y1 := Cnos_int.min._4_TEXT]
data[Cbajo_int.min._4_TEXT > 0, y1 := Cbajo_int.min._4_TEXT]
data[Tnos_int.min. == "Total a pagar\nAl pagar el total, evitarás intereses adicionales y gastos de cobranza.", y1 := fac]
data[Tnos_int.min._4_TEXT > 0, y1 := Tnos_int.min._4_TEXT]
data[Tbajo_int.min._4_TEXT > 0, y1 := Tbajo_int.min._4_TEXT]

data$y1 <- as.numeric(data$y1)

data$op2 <-  "min"
data[y1 != min, op2 := "otro"]
data[y1 == fac, op2 := "total"]
data[y1 < 42304, op2 := "less"]

# share pay 0
data$share_p0 <- data$y0/282026 
# share pay 1
data$share_p1 <- data$y1/282026 

data$less1 <-  ifelse(data$op2 == "less", 1, 0)
data$min1 <-  ifelse(data$op2 == "min", 1, 0)
data$otro1 <-  ifelse(data$op2 == "otro", 1, 0)
data$total1 <-  ifelse(data$op2 == "total", 1, 0)

### Save data (* activate for main experiment, ** activate for experiment with liquidity constraint)
write.csv(data, "DataEx1.csv") # *
#write.csv(data, "DataEx2.csv") # **
