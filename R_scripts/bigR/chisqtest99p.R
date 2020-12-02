install.packages("psych")
library(psych)
setwd("c:/bigR")
practice <- read.csv("practice.csv", header=TRUE)
describe(practice)
install.packages("pastecs")
library(pastecs)
stat.desc(practice)


#카이스퀘어 검정 & 교차분석 (99p~)
#1) 적합도 검정
service <- read.csv("service.csv",header = TRUE)
service
library(Hmisc)
library(prettyR)
table(service)
table(service[1])
prop.table(table(service[1]))
prop.table(table(service[1]))*100
round(prop.table(table(service[1]))*100,1)
serviceFreq <- c(table(service[1]))
serviceFreq
serviceProp <- c(round(prop.table(table(service[1]))*100,1))
serviceProp
servicetable <- data.frame(Freq=serviceFreq, Prop=serviceProp)
servicetable

describe(service)
freq(service)

chisq.test(serviceFreq)


#2) 독립성 검정 문제(교차분석)
install.packages("xlsx")
library(xlsx)

carS <- read.xlsx("study.xlsx", sheetName = "carS")

table(carS$family, carS$carsize)

carS$family2[carS$family==1] <- "1~2"
carS$family2[carS$family==2] <- "3~4"
carS$family2[carS$family==3] <- "5~6"
carS$carsize2[carS$carsize==1] <- "small"
carS$carsize2[carS$carsize==2] <- "medium"
carS$carsize2[carS$carsize==3] <- "large"
carS

library(plyr)
carS$family3 <- mapvalues(carS$family, from=c(1,2,3), to=c("1~2","3~4","5~6"))
carS$carsize3 <- mapvalues(carS$carsize, from=c(1,2,3), to=c("small","medium","large"))
carS

table(carS$family2, carS$carsize2)

chisq.test(carS$family2, carS$carsize2)

install.packages("gmodels")
library(gmodels)

CrossTable(carS$family2, carS$carsize2, expected = TRUE, format = "SPSS")
CrossTable(carS$family2, carS$carsize2, expected = TRUE, format = "SAS")
