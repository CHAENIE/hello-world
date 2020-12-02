#111p No.1
library(Hmisc)
library(prettyR)
coffee <- read.csv("coffee.csv",header=TRUE)
coffee
coffee[3]
table(coffee)
coffee$choice <- mapvalues(coffee$chioce, from=c(1,2,3,4), to=c("size","position","kindness","discount"))
coffee
prop.table(table(coffee[3]))
prop.table(table(coffee[3]))*100
round(prop.table(table(coffee[3]))*100,1)
coffeeFreq <- c(table(coffee[3]))
coffeeFreq
coffeeProp <- c(round(prop.table(table(coffee[3]))*100,1))
coffeeProp
coffeetable <- data.frame(Freq = coffeeFreq, Prop = coffeeProp)
coffeetable
describe(coffee[3])
chisq.test(coffeeFreq)


#No.2
abroad <- read.csv("abroad.csv", header = TRUE)
abroad
abroad$toeic2[abroad$toeic <= 999] <- "3"
abroad$toeic2[abroad$toeic <= 700] <- "2"
abroad$toeic2[abroad$toeic <= 500] <- "1"
abroad$toeic3[abroad$toeic >=0 & abroad$toeic<500] <- "1"
abroad$toeic3[abroad$toeic >=500 & abroad$toeic<700] <- "2"
abroad$toeic3[abroad$toeic >=700 & abroad$toeic<999] <- "3"
abroad$sex2[abroad$sex==1] <- "men"
abroad$sex2[abroad$sex==2] <- "women"
abroad$toeic2[abroad$toeic == 1] <- "250~500"
abroad$toeic2[abroad$toeic == 2] <- "501~750"
abroad$toeic2[abroad$toeic == 3] <- "751~990"
chisq.test(abroad$sex2,abroad$toeic2)
CrossTable(abroad$sex2, abroad$toeic2, expected = TRUE, format = "SPSS")


#No.3
abroad <- read.csv("abroad.csv", header = TRUE)
abroad$experience3 <- mapvalues(abroad$experience, from=c(1,2), to=c("do","not"))
abroad$toeic4 <- mapvalues(abroad$toeic2, from=c(1,2,3), to=c("250~500","501~750","751~990"))
abroad
table(abroad$experience3, abroad$toeic4)
chisq.test(abroad$experience3, abroad$toeic4)
library(gmodels)
CrossTable(abroad$experience3, abroad$toeic4, expected = TRUE, format = "SPSS")
