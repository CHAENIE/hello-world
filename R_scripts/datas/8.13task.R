setwd("c:/Rtest")
mycar <- read.csv("mycar.csv", header = TRUE)
library(Hmisc)
library(prettyR)
table(mycar$color)
table(mycar[2])
prop.table(table(mycar$color))
prop.table(table(mycar[2]))
round(prop.table(table(mycar[2]))*100,1)
surveyFreq <- c(table(mycar$color))
surveyProp <- c(round(prop.table(table(mycar[2]))*100,1))
surveytable <- data.frame(Freq=surveyFreq, Prop=surveyProp)
surveytable
describe(mycar)
describe(mycar$color)
freq(mycar)
freq(mycar$color)
chisq.test(surveyFreq)


mytooth <- read.csv("mytooth.csv", header = TRUE)
table(mytooth$buy)
table(mytooth[2])
prop.table(table(mytooth$buy))
round(prop.table(table(mytooth[2]))*100,2)
buyFreq <- c(table(mytooth$buy))
buyProp <- c(round(prop.table(table(mytooth[2]))*100,2))
buytable <- data.frame(Freq=buyFreq, Prop=buyProp)
buytable
describe(mytooth$buy)
describe(mytooth)
freq(mytooth)
freq(mytooth$buy)
binom.test(c(10,40), p=0.10)
binom.test(c(10,40), p=0.10, alter="less", conf.level = 0.95)
binom.test(c(10,40), p=0.10, alter="greater", conf.level = 0.95)
binom.test(c(10,40), p=0.10, alter="two.sided", conf.level = 0.95)


myheight <- read.csv("myheight.csv",header = TRUE)
myheight$height
mean(myheight$height)
range(myheight$height)
myheight5 <- subset(myheight, height!=999, c(height))
myheight5
mean(myheight5$height)
range(myheight5$height)
describe(myheight5)
shapiro.test(myheight5$height)
t.test(myheight5$height, mu=145.0)
t.test(myheight5$height, mu=145.0, alt="greater", conf.level = 0.95)
t.test(myheight5$height, mu=145.0, alt="less", conf.level = 0.95)
t.test(myheight5$height, mu=145.0, alt="two.sided", conf.level = 0.95)


mycf <- read.csv("mycf.csv", header = TRUE)
mycf
mycf$group
mycf$interest
mycf[c("group","interest")]
table(mycf$group)
table(mycf$interest)
table(mycf$group, mycf$interest)
prop.table(table(mycf$group, mycf$interest))
round(prop.table(table(mycf$group, mycf$interest))*100,1)
prop.test(c(13,27),c(50,50))
prop.test(c(13,27),c(50,50), alt="greater", conf.level = 0.95)
prop.test(c(13,27),c(50,50), alt="less", conf.level = 0.95)
prop.test(c(13,27),c(50,50), alt="two.sided", conf.level = 0.95)


mymethod <- read.csv("mymethod.csv", header = TRUE)
mymethod$method
mymethod$performance
groupA <- subset(mymethod, method==1 & performance<99)
groupB <- subset(mymethod, method==2 & performance<99)
groupAcount <- length(groupA$method)
groupAmean <- round(mean(groupA$performance),2)
groupAcount;groupAmean
groupBcount <- length(groupB$method)
groupBmean <- round(mean(groupB$performance),2)
groupBcount;groupBmean
groupcount <- c(groupAcount, groupBcount)
groupmean <- c(groupAmean, groupBmean)
groupcount;groupmean
grouptable <- data.frame(freq=groupcount, Prop=groupmean)
grouptable
shapiro.test(mymethod$performance)
var.test(groupA$performance, groupB$performance)
t.test(groupA$performance, groupB$performance, alternative = "two.sided", conf.int=TRUE, conf.level = 0.95)
t.test(groupA$performance, groupB$performance, alternative = "greater", conf.int=TRUE, conf.level = 0.95)
t.test(groupA$performance, groupB$performance, alternative = "less", conf.int=TRUE, conf.level = 0.95)


myeffect <- read.csv("myeffect.csv", header = TRUE)
myeffect
myeffect$before
myeffect$after
myeffect2 <- subset(myeffect, after<999)
myeffect3 <- subset(myeffect, after!=999)
myeffect4 <- subset(myeffect, after<999, c(before, after))
myeffect5 <- subset(myeffect, after!=999, c(before, after))
myeffect5
groupBE <- c(myeffect5$before)
groupAF <- c(myeffect5$after)
length(myeffect5$before)
length(myeffect5$after)
mean(myeffect5$before)
mean(myeffect5$after)
var.test(myeffect5$before, myeffect5$after, paired=TRUE)


library(XLConnect)
mystudy <- loadWorkbook("mystudy.xlsx", create = TRUE)
mydrink0 <- readWorksheet(mystudy, sheet = "mydrink", startRow = 1, startCol = 1, endRow = 11, endCol = 3)
mydrink0
a1 <- mydrink0$d1
a2 <- mydrink0$d2
a3 <- mydrink0$d3
z1 <- mean(a1)
z2 <- mean(a2)
z3 <- mean(a3)
z1;z2;z3
mystudy <- loadWorkbook("mystudy.xlsx", create = TRUE)
mydrink <- readWorksheet(mystudy, sheet = "mydrink", startRow = 14, startCol = 1, endCol = 4)
mydrink
mydrink2 <- mydrink[,-1]
mydrink2
fitdrink <- cmdscale(mydrink2, eig = TRUE, k=2)
fitdrink
x <- fitdrink$points[,1]
y <- fitdrink$points[,2]
plot(x,y,pch=19, xlim = c(-4,4), ylim = c(-2,2))
mydrinknames=c("콜라","포카리","게토레이")
text(x,y,pos = 3, labels = mydrinknames)
abline(h=0,v=0)

mycar <- read.csv("mycar.csv",header=TRUE)
library(Hmisc)
library(prettyR)
table(mycar$color)
prop.table(table(mycar$color))
round(prop.table(table(mycar$color))*100,1)
surveyFreq <- c(table(mycar$color))
surveyProp <- c(round(prop.table(table(mycar$color))*100,1))
surveytable <- data.frame(Freq=surveyFreq, Prop=surveyProp)
surveytable
describe(mycar$color)
freq(mycar)
freq(mycar$color)
chisq.test(surveyFreq)
chisq.test(freq(mycar$color))
surveyFreq

mysmoke <- readWorksheet(mystudy,sheet = "mysmoke")
mysmoke
table(mysmoke$education, mysmoke$smoking)
mysmoke$education2[mysmoke$education==1] <- "university"
mysmoke$education2[mysmoke$education==2] <- "highschool"
mysmoke$education2[mysmoke$education==3] <- "elementary"
mysmoke$smoking2[mysmoke$smoking==1] <- "many"
mysmoke$smoking2[mysmoke$smoking==2] <- "middle"
mysmoke$smoking2[mysmoke$smoking==3] <- "none"
mysmoke
library(plyr)
mysmoke$education3 <- mapvalues(mysmoke$education, from=c(1,2,3), to=c("university","highschool","elementary"))
mysmoke$smoking3 <- mapvalues(mysmoke$smoking, from=c(1,2,3), to=c("many","middle","none"))
mysmoke
table(mysmoke$education2, mysmoke$smoking2)
chisq.test(mysmoke$education2, mysmoke$smoking2)
mycf <- read.csv("mycf.csv",header=TRUE)
mycf$group
mycf$interest
mycf[c("group","interest")]
table(mycf$group)
table(mycf$interest)
table(mycf$group, mycf$interest)
prop.table(table(mycf$group, mycf$interest))
round(prop.table(table(mycf$group, mycf$interest))*100,1)
prop.test(c(37,23),c(50,50), alt="greater", conf.level = 0.95)


myheight <- read.csv("myheight.csv", header = TRUE)
myheight$height
mean(myheight$height)
range(myheight$height)
myheight2 <- subset(myheight, height!=999, c(height))
myheight2
mean(myheight2$height)
range(myheight2$height)
describe(myheight2)
describe(myheight2$height)
shapiro.test(myheight2$height)
t.test(myheight2$height, mu=145.0)
t.test(myheight2$height, mu=145.0, alt="greater", conf.level = 0.95)
t.test(myheight2$height, mu=145.0, alt="less", conf.level = 0.95)


myeffect <- read.csv("myeffect.csv", header=TRUE)
myeffect
myeffect$before
myeffect$after
myeffect2 <- subset(myeffect, after<999)
myeffect4 <- subset(myeffect, after<999, c(before, after))
myeffect4
groupBE <- c(myeffect4$before)
groupAF <- c(myeffect4$after)


setwd("c:/datas")
abroad <- read.csv("abroad.csv",header = TRUE)
abroad$score2[abroad$score<=999] <- "3"
abroad$score2[abroad$score<=700] <- "2"
abroad$score2[abroad$score<=500] <- "1"

abroad$score3[abroad$score >=0 & abroad$score<500] <- "1"
abroad$score3[abroad$score >=500 & abroad$score<700] <- "2"
abroad$score3[abroad$score >=700 & abroad$score<999] <- "3"
abroad$gender2[abroad$gender==1] <- "men"
abroad$gender2[abroad$gender==2] <- "women"
abroad$score4[abroad$score3 == 1] <- "250~500"
abroad$score4[abroad$score3 == 2] <- "501~750"
abroad$score4[abroad$score3 == 3] <- "751~990"
abroad
chisq.test(abroad$gender2, abroad$score4)
CrossTable(abroad$gender2, abroad$score4, expected=TRUE, format="SPSS")
abroad$score4

theater <- read.csv("theater.csv", header=TRUE)
theater$sati1
theater$sati2
theater$sati3
round(theater$sati,2)
theater$gender2[theater$gender==1] <- "men"
theater$gender2[theater$gender==2] <- "women"
theater$gender2
describe(theater$sati)
range(theater$sati)
theaterM <- subset(theater, gender == 1)
theaterM
theaterF <- subset(theater,gender==2)
theaterF

theaterMcount <- length(theaterM$gender)
theaterFcount <- length(theaterF$gender)
theaterMcount;theaterFcount
theaterMmean <- round(mean(theaterM$sati,2))
theaterFmean <- round(mean(theaterF$sati,2))
theaterMmean;theaterFmean
theatermean <- c(theaterMmean,theaterFmean)
theatercount <- c(theaterMcount,theaterFcount)
theatercount
theatermean

var.test(theaterM$sati, theaterF$sati)
t.test(theaterM$sati, theaterF$sati)



result <- read.csv("mytelecom.csv",header = TRUE)
result
View(result)
result2 <- lm(churn~tenure+income+age+gender, data=result)
result2
summary(result2)


library(psych)
library(dplyr)
reliability <- read.csv("theater.csv",header = TRUE)
reliability
satisfaction <- select(reliability, sati1, sati2, sati3)
alpha(satisfaction)


