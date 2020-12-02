#7장 1절
setwd("c:/Rtest")
mycar <- read.csv("mycar.csv",header = TRUE)
install.packages("Hmisc")
library(Hmisc)
install.packages("prettyR")
library(prettyR)

table(mycar$color)
table(mycar[2])

prop.table(table(mycar$color))
prop.table(table(mycar[2]))
prop.table(table(mycar[2]))*100
round(prop.table(table(mycar[2]))*100,1)
surveyFreq <- c(table(mycar$color))
surveyProp <- c(round(prop.table(table(mycar$color))*100,1))
surveytable <- data.frame(Freq=surveyFreq, Prop=surveyProp)
surveytable

describe(mycar)
describe(mycar$color)
freq(mycar)
freq(mycar$color)
chisq.test(surveyFreq)


#7장 2절
setwd("c:/Rtest")
mytooth <- read.csv("mytooth.csv",header = TRUE)
library(Hmisc)
library(prettyR)
table(mytooth$buy)
table(mytooth[2])

prop.table(table(mytooth$buy))
prop.table(table(mytooth[2]))
prop.table(table(mytooth[2]))*100
round(prop.table(table(mytooth[2]))*100,2)
buyFreq <- c(table(mytooth$buy))
buyFreq
buyProp <- c(round(prop.table(table(mytooth$buy))*100,1))
buyProp
buytable <- data.frame(Freq=buyFreq,Prop=buyProp)
buytable

describe(mytooth)
describe(mytooth$buy)
freq(mytooth)
freq(mytooth$buy)

binom.test(c(10,40),p=0.20)
binom.test(c(10,40),p=0.20, alternative = "two.sided", conf.level = 0.95)


#7장 3절
myheight <- read.csv("myheight.csv", header = TRUE)
myheight$height
mean(myheight$height)
range(myheight$height)
table(myheight$height)
table(myheight[2])

myheight5 <- subset(myheight, height!=999, c(height))
myheight5
mean(myheight5$height)
range(myheight5$height)
describe(myheight5)
describe(myheight$height)
shapiro.test(myheight5$height)
t.test(myheight5$height, mu = 145.0)
t.test(myheight5$height, mu = 145.0, alternative = "greater", conf.level = 0.95)



#7장 4절
mycf <- read.csv("mycf.csv",header=TRUE)
mycf
mycf$group
mycf$interest
mycf[c("group","interest")]
table(mycf$group)
table(mycf$interest)
table(mycf$group,mycf$interest)

prop.table(table(mycf$group, mycf$interest))
prop.table(table(mycf$group, mycf$interest))*100
round(prop.table(table(mycf$group, mycf$interest))*100,1)
prop.test(c(13,27),c(50,50))
prop.test(c(13,27),c(50,50), alter="less", conf.level = 0.95)


#7장 5절
mymethod <- read.csv("mymethod.csv",header=TRUE)
mymethod$method
mymethod$performance
groupA <- subset(mymethod, method == 1 & performance<99)
groupB <- subset(mymethod, method == 2 & performance<99)
groupAcount <- length(groupA$method)
groupAmean <- round(mean(groupA$performance),2)
groupBcount <- length(groupB$method)
groupBmean <- round(mean(groupB$performance),2)
groupAcount;groupAmean
groupBcount;groupBmean

groupcount <- c(groupAcount, groupBcount)
groupmean <- c(groupAmean, groupBmean)
groupcount;groupmean
grouptable <- data.frame(Freq=groupcount, Mean=groupmean)
grouptable
var.test(groupA$performance,groupB$performance)
var.test(groupA$method,groupB$method)

t.test(groupA$performance, groupB$performance, alter="two.sided", conf.level = 0.95, conf.int=TRUE)
t.test(groupA$performance, groupB$performance, alter="greater", conf.level = 0.95, conf.int=TRUE)
t.test(groupA$performance, groupB$performance, alter="less", conf.level = 0.95, conf.int=TRUE)


#7장 6절
myeffect <- read.csv("myeffect.csv", header=TRUE)
myeffect
myeffect$before
myeffect$after
myeffect2 <- subset(myeffect, after<999)
myeffect3 <- subset(myeffect, after!=999)
myeffect4 <- subset(myeffect, after<999, c(before,after))
myeffect5 <- subset(myeffect, after!=999,c(before, after))
myeffect2
myeffect3
myeffect4
myeffect5
groupBE <- c(myeffect5$before)
groupAF <- c(myeffect5$after)

length(myeffect5$before)
length(myeffect5$after)
mean(myeffect5$before)
mean(myeffect5$after)

var.test(myeffect5$before,myeffect5$after, paired=TRUE)
wilcox.test(groupBE, groupAF, paired = TRUE)
wilcox.test(groupBE, groupAF, paired = TRUE, alter="two.sided", conf.int = TRUE, conf.level = 0.95)


#7장 7절
install.packages("XLConnect")
library(XLConnect)

mystudy <- loadWorkbook("mystudy.xlsx", create = TRUE)
mysmoke <- readWorksheet(mystudy, sheet = "mysmoke")
mysmoke
table(mysmoke$education, mysmoke$smoking)

mysmoke$education2[mysmoke$education==1] <- "university"
mysmoke$education2[mysmoke$education==2] <- "highschool"
mysmoke$education2[mysmoke$education==3] <- "elementary"
mysmoke$smoking2[mysmoke$smoking==1] <- "many"
mysmoke$smoking2[mysmoke$smoking==2] <- "middle"
mysmoke$smoking2[mysmoke$smoking==3] <- "none"
mysmoke

install.packages("plyr")
library(plyr)

mysmoke$education3 <- mapvalues(mysmoke$education,
                                from=c(1,2,3), to=c("university","highschool","elementary"))
mysmoke$smoking3 <- mapvalues(mysmoke$smoking,
                              from = c(1,2,3), to=c("many","middle","none"))
mysmoke
table(mysmoke$education2, mysmoke$smoking2)
chisq.test(mysmoke$education2, mysmoke$smoking2)


#7장 8절
mystudy <- loadWorkbook("mystudy.xlsx",create = TRUE)
mydrink0 <- readWorksheet(mystudy, sheet = "mydrink", startRow = 1, startCol = 1, endRow = 11, endCol = 3)
mydrink0

a1 <- mydrink0$d1
a2 <- mydrink0$d2
a3 <- mydrink0$d3
z1 <- mean(a1)
z2 <- mean(a2)
z3 <- mean(a3)
z1;z2;z3

mystudy <- loadWorkbook("mystudy.xlsx",create = TRUE)
mydrink <- readWorksheet(mystudy, sheet = "mydrink", startRow = 14, startCol = 1, endCol = 4)
mydrink
mydrink2 <- mydrink[,-1]
mydrink2
fitdrink <- cmdscale(mydrink2, eig = TRUE, k=2)
fitdrink
x <- fitdrink$points[,1]
y <- fitdrink$points[,2]
plot(x, y, pch=19, xlim = c(-4,4), ylim = c(-2,2))
mydrinknames=c("콜라","포카리","게토레이")
text(x,y, pos=3, labels=mydrinknames)
abline(h=0, v=0)
