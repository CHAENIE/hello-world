#112p 단일집단분석 - 이항분포검정
smoke <- read.csv("smoke.csv",header = TRUE)
library(Hmisc)
library(prettyR)
library(psych)

table(smoke$success)
prop.table(table(smoke$success))
prop.table(table(smoke$success))*100
round(prop.table(table(smoke$success))*100,1)
smokeFreq <- c(table(smoke$success))
smokeProp <- c(round(prop.table(table(smoke$success))*100,1))               
smokeFreq
smokeProp
smoketable <- data.frame(Freq=smokeFreq, Prop=smokeProp)
smoketable
describe(smoke$success)
freq(smoke$success)
binom.test(c(16,44),p=0.10)
binom.test(c(16,44),p=0.15, alter="greater", conf.level = 0.95)
binom.test(c(16,44),p=0.15, alter="less", conf.level = 0.95)
binom.test(c(16,44),p=0.15, conf.level = 0.95)

#두집단비율차이검정
museum <- read.csv("museum.csv", header = TRUE)
museum[c("group","visit")]
table(museum$group)
table(museum$visit)
table(museum$group, museum$visit)
prop.table(table(museum$group, museum$visit))
round(prop.table(table(museum$group, museum$visit))*100,0)

prop.test(c(20,34),c(100,100))
prop.test(c(20,34),c(100,100), alternative = "greater", conf.level = 0.95)
prop.test(c(20,34),c(100,100), alternative = "less", conf.level = 0.95)


#122p 비율검정연습문제 no.1
factory <- read.csv("factory.csv",header = TRUE)
factory[c("new","faulty")]
table(factory$new)
table(factory$faulty)
table(factory$new, factory$faulty)
prop.table(table(factory$new, factory$faulty))
round(prop.table(table(factory$new, factory$faulty))*100,1)

prop.test(c(17,37),c(100,100))
prop.test(c(17,37),c(100,100),alter="greater",conf.level = 0.95)
prop.test(c(17,37),c(100,100),alter="less",conf.level = 0.95)


#122p 비율검정연습문제 no.2
cf <- read.csv("cf.csv",header=TRUE)
cf[c("group","interest")]
table(cf$group)
table(cf$interest)
table(cf$group, cf$interest)
prop.table(table(cf$group, cf$interest))
round(prop.table(table(cf$group, cf$interest))*100,0)

prop.test(c(13,27),c(100,100))
prop.test(c(13,27),c(100,100),alter="greater",conf.level = 0.95)
prop.test(c(13,27),c(100,100),alter="less",conf.level = 0.95)
