setwd("c:/datas")
library(Hmisc)
library(prettyR)
library(psych)
library(car)
library(agricolae)
library(gmodels)
library(dplyr)
#1
service <- read.csv("service.csv",header = T)

table(service) #%%
prop.table(table(service))
round(prop.table(table(service))*100,1)

Freq <- c(table(service))
freq(service)
describe(service)
chisq.test(Freq)
#p-value가 0.05보다 크므로 대립가설 기각. 요일별 a/s건수에 통계적으로 유의한 차이가 없다.

#2
carS <- read.csv("carS.csv", header=T)
carS$family2[carS$family==1] <- "1~2명"
carS$family2[carS$family==2] <- "3~4명"
carS$family2[carS$family==3] <- "5~6명"
carS$carsize2[carS$carsize==1] <- "small"
carS$carsize2[carS$carsize==2] <- "medium"
carS$carsize2[carS$carsize==3] <- "large"

table(carS$family2, carS$carsize2)
chisq.test(carS$family2, carS$carsize2)
CrossTable(carS$family2, carS$carsize2, expected = T, format = "SPSS")
#p-value가 0.05보다 작으므로 대립가설 채택. 가족구성원의 크기에 따라 자동차크기에 통계적으로 유의한 차이가 있다.

#3
theater <- read.csv("theater.csv", header = T)
theater$satis <- (theater$sati1+theater$sati2+theater$sati3)/3
theater$satis

groupM <- subset(theater, gender==1)
groupF <- subset(theater, gender==2)
groupM$satis
groupF$satis

var.test(groupM$satis,groupF$satis,data=theater)
#p-value가 0.05보다 크므로 집단간 등분산 충족->t.test
t.test(groupM$satis,groupF$satis,data=theater, alt="two.sided", conf.int=TRUE, conf.level = 0.95)
t.test(groupM$satis,groupF$satis,data=theater, alt="greater", conf.int=TRUE, conf.level = 0.95)
t.test(groupM$satis,groupF$satis,data=theater, alt="less", conf.int=TRUE, conf.level = 0.95)

#검정결과, p-value가0.05보다 작으므로 대립가설 기각.
#성별에 따라 영화관환경만족도 평균에 통계적으로 유의한 차이가 있다. 여성이 남성보다 영화관환경만족도가 통계적으로 높다

#4
rating <- read.csv("rating.csv",header = T)

groupBE <- c(rating$berating)
groupAF <- c(rating$afrating)
groupBE
groupAF

describe(rating)

var.test(rating$berating,rating$afrating, paired=TRUE)
#집단간등분산충족하지않음 -> wilcox.test
wilcox.test(rating$berating,rating$afrating, paired=TRUE, alt="two.sided", conf.int = T, conf.level = 0.95)
#p-value가0.05보다작으므로대립가설채택.
#영화관람전,후평점에통계적으로유의한차이가있다. 영화관람후 평점이 전 평점보다 통계적으로 유의하게 높다

#5
grape <- read.csv("grape.csv", header = T)

grape.lm <- lm(price~size+period, data=grape)
summary(grape.lm)

vif(grape.lm)
#vif값이 모두 1점대로 4보다 낮아 다중공선성이 낮다. 따라서 독립변수간 관계가 적다.
#독립변수를 그대로 사용할 수 있다
summary(grape.lm)
#Adjusted R-squared:  0.8853/ R-squared값이 0.88로, 종속변수분산의 88%가 독립변수에 의해 설명된다(설명력)
#p-value가 0.05보다 작으므로 대립가설이 채택. 포도의 크기와 기간이 포도의 가격에 영향을 미친다. 
#추정값이 +값을 갖기 때문에 포도의 크기와 기간이 증가할수록 포도의 가격도 상승한다.

#6
jobedu <- read.csv("jobedu.csv", header = T)

tapply(jobedu$performance, jobedu$method, shapiro.test)
is.na(jobedu$performance)

jobedu$performance <- ifelse(jobedu$performance==99, NA, jobedu$performance)
jobedu$performance
jobedu2 <- jobedu %>% filter(!is.na(jobedu$performance))
jobedu2$performance
tapply(jobedu2$performance, jobedu2$method, shapiro.test)
#p-value가 0.05보다 크므로 정규성충족
bartlett.test(jobedu2$performance, jobedu2$method)
#p-value가0.05보다 크므로 집단 간 등분산 충족 (정규성 및 등분산성 충족) -> one-way 분산분석
job.lm <- lm(performance~method, data=jobedu2)
anova(job.lm)
#p-value가 0.05보다 작으므로 대립가설을 채택. 직업교육방법에 따라 영업실적 평균에 차이가 있다
model <- aov(performance~method,jobedu2)
comparison <- LSD.test(model, "method", p.adj = "bonferroni", group = T)
comparison
#각 집단별 평균차이를 확인할 수 있다

#chisq.test practice
#1
coffee <- read.csv("coffee.csv", header = T)

coffee$choice
table(coffee$choice)
round(prop.table(table(coffee$choice))*100,2)

freq(coffee$choice)
coffeetable <- data.frame(Freq=c(table(coffee$choice)),Prop=c(round(prop.table(table(coffee$choice))*100,2)))
coffeetable
coffeeFreq <- c(table(coffee$choice))
chisq.test(coffeeFreq)
#p-value가 0.05보다 작으므로 선택대안별로 통계적으로 유의한 차이가 있다

#2
abroad <- read.csv("abroad.csv", header = T)
abroad$gender
abroad$score
abroad$score2[abroad$score<400] <- "~400미만"
abroad$score2[abroad$score<600 & abroad$score>=400] <- "400이상~600미만"
abroad$score2[abroad$score<800 & abroad$score>=600] <- "600이상~800미만"
abroad$score2[abroad$score<1000 & abroad$score>=800] <- "800이상~999"
abroad$score2

table(abroad$gender, abroad$score2)
chisq.test(abroad$gender,abroad$score2)
CrossTable(abroad$gender,abroad$score2, expected = TRUE, format = "SPSS")
#p-value가 0.05보다 작으므로 귀무가설 기각,대립가설 채택 
#성별에 따라 토익점수에 통계적으로 유의한 차이가 있다.

#3
abroad$experience

table(abroad$experience,abroad$score2)
chisq.test(abroad$experience,abroad$score2)
#p-value가보다 크므로 귀무가설 채택,대립가설 기각
#해외연수경험에 따른 토익점수에 통계적으로 유의한 차이가 없다

#t.test practice
#1(two-sample t.test)
business <- read.csv("business.csv", header = T)

business$성별;business$학점
groupM <- subset(business,business$성별==1)
groupF <- subset(business, business$성별==2)

var.test(groupM$학점, groupF$학점)
#p-value가 0.05보다 크므로 등분산 충족 -> t.test
t.test(groupM$학점, groupF$학점, alt="two.sided", conf.int=TRUE, conf.level=0.95)
#p-value가 0.05보다 크므로 대립가설기각.
#성별에 따라 학점에 유의한 차이가 없다.

#2(paired sample)
diet <- read.csv("diet.csv", header = T)

diet$before
diet$after

diet2 <- subset(diet, diet$after<999)
diet2$after

# groupBE <- c(diet2, diet2$before)
# groupAF <- c(diet2, diet2$after)
var.test(diet2$before, diet2$after, paired=TRUE)
#p-value가 0.05보다 작으므로 등분산 충족하지 않음->wilcox
wilcox.test(diet2$before, diet2$after, paired=TRUE, conf.int=TRUE, conf.level=0.95)
#p-value가 0.05보다 작으므로 대립가설 채택.
#다이어트약복용 전과 후의 몸무게에 통계적으로 유의한 차이가 있다

#다중회귀 practice
#1
sales <- read.csv("sales.csv", header = T)

str(sales)

sales.lm <- lm(profit~rd+ad+promotion,data=sales)
summary(sales.lm)

vif(sales.lm)
#promotion, rd의 vif값이 10 이상으로 다중공선성이 심각한수준. 독립변수간 관계성이 강하므로 둘중하나 제거필요
#유의한 p-value값이 나온 promotion이 아닌 rd를 제거
sales2.lm <- lm(profit~ad+promotion,data=sales)
summary(sales2.lm)
vif(sales2.lm)
#p-value가 0.0313으로 0.05보다 작으므로 대립가설 채택
#Adjusted R-squared:  0.1003 /R-squared값이 0.10으로 10%의 설명력을 갖는다 
#promotion이 


#1
library(Hmisc)
library(prettyR)
library(dplyr)
library(car)
library(psych)

service <- read.csv("service.csv", header = T)

table(service)
chisq.test(table(service))


#weight
setwd("c:/MBDM1")

weight <- read.csv("weight.csv", header = T)

weight$before
weight$after

var.test(weight$before,weight$after, paired=TRUE)
wilcox.test(weight$before,weight$after, paired=TRUE, conf.int = TRUE, conf.level = 0.95)

#cartype
cartype <- read.csv("cartype.csv", header = T)

table(cartype$group,cartype$type)
chisq.test(cartype$group,cartype$type)

#electronics
eletronics <- read.csv("eletronics.csv", header = T)

eletronics$satis <- (eletronics$satisfaction1+eletronics$satisfaction2+eletronics$satisfaction3)/3
eletronics$satis

brand1 <- subset(eletronics,brand==1)
brand2 <- subset(eletronics,brand==2)

brand1$satis
brand2$satis

var.test(brand1$satis, brand2$satis)

t.test(brand1$satis, brand2$satis, alt="two.sided", conf.int=T, conf.level = 0.95)

#restarant
restaurant <- read.csv("restaurant.csv", header = T)

restaurant.lm <- lm(sales~raw+time+price+ad+guest, data=restaurant)
summary(restaurant.lm)
vif(restaurant.lm)

