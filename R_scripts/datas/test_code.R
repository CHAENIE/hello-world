setwd("c:/datas")
library(car)
library(Hmisc)
library(prettyR)
library(psych)
library(agricolae)
library(gmodels)
#library(dplyr) <- filter%>% 쓰는것
###chisq.test-적합도검정###
service <- read.csv("service.csv",header=T)

table(service)
round(prop.table(table(service))*100,1)

serviceFreq <- c(table(service))
serviceProp <- c(round(prop.table(table(service))*100,1))
serviceFreq;serviceProp
servicetable <- data.frame(Freq=serviceFreq, Prop=serviceProp)
servicetable

describe(service)
freq(service)
chisq.test(serviceFreq)
# chisq.test(table(service))
# chisq.test(c(table(service)))
# 검정결과, p-value(유의확률)가 0.05보다 크므로 대립가설(요일에따라 A/S빈도에 차이가 있다) 기각,
# 귀무가설 채택. 요일별 A/S처리 건수에는 차이가 없다.

###chisq.test-교차분석###
carS <- read.csv("carS.csv", header = T)
carS$family2[carS$family==1] <- "1~2명"
carS$family2[carS$family==2] <- "3~4명"
carS$family2[carS$family==3] <- "5~6명"
carS$carsize2[carS$carsize==1] <- "small"
carS$carsize2[carS$carsize==2] <- "medium"
carS$carsize2[carS$carsize==3] <- "large"

carS
freq(carS)
table(carS$family2, carS$carsize2)
chisq.test(carS$family2,carS$carsize2)

CrossTable(carS$family2, carS$carsize2, expected = T, format = "SPSS")
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택. 가족구성원 크기에 따라 차의 크기에 차이가 있다.

###t.test-독립표본(two-samples)###
promotion <- read.csv("promotion.csv", header = T)
table(promotion$event2, promotion$brand)

groupA <- subset(promotion,event==1 & brand<999)
groupB <- subset(promotion,event==2 & brand<999)

groupAcount <- length(groupA$event)
groupAmean <- round(mean(groupA$brand),2)
groupAcount;groupAmean
groupBcount <- length(groupB$event)
groupBmean <- round(mean(groupB$brand),2)
groupBcount;groupBmean

groupcount <- c(groupAcount,groupBcount)
groupmean <- c(groupAmean, groupBmean)
grouptable <- data.frame(Freq=groupcount, Mean=groupmean)
grouptable
# 검정통계량 계산 전, 두 모집단 분산의 동질성 여부 판단(분산의 동질성)
var.test(groupA$brand, groupB$brand)
# p-value가 0.05보다 크면 두집단 분산 동질적 -> t-test
t.test(groupA$brand, groupB$brand, alter="two.sided", conf.int=TRUE, conf.level = 0.95)
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택. 
# 이벤트 프로모션 참가여부에 따라 브랜드평가에 차이가 있다. 
# 이벤트 프로모션 참가집단이 참가하지않은 집단보다 브랜드평가가 통계적으로 유의하게 높다.


###t.test-독립표본(two-samples)###
theater <- read.csv("theater.csv", header = T)

theater$satis <- (theater$sati1+theater$sati2+theater$sati3)/3
theater$satis

groupM <- subset(theater, gender==1)
groupF <- subset(theater, gender==2)
groupM;groupF

var.test(groupM$satis, groupF$satis)
# p-value가 0.05보다 크므로 두집단 분산 동질적 -> t.test
t.test(groupM$satis, groupF$satis, alter="two.sided", conf.int=TRUE, conf.level = 0.95)
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택
# 성별에 따라 환경만족도에 차이가 있다
# 여성이 남성보다 환경만족도가 통계적으로 유의하게 높다.


###t.test-대응표본(paired samples)###
rating <- read.csv("rating.csv", header = T)

describe(rating$berating)
describe(rating$afrating)

ratingBE <- c(rating$berating)
ratingAF <- c(rating$afrating)
ratingBE;ratingAF

var.test(rating$berating,rating$afrating, paired=TRUE)
# p-value가 0.05보다 작으므로 두집단 분산 동질적이지 않다 -> wilcox.test
wilcox.test(ratingBE,ratingAF, paired=TRUE, alt="two.sided", conf.int=TRUE, conf.level = 0.95)
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택
# 영화보기 전 평점보다 영화본 후 평점이 통계적으로 유의하게 높다.


###다중회귀분석###
grape <- read.csv("grape.csv", header = T)
grape$size
grape$period
grape$price #NA 존재
str(grape)

grape.lm <- lm(price~size+period, data=grape)
summary(grape.lm)
#price NA 제거
is.na(grape)
grape_na <- na.omit(grape)

grape_na.lm <- lm(price~size+period, data=grape_na)
summary(grape_na.lm)

vif(grape_na.lm)
# vif값이 1점대로, 4점보다 낮아 다중공선성이 거의 없다.
# 따라서 독립변수 간 상관관계가 거의 없으므로 독립변수를 제거하지 않는다.
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택
# 포도의 크기와 기간이 포도의 가격에 영향을 미친다.
# 회귀계수가 모두 +값을 갖기 때문에,포도의 크기와 기간이 증가할수록 포도의 가격도 증가한다.
# (Adjusted R-squared:  0.8853)R-squared값이 0.88로 88%의 설명력을 갖는다.(종속변수분산의 15%가 독립변수에 의해 설명)


###분산분석###
jobedu <- read.csv("jobedu.csv", header = T)

tapply(jobedu$performance, jobedu$method, shapiro.test)

is.na(jobedu$performance)
jobedu$performance <- ifelse(jobedu$performance==99, NA, jobedu$performance)
is.na(jobedu$performance)
jobedu2 <- jobedu %>% filter(!is.na(performance))
table(is.na(jobedu2$performance))

tapply(jobedu2$performance, jobedu2$method, shapiro.test)
# p-value가 0.05보다 크므로 정규성을 충족
bartlett.test(jobedu2$performance, jobedu2$method, data=jobedu2)
# p-value가 0.05보다 크므로 집단 간 등분산 충족
jobedu.lm <- lm(performance~method, data=jobedu2)
anova(jobedu.lm)
# jobedu.ano <- oneway.test(performance~method,jobedu2,var.equal=T)
# jobedu.ano
# result <- aov(performance~method, data=jobedu2)
# summary(result)
# 검정결과, p-value가 0.05보다 작으므로 대립가설 채택
# 집단별 평균에 차이가 있다/교육과정별 성과에 차이가 있다
model <- aov(performance~method, data=jobedu2)
comparison <- LSD.test(model, "method",p.adj = "bonferroni", group = T)
comparison
# 각 집단별 평균 차이를 확인할 수 있다





###비율검정### <- 안나옴
museum <- read.csv("museum.csv", header = T)
table(museum)
table(museum$group,museum$visit)
freq(museum$visit)
freq(museum$group)

prop.table(table(museum$group,museum$visit))
round(prop.table(table(museum$group,museum$visit))*100,0)
prop.test(c(20,34),c(50,50))
# 검정결과, p-value가 0.05보다 작으므로 두집단 간 방문의도에 대한 비율은 차이가 있다
prop.test(c(20,34),c(100,100), alternative = "two.sided", conf.level = 0.95)

