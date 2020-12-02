jobedu <- read.csv("jobedu.csv",header=TRUE)
tapply(jobedu$performance, jobedu$method, shapiro.test)
is.na(jobedu)
table(is.na(jobedu$performance))
jobedu$performance <- ifelse(jobedu$performance==99, NA, jobedu$performance)
is.na(jobedu$performance)
table(is.na(jobedu$performance))
jobedu2 <- jobedu %>% filter(!is.na(performance))
tapply(jobedu2$performance, jobedu2$method, shapiro.test)

bartlett.test(jobedu2$performance, jobedu2$method, data=jobedu2)
jobedu.lm <- lm(performance~method, data = jobedu2)
anova(jobedu.lm)

install.packages("agricolae")
library(agricolae)
model <- aov(performance~method, jobedu2)
comparison <- LSD.test(model, "method",p.adj="bonferroni",group = T)
comparison

install.packages("nortest")
library(nortest)
tapply(jobedu2$performance, jobedu2$method, nortest::ad.test)
jobedu2.anova <- oneway.test(performance~method, data=jobedu2, var.equal = F)
jobedu2.anova

library(psych)
a <- read.csv("cosmetics.csv", header = TRUE)
attach(a)
describeBy(satisf_al, group = decision) #describe by group(decision) ,library(psych)
ano <-  aov(satisf_al~factor(decision))
summary(ano)
TukeyHSD(ano)
scheffe.test(ano, "factor(decision)", alpha = 0.05, console = T) #alpha=신뢰수준/"factor(decision)"매개변수로 돌리겠다
duncan.test(ano, "factor(decision)",alpha = 0.05, console = T)
LSD.test(ano, "factor(decision)", alpha = 0.05, console = T)
#detach(a)
a.lm <- lm(satisf_al~factor(decision))
anova(a.lm)
a.anova <- oneway.test(satisf_al~factor(decision), var.equal = F) #var.test가 동질적X(등분산이 동질적이지 않아도 분산분석돌릴 수 있게함)
a.anova
detach(a)

######회귀분석########
sales <- read.csv("sales.csv", header=TRUE)
str(sales)
sales.lm <- lm(sales~rd, data=sales)
summary(sales.lm)
plot(sales~rd, data = sales)
abline(sales.lm)

sales.lm2 <- lm(profit~rd+ad+promotion, data=sales)
sales.lm2
summary(sales.lm2)

install.packages("car")
library(car)
vif(sales.lm2)
sales.lm3 <- lm(profit~promotion+ad, data=sales)
vif(sales.lm3)
summary(sales.lm3)

install.packages("pequod")
library(pequod)
model1 <- lm(repurchase~satisf_i, data = a, x=TRUE, y=TRUE)
anova(model1)
summary(model1)
ggplot(a, aes(x=satisf_i, y=satisf_al))+geom_smooth(method = lm, color="red")+geom_point()

install.packages("lm.beta")
library(lm.beta)
library(lmtest)

ncvTest(model1)
dwtest(model1)
model2 <- lm.beta(model1)
summary(model2)
par(mfrow=c(1,2))
plot(model1)
avPlots(model1, id.n=2, id.cex=0.7)
outlierTest(model1)
influenceIndexPlot(model1, id.n=3)
a$pre <- model1$fitted.values
a$res <- model1$residuals
hist(a$res)
hist(a$pre)
model3 <- lm(repurchase~satisf_i+satisf_b, x=TRUE, y=TRUE)
summary(model3)




####### dummy #########
library(pequod)
dum.model1 <- lm(repurchase~satisf_b+satisf_i+factor(gender)+factor(decision),x=TRUE, y=TRUE)
anova(dum.model1)
summary(dum.model1)

a$gender <- factor(a$gender, levels = c(1,2), labels = c('male','female'))

dum.model2 <- lm.beta(dum.model1)
summary(dum.model2)
a$gender



####### R point 178p no.1 ############
library(nortest)
library(dplyr)
coffee <- read.csv("coffee.csv", header = TRUE)
tapply(coffee$revisit,coffee$brand, shapiro.test)
is.na(coffee)
table(is.na(coffee$brand))
tapply(coffee$revisit,coffee$brand, nortest::ad.test)

bartlett.test(coffee$revisit, coffee$brand, data=coffee)
coffee.anova <- oneway.test(coffee$revisit~coffee$brand, var.equal = F)
coffee.anova





########logistic#######
install.packages("car")
library(car)
a$repurchase_re <- recode(a$repurchase, "lo:3=0 ; 4:hi=1; else=NA")
library(aod)
install.packages("aod")
install.packages("Rcpp")
library(Rcpp)
logit.model <- glm(a$repurchase_re~factor(propensity)+factor(decision)+satisf_al, family = binomial)
summary(logit.model)
exp(cbind(OR=coef(logit.model), confint(logit.model)))


######보건의료 stepwise regression analysis######
install.packages("survival")
library(survival)
data(colon)
str(colon)
colonl <- na.omit(colon)
result <- glm(status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg, family = binomial, data=colonl)
summary(result)
f.model <- glm(status~., data=colonl)
r.model1=step(f.model, direction = "backward")
r.model2=step(f.model, direction = "forward")
r.model3=step(f.model, direction = "both")



###### dummy ######
cust_id <- c("c01","c02","c03","c04","c05","c06","c07")
age <- c(25,45,31,30,49,53,27)
cust_profile <- data.frame(cust_id,age,stringsAsFactors = F)
sapply(cust_profile, class)

cust_profile <- transform(cust_profile,
                          age_20=ifelse(age>=20 & age<30, 1,0),
                          age_30=ifelse(age>=30 & age<40, 1,0),
                          age_40=ifelse(age>=40 & age<50, 1,0),
                          age_50=ifelse(age>=50 & age<60, 1,0))
cust_profile


################## sampling ########################
##### simple random sampling #####
#비복원추출
qm <- 1:100
sample(qm, size = 5)
#복원추출
qm2 <- 1:100
sample(qm2, size=5, replace=TRUE)
#가중치추출
qm3 <- 1:10
sample(qm3, size=5, prob = 1:10)
##### stratified random sampling #####
#충화임의추출
install.packages("sampling")
library(sampling)
qm4 <- strata(data=iris,
        c("Species"),
        size=c(3,3,3),
        method="srswor")
qm4
getdata(iris, qm4)
##### systematic sampling #####
#계통추출
install.packages("doBy")
library(doBy)
qm5 <- data.frame(x=1:100)
sampleBy(~1, frac=0.3, data = qm5, systematic = TRUE)


############ 두집단비교 ###############
x <- c(144,128,145,144,140,146,142,135,142,140,142,142,144,141,141,135,142,138,135,141,146,138,140,136,140,140,142,136,131,140)
y <- c(140,137,139,136,139,135,138,140,137,139,139,136,140,130,139,133,139,141,138,135,142,136,135,139,136,136,137,137)
var.test(x,y,paired=TRUE)
t.test(x,y,alternative = "two.sided",conf.int=TRUE,conf.level = 0.95)
t.test(x,y,alternative = "greater",conf.int=TRUE,conf.level = 0.95)
t.test(x,y,alternative = "less",conf.int=TRUE,conf.level = 0.95)
########### 상관관계분석 ###############
x <- c(57.4, 64.7,50.4,55.8,64.1,72.3,50.5,52.3,60.3,54.5)
y <- c(44.4,52.0,38.1,49.3,60.3,63.2,39.5,30.3,57.5,41.0)
plot(x,y,cex=3, pch="*",col="blue")
corr <- data.frame(x,y)
