######## 8/18 건강검진데이터 ##############
####no.1######
health.man<-subset(health, 성별코드 == 1)

nortest::ad.test(health.man$허리둘레)
t.test(health.man$허리둘레, mu=85, alter="two.sided", conf.level=0.95)
t.test(health.man$허리둘레, mu=85, alter="greater", conf.level=0.95)


####no.2######
setwd("C:/Rtest")
health<-read.csv("health.csv", header = T)

groupA<-subset(health, 성별코드 == 1)
groupB<-subset(health, 성별코드 == 2)

var.test(groupA$총콜레스테롤, groupB$총콜레스테롤)
wilcox.test(groupA$총콜레스테롤, groupB$총콜레스테롤, alter="two.sided", confint=T, conf.level=0.95)

####no.3######
health<-read.csv("health.csv", header = T)
table(health$성별코드, health$흡연상태)

health$성별코드1[health$성별코드 == 1]<-"남"
health$성별코드1[health$성별코드 == 2]<-"여"
health$흡연상태1[health$흡연상태 == 1]<-"피우지 않는다"
health$흡연상태1[health$흡연상태 == 2]<-"피웠다 끊었다"
health$흡연상태1[health$흡연상태 == 3]<-"현재까지 피운다"

table(health$성별코드1, health$흡연상태1)
chisq.test(health$성별코드1, health$흡연상태1)

install.packages("gmodels")
library(gmodels)
CrossTable(health$성별코드1, health$흡연상태1, expected=T, format="SPSS")


####no.4######
setwd("C:/Rtest")
health<-read.csv("health.csv", header = T)
install.packages("nortest")

health$흡연상태<-as.factor(health$흡연상태)
tapply(health$총콜레스테롤, health$흡연상태, nortest::ad.test)
table(is.na(health$흡연상태))

health1<-health %>% filter(!is.na(흡연상태))
health1$흡연상태<-as.factor(health1$흡연상태)
tapply(health1$총콜레스테롤, health1$흡연상태, nortest::ad.test)

bartlett.test(health1$총콜레스테롤, health1$흡연상태)

health.anova <- oneway.test(총콜레스테롤~흡연상태, data=health1, var.equal=F)
health.anova                          

install.packages("agricolae")
library(agricolae)
model<-aov(총콜레스테롤~흡연상태, health1)
comparison<-LSD.test(model, "흡연상태", p.adj = "bonferroni", group = T)
comparison
TukeyHSD(model)

####no.5######
setwd("C:/Rtest")
health<-read.csv("health.csv", header = T)

health$age[health$연령대코드.5세단위. >=1 & health$연령대코드.5세단위. <=4]<-1
health$age[health$연령대코드.5세단위. >=5 & health$연령대코드.5세단위. <=8]<-2
health$age[health$연령대코드.5세단위. >=9 & health$연령대코드.5세단위. <=12]<-3
health$age[health$연령대코드.5세단위. >=13 & health$연령대코드.5세단위. <=16 ]<-4
health$age[health$연령대코드.5세단위. >=17 ]<-5

table(health$신장.5Cm단위.)
health$age<-as.factor(health$age)
tapply(health$신장.5Cm단위., health$age, nortest::ad.test)

table(is.na(health$신장.5Cm단위.))
health$신장.5Cm단위.<-ifelse(health$신장.5Cm단위. >=195, NA, health$신장.5Cm단위.)
table(is.na(health$신장.5Cm단위.))

health2<-health %>% filter(!is.na(신장.5Cm단위.))

health2$age<-as.factor(health2$age)
tapply(health2$신장.5Cm단위., health2$age, nortest::ad.test)

bartlett.test(health2$신장.5Cm단위., health2$age)

health.anova1 <- oneway.test(신장.5Cm단위.~age, data=health2, var.equal=F)
health.anova1       

library(agricolae)
model1<-aov(신장.5Cm단위.~age, data=health2)
comparison1<-LSD.test(model1, "age", p.adj = "bonferroni", group = T)
comparison1
TukeyHSD(model1)

####no.6######
health$X.혈청지오티.AST<-as.numeric(health$X.혈청지오티.AST)
health$X.혈청지오티.ALT<-as.numeric(health$X.혈청지오티.ALT)
health$감마지티피<-as.numeric(health$감마지티피)

health.cor<-health[, c("X.혈청지오티.AST","X.혈청지오티.ALT","감마지티피")]
cor(health.cor)

cor.pearson<-cor.test(~ X.혈청지오티.AST+X.혈청지오티.ALT, method="pearson",data=health.cor)
cor.pearson
cor.spearman<-cor.test(~ X.혈청지오티.AST+X.혈청지오티.ALT, method="spearman",data=health.cor)
cor.spearman
cor.kendall<-cor.test(~ X.혈청지오티.AST+X.혈청지오티.ALT, method="kendall",data=health.cor)
cor.kendall

install.packages("corrplot")
library(corrplot)

health.co<-cor(health.cor)
health.co

corrplot(health.co, method = "number")

####no.7######
setwd("C:/Rtest")
health<-read.csv("health.csv", header = T)

health<-transform(health, 피지않는다 = ifelse(흡연상태=="1", 1,0), 
                  피웠으나끊었다=ifelse(흡연상태=="2", 1,0), 피고있다=ifelse(흡연상태=="3", 1,0))
healt.lm<-lm(체중.5Kg.단위.~식전혈당.공복혈당.+총콜레스테롤+트리글리세라이드+
                 혈색소+혈청크레아티닌+감마지티피+피웠으나끊었다+피고있다, data=health)
summary(healt.lm)

install.packages("car")
library(car)
vif(healt.lm)

####no.8######
health<-transform(health,man=ifelse(성별코드=="1", 1,0), woman=ifelse(성별코드=="2", 1,0))

health.glm<-glm(음주여부~man+식전혈당.공복혈당.+총콜레스테롤+트리글리세라이드+
                      혈색소+혈청크레아티닌+감마지티피, data=health)
summary(health.glm)
