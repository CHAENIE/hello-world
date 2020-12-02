setwd("c:/bigR")
#140p 상관관계분석
sales <- read.csv("sales.csv",header = TRUE)
str(sales)

cor.pearson <- cor.test(~promotion + sales, method="pearson",data=sales)
cor.pearson
cor.spearman <- cor.test(~promotion + sales, method="spearman",data=sales)
cor.spearman
cor.kendall <- cor.test(~promotion + sales, method="kendall",data=sales)
cor.kendall

cor(sales)
plot(sales)
pairs(sales, panel=panel.smooth)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(sales, histogram = TRUE, pch=20) #pch 숫자: 숫자에 해당되는 형태출력
install.packages("corrplot")
library(corrplot)
sales.cor <- cor(sales)
sales.cor
corrplot(sales.cor, method = "ellipse")
corrplot(sales.cor, method = "pie")
corrplot(sales.cor, method = "number")
corrplot(sales.cor, method = "color")
corrplot(sales.cor, method = "shade")
corrplot(sales.cor, method = "shade", addshade = "all", shade.col = FALSE, tl.col = "red", 
         tl.srt = 30, diag = FALSE, addCoef.col = "white", order = "FPC")


#상관관계분석연습문제 no.1
coffee.cor <- read.csv("coffee.cor.csv", header = TRUE)
str(coffee.cor)
revist <- coffee.cor$revist
interior <- coffee.cor$interior
quality <- coffee.cor$quality
coffee2 <- data.frame(revist, interior, quality)
str(coffee2)

cor.pearson <- cor.test(~revist + interior, method="pearson",data=coffee2)
cor.pearson

coffee3 <- cor(coffee2)
plot(coffee2)
pairs(coffee2,panel=panel.smooth)
corrplot(coffee3, method = "ellipse")
corrplot(coffee3, method = "shade",addshade = "all", shade.col = FALSE, tl.col = "red", 
         tl.srt = 30, diag = FALSE, addCoef.col = "white", order = "FPC")


#상관관계분석연습문제 no.2
attitude1 <- student$attitude1
attitude2 <- student$attitude2
attitude3 <- student$attitude3
grade <- student$grade
student4 <- data.frame(attitude1,attitude2, attitude3, grade)
student4
str(student4)
student5 <- cor(student4)
plot(student4)
pairs(student4, panel=panel.smooth)
corrplot(student5, method = "shade",addshade = "all", shade.col = FALSE, tl.col = "red", 
         tl.srt = 30, diag = FALSE, addCoef.col = "white", order = "FPC")



#상관관계분석연습문제 no.3
student <- read.csv("student.csv",header = TRUE)
activity1 <- student$activity1
activity2 <- student$activity2
activity3 <- student$activity3
activity1;activity2;activity3
grade <- student$grade
student2 <- data.frame(activity1,activity2,activity3,grade)
str(student2)
student3 <- cor(student2)
plot(student2)
pairs(student2, panel = panel.smooth)
corrplot(student3, method = "ellipse")
corrplot(student3, method = "shade",addshade = "all", shade.col = FALSE, tl.col = "red", 
         tl.srt = 30, diag = FALSE, addCoef.col = "white", order = "FPC")
