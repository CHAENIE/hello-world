#124p 일표본t-test
toeic <- read.csv("toeic.csv", header = TRUE)
mean(toeic$score)
range(toeic$score)
describe(toeic$score)
shapiro.test(toeic$score)
t.test(toeic$score, mu=650.0)
t.test(toeic$score, mu=650.0, alternative = "two.sided", conf.level = 0.95)


#129p 독립표본t-test
promotion <- read.csv("promotion.csv",header = TRUE)
promotion$event
promotion$brand
groupA <- subset(promotion, event==1&brand<999)
groupB <- subset(promotion, event==2&brand<999)
groupA
groupAcount <- length(groupA$event)
groupAmean <- round(mean(groupA$brand),2)
groupAcount;groupAmean
groupBcount <- length(groupB$event)
groupBmean <- round(mean(groupB$brand),2)
groupBcount;groupBmean

groupcount <- c(groupAcount, groupBcount)
groupmean <- c(groupAmean, groupBmean)
groupcount;groupmean
grouptable <- data.frame(Freq=groupcount, Mean=groupmean)
grouptable
var.test(groupA$brand, groupB$brand)
t.test(groupA$brand, groupB$brand, alternative = "two.sided",conf.level = 0.95)


#134p 대응표본t-test
rating <- read.xlsx("movie.xlsx", sheetName = "movie")
rating$before
rating$after
describe(rating$before)
describe(rating$after)

rating2 <- subset(rating, after<99)
rating3 <- subset(rating, after!=99)
rating4 <- subset(rating, after<99 & c(before, after))
rating5 <- subset(rating, after!=99 & c(before, after))
rating4
rating5
movieBE <- c(rating5$before)
movieAF <- c(rating5$after)
movieBE
movieAF
var.test(rating5$before, rating5$after, paired=TRUE)
wilcox.test(movieBE, movieAF, paired = TRUE, alter="two.sided", conf.int = TRUE, conf.level = 0.95)


#t-test연습문제 no.1
business <- read.csv("business.csv", header = TRUE)
mean(business$score)
range(business$score)
describe(business$score)
shapiro.test(business$score)
t.test(business$score, mu=3.2)
t.test(business$score, mu=3.2, alter="two.sided", conf.level = 0.95)


#t-test연습문제 no.2
business$gender
business$score
businessA <- subset(business, gender == 1) #female
businessB <- subset(business, gender == 2) #male
businessAcount <- length(businessA$gender)
businessAmean <- round(mean(businessA$score),2)
businessAcount;businessAmean
businessBcount <- length(businessB$gender)
businessBmean <- round(mean(businessB$score),2)
businessBcount;businessBmean

businesscount <- c(businessAcount, businessBcount)
businessmean <- c(businessAmean, businessBmean)
businesscount;businessmean
var.test(businessA$score,businessB$score)
t.test(businessA$score,businessB$score)
t.test(businessA$score,businessB$score, alter="two.sided", conf.int=TRUE, conf.level = 0.95)


#t-test연습문제 no.3
diet <- read.csv("diet.csv", header = TRUE)
diet$before
diet$after
describe(diet$before)
describe(diet$after)
diet2 <- subset(diet, after<999)
diet3 <- subset(diet, after!=999)
diet4 <- subset(diet, after<999, c(before, after))
diet5 <- subset(diet, after!=999, c(before, after))
diet5
dietBE <- c(diet5$before)
dietAF <- c(diet5$after)
dietBE;dietAF
var.test(diet5$before, diet5$after, paired=TRUE)
var.test(dietBE, dietAF, paired=TRUE)
wilcox.test(dietBE, dietAF, paired = TRUE, alter="two.sided", conf.int = TRUE, conf.level = 0.95)
length(dietBE)
length(dietAF)
