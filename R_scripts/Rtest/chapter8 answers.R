setwd("c:/Rtest")
#8장1절
library(cluster)
result <- read.csv("myRFM.csv",header = TRUE)
View(result)
result2 <- hclust(dist(result), method = "ave")
names(result2)
result2$order
plot(result2, hang=-1, labels = result2$ID)
result3 <- kmeans(result,3)
result3
plot(result[c("Frequency","Monetary")], col=result3$cluster)
points(result3$centers[,c("Frequency","Monetary")], col=1:3, pch=8, cex=2)



#8장2절
install.packages("arules")
library(arules)
result <- read.transactions("mybasket.csv",format = "basket",sep = ',')
result
summary(result)
image(result)
as(result, "data.frame")
rules <- apriori(result, parameter = list(supp=0.1, conf=0.1))
inspect(rules)

install.packages("arulesViz")
library(arulesViz)
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph", control = list(type="items"))
plot(rules, method = "graph",interactive = TRUE, control = list(type="items"))


#8장3절
install.packages("party")
library(party)
result <- read.csv("myhuman.csv",header = TRUE)
View(result)
set.seed(1234)
resultsplit <- sample(2, nrow(result), replace = TRUE, prob = c(0.7,0.3))
trainD <- result[resultsplit==1,]
testD <- result[resultsplit==2,]
rawD <- Group~Sociability + Rating + Career + Score
trainD$Group <- factor(trainD$Group) #추가
trainModel <- ctree(rawD, data = trainD)
table(predict(trainModel), trainD$Group)
print(trainModel)
plot(trainModel)
plot(trainModel, type="simple")
testModel <- predict(trainModel, newdata=testD)
table(testModel, testD$Group)


#8장4절
result <- read.csv("mytelecom.csv",header = TRUE)
View(result)
result2 <- lm(churn~tenure + income + age + gender, data = result)
result2
summary(result2)


#8장5절
library(KoNLP)
useSejongDic()
library(wordcloud)
library(RColorBrewer)
trump <- file("2017trump.txt", encoding = "UTF-8")
speech <- readLines(trump)
close(trump)
head(speech,5)
tail(speech,5)
pword <- sapply(speech, extractNoun, USE.NAMES = F)
pword
text <- unlist(pword)
head(text,20)
text2 <- Filter(function(x) {nchar(x)<=2},text)
head(text2,20)
text3 <- Filter(function(x) {nchar(x)<=3},text)
head(text3,20)
text4 <- Filter(function(x) {nchar(x)<=4},text)
head(text4,20)

text2 <- gsub("저","",text2)
text2 <- gsub("한","",text2)
text2 <- gsub("들이","",text2)
text2 <- gsub("앞","",text2)
text2 <- gsub("한","",text2)
text2 <- gsub("분","",text2)
text2 <- gsub("저","",text2)

text2 <- gsub("\\n","",text2)
text2 <- gsub("\\d+","",text2)
text2 <- gsub("\\.","",text2)
text2 <- gsub("","",text2)
text2 <- gsub("한","",text2)
head(text2,20)
write(unlist(text2), "mytext.txt")
myword <- read.table("mytext.txt")
nrow(myword)
wordcount <- table(myword)
head(sort(wordcount, decreasing = T),20)
palete <- brewer.pal(9,"Set1")
x11()
wordcloud(
  names(wordcount),
  freq = wordcount,
  scale = c(5,1),
  rot.per=0.5,
  min.freq = 7,
  random.order = F,
  random.color = T,
  colors=palete
)
text2 <- gsub("것","",text2)
text2 <- gsub("들","",text2)
text2 <- gsub("국","",text2)
text2 <- gsub("이","",text2)
text2 <- gsub("하게","",text2)
text2 <- gsub("년","",text2)
text2 <- gsub("선","",text2)
text2 <- gsub("나","",text2)
text2 <- gsub("해","",text2)
text2 <- gsub("전","",text2)
text2 <- gsub("속","",text2)

a <- head(sort(wordcount, decreasing = T),20)
pie(a, col=rainbow(10), radius = 1)
pct <- round(a/sum(a)*100,1)
names(a)
lab <- paste(names(a),"\n",pct,"%")
pie(a,main = "트럼프 대통령연설문", col=rainbow(10),cex=0.8,labels=lab)
par(new=T)
pie(a, radius = 0.6, col="white", lables=NA, border = NA)



install.packages(c("igraph","combinat"))
library(igraph)
library(combinat)
library(arules)

rule <- file("2017trump.txt",encoding = "UTF-8")
rules <- readLines(rule)
close(rule)
head(rules,10)

tran <- Map(extractNoun, rules)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y)
{nchar(y) <=4 && nchar(y)>1&& is.hangul(y)},x)})
tran <- Filter(function(x) {length(x) >=2}, tran)
tran
names(tran) <- paste("Tr", 1:length(tran), sep="")
names(tran)
wordtran <- as(tran, "transactions")
wordtran
wordtab <- crossTable(wordtran)
wordtab
ares <- apriori(wordtran, parameter = list(supp=0.1, conf=0.2))
inspect(ares)
rules <- labels(ares, ruleSep="")
rules <- sapply(rules, strsplit, " ",USE.NAMES = F)
rulemat <- do.call("rbind",rules)
