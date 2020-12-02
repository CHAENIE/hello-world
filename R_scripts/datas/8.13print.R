install.packages("installr")
library(installr)
updateR()
update.packages(checkBuilt = TRUE)
version
packageStatus()
w <- c(1,2,3)
e <- c(4,5,6)
z = rbind(w,e)
z
zz=cbind(w,e)
zz
z[1,]
z[,2]
z[2,3]
z[-1,]
zlist <- z[2,3]
zlist
a <- c(10,11,12)
b <- c(13,14,15)
ab <- rbind(a,b)
ab

x <- matrix(1:20, nrow = 4)
x
z
row <- apply(x, 1, mean)
row
col <- apply(x, 2, mean)
col

head(iris)
summary(iris)
order(iris$Sepal.Length)
tem <- iris[order(iris$Sepal.Width),]
head(tem)

sample(1:10, 5, replace=TRUE)
split(iris, iris$Species)
tem2 <- subset(iris, Species=="setosa")
head(tem2)
tem3 <- subset(iris, Species=="setosa" & Sepal.Length>5.0)
head(tem3)
sample(1:10, 5 , replace = TRUE)

tem4 <- subset(iris, select = c(Sepal.Width, Species))
str(tem4)
head(tem4)

score <- data.frame(name=c("Seoul","Busan","Daegu","Kwangju"), population=c(1500,200,150,70))
score
score2 <- data.frame(name=c("Kwangju","Daegu","Seoul","Busan"), HighTemp=c(35,40,32,29))
score2
merge(score,score2)

with(iris,
       {
        print("Max of Sepal.Width\n")
        print(max(Sepal.Width))
        print("Min of Sepal.Width\n")
        print(min(Sepal.Width))
       })
which.min(iris$Sepal.Length)
which.max(iris$Sepal.Length)
aggregate(Sepal.Width~Species, iris, mean)
aggregate(Sepal.Width~Species, iris, max)

a <- 1:12
for (i in a) {
  print(i*2)
}
tracemem(a)
a <- 5
while (a<12) {
  a=a+1
  print(a)
}
tapply(vector, index, function)
apply(array, margin, ...)
tapply(1:10, rep(c(1,2),each=5),sum) #rep(1,5, each=5) 특정값을 각각 몇 번 반복할때 사용





