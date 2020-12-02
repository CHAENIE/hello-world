a<-4
b<-5
c=a+b
print(c) #print 사용 잘 안함
c
a=3.14 #PI
a<-"Hello"
a
print(a)
x="seoul"
x
y='수도'
y

TRUE & TRUE
TRUE & FALSE
TRUE | TRUE
TRUE | FALSE
!TRUE
!FALSE
TRUE

gender<-factor("f",c("m","f"))
gender
levels(gender)[1]
levels(gender)[2]
gender
levels(gender)<-c("male","female")
gender

ordered(c("a",'b','c','d'))
factor(c("a",'b','c','d'),ordered=TRUE)

x<-c(1,2,3,4,5,6,7,8,9)
x
x<-c('1','2','3','4','5')
x
c(1,2,3,4,5)
c(1,2,3,4,5,c(1,2,3,4,5))

x<-1:5
x
x<-2:4
x
seq(1,100,3)
seq_along(c("a","b","c","d","h"))
seq_len(6)
x<-c(1,3,5,7)
names(x)<-c("noh","kim",'park','baek')
x

x<-c("a","b","c","d")
x[2]
x[4]
x[-3] #세 번째 "c" 제거
x[-4]
x
x<-c("a","b","c","d")
x[c(1,3)]
x[c(2,4)]
length(x)
nrow(x)
NROW(x)

"c" %in% c("a","v","c","d")
"e" %in% c("a","v","c","d")

x=1:4
y=c(1,3,5)
z=c(1,2,3,3,4)
setequal(x,y)
setequal(x,z) #z애서 중복된 3은 제거가 되기 때문에 x와 같은 값
seq(1,10)
seq(10,1,-2)

1:10
seq(1,10)
rep(1:3,5) #rep(1:3,times = 5)
rep(1:3,each=3)
sequence(c(2,5,3)) #1:2,1:5,1:3

mode("big")
mode(3.14)
mode(list("big",1))

x=1:7; x
x<7
x>3
x>1 & x<7
x>1 && x<7
x>1 |x<7
x>1||x<7
x==5
x!=5
!x==5

mode(pi);typeof(pi);storage.mode(pi)
a=3.14159;mode(a)
mode(5>8)
국가="대한민국";mode(국가)
mode(T);mode(TRUE);mode(FALSE)
mode(1+5i)
typeof("a")

one<-100
two<-90
three<-80
four<-70
five<-NA
is.na(four)

x<-NULL
is.null(x)

3+TRUE
T+T
T*F
5i

a=as.numeric("3.14159");as.integer("3.14159")
as.integer(pi)
is.numeric(pi);is.double(pi);is.integer(pi)
is.data.frame(a);is.data.frame(BOD)

matrix(1:20,ncol=5)
array(1:20,dim=c(4,5)) #dimension 약자, 배열 선언에 사용

array(1:20, dim=c(4,4,3))
x<-array(1:20, dim=c(4,4,3))
x

v<-c(10,20,30,40,50,60)
names(v)=c("a","b","c","d","e","f")
v
A<-1:9
dim(A)
dim(A)<-c(3,3) #dim() 벡터에 차원이 주어져야만 함 
A

c<-list(1,2,3,4,5,6,"a","b","c","d","e","f")
c
dim(c)<-c(3,4)
c

lst<-list(c(1,2),c(3,4),c(5,6),c(7,8))
lst

lst<-list(mid=3.14,right='abc')
lst
year<-list(1975,1995,1983)
year
year[[3]]
class(year[[3]])
class(year[3])
cat(year[3]) #cat()은 리스트 구조 분석 수행X - Error

iq.score<-list(c(110,130,145,160))
mean(unlist(iq.score))
cat("iq score:",unlist(iq.score))
lst<=list(NULL,1,2,3,4,5)
lst

a<-data.frame(x=c(1,3,5),y=c(2,4,6),z=c("P","S","T"))
a

capital<-c("seoul","rome","vienna",'bern')
values<-1:4
mode(capital)
nal<-data.frame(capital,values) #다른 벡터 합칠때 data.frame() 사용
nal
names(nal)
names(nal)<-c("city","rank")
names(nal)
nal
vec<-c("100","95","90","85")
nal$ncol<-vec
nal
names(nal)<-c("city","rank","score")
nal
nal<-subset(nal, select = -score)
nal

a1<-1:5
a2<-6:10
a1;a2
my1<-list(data1=a1,data2=a2)
my1
mode(my1)
data.frame(my1)
mydata<-data.frame(my1)
rownames(mydata) #데이터프레임 행이름 변경
colnames(mydata) #데이터프레임 열이름 변경
rownames(mydata)<-c("one","two","three","four","five")
mydata

x<-c(1,2,3,4,5,6,7,8,9)
x
mode(x)
x<-c("1","2","3","4")
x
c(1,2,3,4,5)
c(1,2,3,4,5,c(1,2,3,4,5))
x<-1:5
x
x<-2:4
x
seq(1,5,2)

seq_along(c("a","b","c","d"))
seq_len(4)
x <- c(1,3,5,7)
names(x) <- c("noh","kim","park","baek")
x
x<-c("a","b","c","d")
x[2]
x[4]
x[-3]
x
x[c(1,3)]
x[c(2,4)]
x[c(1:3)]
x[c(1:4)]
length(x)
nrow(x)
NROW(x)
"c" %in% c("a","b","c","d")
"e" %in% c("a","b","c","d")
x=1:4
y=c(1,3,5)
z=c(1,2,3,3,4)
setequal(x,z)
setequal(y,z)
seq(1,10)
seq(1:10)
1:10
seq(1,10,2)
iris
summary(iris)
View(iris)
summary(AirPassengers)
summary(iris3)
summary(as.list(iris))
lapply(as.list(iris),summary)
iris$Species2<-as.factor(sample(1:5,150,replace=TRUE))
str(iris)
View(iris)
summary(table(iris$Species, iris$Species2))
scale(iris$Sepal.Length)
(3.8 - mean(iris$Sepal.Length))/sd(iris$Sepal.Length)
x<=rnorm(40,mean=95, sd=10)
t.test(x,mu=90)
x<-rnorm(40,mean=95,sd=10)
t.test(x)
t.test(x, conf.level=0.99)
prop.test(56,100,0.5)
prop.test(56,100,0.5,alternative = "greater")
shapiro.test(x)
attach(iris)
shapiro.test(Sepal.Length)
cor.test(iris$Sepal.Length,iris$Sepal.Width)

a<-c(100,98,85,90,88,80)
b<-c(73,80,80,75,67,57)
c<-c(110,104,91,109,85,95)
life <- data.frame(a,b,c)
b.life=stack(life)
b.life

type=c(rep('a',6),rep('b',6),rep('c',6))
y=c(100,98,85,90,88,80,73,80,80,75,67,57,110,104,91,109,85,95)
ty=as.factor(type)
life.aov=aov(y~ty)
summary(life.aov)
life.tukey=TukeyHSD(life.aov, "ty",ordered=TRUE)
life.tukey
plot(life.tukey)


pressure=as.factor(c(320,340,360,310,330,350,300,320,340,310,330,350))
temp=as.factor(c(rep('low',6),rep('high',6)))
y=c(130.5,120.2,150.8,
    +170.2, 157.1, 164.7,
    +102.6, 181.6, 160.5,
    +189.5, 165.3, 176.5)
op=par(mfrow=c(2,2))
plot(y~temp)
plot(y~pressure)
stripchart(y~temp,vertical=TRUE, xlab="temperature")
stripchart(y~pressure, vertical=TRUE, xlab="pressure")
par(op)

op=par(mfrow=c(1,2))
interaction.plot(temp, pressure, y, bty='l',main='interaction plot')

interaction.plot(pressure,temp, y, bty='o')
par(op)

aov_pt=aov(y~temp+pressure+temp:pressure)
aov_pt
summary(aov_pt)

install.packages("zoo")
library(zoo)

apts<-ts(AirPassengers,frequency = 12)
plot(apts)
acf(apts)
pacf(apts)
spectrum(apts)
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

m <- lm(coredata(apts)~index(apts))
apts.eltr <- ts(resid(m),index(apts))
plot(apts.eltr)
plot(diff(log(apts)))

f <- decompose(apts)
attributes(f)
plot(f$figure, type="b",xaxt="n",xlab="")
monthNames <- months(ISOdate(2012,1:12,1))
axis(1,at=1:12,labels=monthNames,las=2)
plot(f)

install.packages("forecast")
library(forecast)

apts.arima <- auto.arima(apts)
summary(apts.arima)

fore <- predict(apts.arima, n.ahead = 24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
apts.smoothing <- HoltWinters(apts, seasonal="mul")
fore2 <- predict(apts.smoothing, n.ahead = 24)
ts.plot(apts, fore$pred, U, L, fore2, col = c(1,2,4,4,6),lty=c(1,1,2,2,3))
legend("topleft",c("Actual","ARIMA","ARIMA Error Bounds (95% confidence)","exponential smoothing"),col=c(1,2,4,6),lty=c(1,1,2,3))

apts.log <- log(apts)
apts.log.arima <- auto.arima(apts.log)
summary(apts.log.arima)

fore <- predict(apts.log.arima, n.ahead = 24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
apts.smoothing <- HoltWinters(apts.log,seasonal="mul")
fore2 <- predict(apts.smoothing, n.ahead = 24)
ts.plot(exp(apts.log),exp(fore$pred),exp(U), exp(L),exp(fore2),col=c(1,2,4,4,6),lty=c(1,1,2,2,3))
legend("topleft",c("Actual","ARIMA","ARIMA Error Bounds (95% confidence)","exponential smoothing"),col=c(1,2,4,6),lty=c(1,1,2,3))

a <- c(1,5)
b <- c(2,3)
c <- c(5,7)
d <- c(3,5)
e <- c(5,2)
data <- data.frame(a,b,c,d,e)
data <- t(data)
data
par(mfrow=c(2,2))
(m1 <- hclust(dist(data)^2, method = "single"))
plot(m1)
(m2 <- hclust(dist(data)^2, method="complete"))
plot(m2)
(m3 <- hclust(dist(data)^2, method="ward.D2"))
plot(m3)
(m4 <- hclust(dist(data)^2, method = "average"))
plot(m4)

rm(list=ls(all=TRUE))
data <- iris
head(data)
data$Species <- NULL
head(data)
(m <- kmeans(data,3))
table(iris$Species,m$cluster)
plot(data[c("Sepal.Length","Sepal.Width")],main="kmeans",col=m$cluster)
plot(iris$Sepal.Length,iris$Sepal.Width, main="true",col=c(1,2,3)[unclass(iris$Species)])
(m <- kmeans(data,4))
table(iris$Species,m$cluster)
plot(data[c("Sepal.Length","Sepal.Width")],main="kmeans",col=m$cluster)
(m <- kmeans(data,5))
table(iris$Species,m$cluster)
plot(data[c("Sepal.Length","Sepal.Width")],main="kmeans",col=m$cluster)

x <- c(2,5,6,7,5,9,11,5,7,9,13,15,17)
y <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
par(mfrow=c(2,5))
plot(x,y,main="PLOT",sub = "Test",xlab = "x-label",ylab = "y-label",type = "p",xlim = c(0,20),ylim = c(0,20))
z <- c(3.5,1.5,2.3,6.6,4.7)
a <- c(2,7,12)
plot(x,y,main = "PLOT",sub = "Test", xlab = "x-label",ylab = "y-label",type = "n")
points(z,pch=1,cex=1)
points(z,pch=3,cex=1)
points(z,pch=5,cex=1)
points(z,pch=7,cex=1)
points(z,pch=9,cex=1)
points(z,pch=11,cex=1)
points(z,pch=13,cex=1)
points(z,pch=15,cex=1)
points(z,pch=17,cex=1)
points(z,pch=19,cex=1)
text(2,1,"Plotting",col = "red")

plot(0:8,0:8,type = "n",ylim=c(0,20))
lines(c(2,6),c(20,20),lty=1)
lines(c(2,6),c(19,19),lty=2)
lines(c(2,6),c(18,18),lty=3)
lines(c(2,6),c(17,17),lty=4)
lines(c(2,6),c(16,16),lty=5)
lines(c(2,6),c(15,15),lty=6)
lines(c(2,6),c(14,14),lty="blank")
lines(c(2,6),c(13,13),lty="solid")
lines(c(2,6),c(12,12),lty="dashed")
lines(c(2,6),c(11,11),lty="dotted")
lines(c(2,6),c(10,10),lty="dotlash")
lines(c(2,6),c(9,9),lty="longdash")
lines(c(2,6),c(8,8),lty="twodash")
lines(c(2,6),c(7,7),lty="33")
lines(c(2,6),c(6,6),lty="24")
lines(c(2,6),c(5,5),lty="F2")
lines(c(2,6),c(4,4),lty="2F")
lines(c(2,6),c(3,3),lty="3313")
lines(c(2,6),c(2,2),lty="F252")
lines(c(2,6),c(1,1),lty="FF29")

plot(x,y,main = "PLOT",sub = "Test", xlab = "x-label",ylab = "y-label",type = "p")
text(9,10,"Plotting",col="red")

x <- c(2,5,6,7,5,9,11,5,7,9,13,15,17)
y <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
plot(x,y,main = "PLOT",sub = "Test", xlab = "x-label",ylab = "y-label",type = "p")
abline(h=6,v=9,lty=3)
plot(x,y,type="n")
legend("center","(x,y)",pch = 1,title="center")
legend("top","(x,y)",pch = 1,title="top")
legend("left","(x,y)",pch = 1,title="left")
legend("right","(x,y)",pch = 1,title="right")
legend("bottom","(x,y)",pch = 1,title="bottom")

x <- rnorm(1000,mean = 5,sd=1)
hist(x)
hist(x,freq = F)
curve(dnorm(x,mean = 5,sd=1),add = T)

x <- c(2,5,6,5,7,9,11,5,7,9,13,15,17)
z <- c(3.5,2.2,1.5,4.6,6.9)
boxplot(x,z)

pie(x)

barplot(x)

hist(x)
hist(x,freq = F)

install.packages("ggplot2")
library(ggplot2)
test.data <- data.frame(length=c(3,2,5,8),width=c(4,3,6,9),depth=c(5,2,16,80),trt=c("a","a","b","b"))
ggplot(test.data, aes(x=length,y=width))+geom_point(aes(colour=trt))
ggplot(test.data,aes(x=length,y=width))+geom_point(aes(colour=trt))+geom_smooth()

head(diamonds)
View(diamonds)

ggplot(data=diamonds,aes(x=carat,y=price, colour=clarity))+geom_point()+geom_smooth()

apts <- ts(AirPassengers,frequency = 12)
plot(apts)
library(forecast)
apts.arima <- auto.arima(apts)
summary(apts.arima)

fore <- predict(apts.arima, n.ahead = 24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
apts.smoothing <- HoltWinters(apts,seasonal = "mul")
fore2 <- predict(apts.smoothing,n.ahead = 24)
ts.plot(apts, fore$pred, U, L, fore2, col=c(1,2,4,4,6),lty=c(1,1,2,2,3))
legend("topleft",c("Actual","ARIMA Error Bounds (95% confidence)","exponential smoothing"),col=c(1,2,4,6),lty=c(1,1,2,3))

data <- iris
head(data)
data$Species<-NULL
head(data)
(m <- kmeans(data,3))
table(iris$Species,m$cluster)

x <- rnorm(1000,mean = 5, sd=1)
hist(x, freq = F)
curve(dnorm(x,mean = 5,sd = 1),add = T)

library(ggplot2)

test.data <- data.frame(length=c(3,2,5,8),width=c(4,3,6,9),depth=c(5,2,16,80),trt=c("a","a","b","b"))
ggplot(test.data,aes(x=length, y=width)) + geom_point(aes(colour=trt)) + geom_smooth()
ggplot(data=diamonds, aes(x=carat, y=price, colour=clarity)) + geom_point() + geom_smooth()

ggplot(data=diamonds, aes(x=carat, y=price)) + geom_smooth(aes(group=clarity))

ggplot(data = diamonds, aes(x=price)) + stat_bin(geom="bar",fill="gold",col="white")
ggplot(diamonds, aes(clarity,fill=cut)) + geom_bar()
