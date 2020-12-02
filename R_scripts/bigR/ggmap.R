if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
install.packages("ggmap")
library(ggmap)
register_google(key='AIzaSyB-6o0vZa7QcNPYBMcXMVP1FG1tE7U4JK4')

getwd()

setwd("c:/Rtest")
library(devtools)
install_github("dkahle/ggmap", force = TRUE)
install.packages("dplyr")
library(dplyr)
library(ggplot2)



map <- get_map(location = "seoul", zoom=14, maptype = "roadmap", source="google")
g <- ggmap(map)
print(g)

map <- get_map(location="seoul",zoom=14, maptype = "satellite", source="google")
g <- ggmap(map)               
print(g)

map <- get_map(location="seoul",zoom=14, maptype = "toner-lite", source="google")
g <- ggmap(map)
print(g)

landmarks <- c("nseoul tower, seoul", "city hall, seoul")
lbls <- cbind(geocode(landmarks), text=landmarks)
g <- ggmap(map)
g <- g+geom_point(data = lbls, aes(x=lon, y=lat), size=5, colour="orange")
g <- g+geom_point(data = lbls, aes(x=lon, y=lat), size=3, colour="red")
g <- g+geom_text(data = lbls, aes(x=lon, y=lat, label=text), size=5, colour="blue", hjust=0, vjust=0)
print(g)

wifi=read.csv("WIFI.csv", header = T)
View(a)
head(a)

attach(wifi)

bmap <- ggmap(get_googlemap(center = c("seoul"), zoom=11, maptype = "roadmap")) + geom_point(data=wifi, aes(x=LON, y=LAT, colour=INSTL_DIV, size=4))
print(bmap)

bmap + facet_wrap(~INSTL_DIV)

ggplot(wifi, aes(x=factor(1))) + geom_bar(aes(fill=INSTL_DIV), width = 1) + coord_polar(theta = "y") + xlab("") + ylab("")

ggplot(wifi, aes(CATEGORY)) + geom_bar(aes(fill=INSTL_DIV))
ggplot(wifi, aes(INSTL_DIV)) + geom_bar(aes(fill=CATEGORY))


##practice1

pigmap <- ggmap(get_map(location = 'south korea', zoom=7, color = 'bw'))
pig15 <- read.csv("map.csv", header = T, as.is = T)
attach(pig15)
head(pig15)
ppp15 <- subset(pig15, 년도=="15")
ppp15map <- pigmap + geom_point(data=ppp15, aes(x=lon, y=lat))
ppp15map <- ppp15map + geom_text(data=ppp15, aes(x=lon+0.01, y=lat+0.01, label=위치), size=2.5, check_overlap=T)
ppp15map + geom_point(data=ppp15, aes(x=lon, y=lat, color=factor(위치)), size=2) + scale_color_discrete(name="위치")


##practice2

getwd()
loc <- read.csv("서울_강동구_공영주차장_위경도.csv", header=T)
loc

kd <- get_map("Amsa-dong", zoom=13, maptype = "roadmap")
kor.map <- ggmap(kd)+ geom_point(data=loc, aes(x=LON, y=LAT), size=3, alpha=0.7, color="red")
kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명), size=3)
ggsave("kd.png",dpi=500)


##practice3
setwd("c:/Rtest")
pop <- read.csv("지역별인구현황_2014_4월기준.csv", header=T)
pop

lon <- pop$LON
lat <- pop$LAT
data <- pop$총인구수

df <- data.frame(lon, lat, data)
df

map1 <- get_map("Jeonju", zoom=7, maptype = "roadmap")
map1 <- ggmap(map1)
map1 + geom_point(aes(x=lon, y=lat, colour=data, size=data), data=df)
ggsave("pop.png", scale=1, width = 7, height = 4, dpi=1000)

map2 <- get_map("Jeonju", zoom=7, maptype = "terrain")
map2 <- ggmap(map2)
map2 + geom_point(aes(x=lon, y=lat, colour=data, size=data),data=df)
ggsave("pop2.png",scale=1, width = 7, height = 4, dpi = 1000)

map3 <- get_map("Jeonju", zoom=7, maptype = "satellite")
map3 <- ggmap(map3)
map3 + geom_point(aes(x=lon, y=lat, colour=data, size=data),data=df)
ggsave("pop3.png",scale=1, width = 7, height = 4, dpi = 1000)

map4 <- get_map("Jeonju", zoom=7, maptype = "hybrid")
map4 <- ggmap(map4)
map4 + geom_point(aes(x=lon, y=lat, colour=data, size=data),data=df)
ggsave("pop4.png",scale=1, width = 7, height = 4, dpi = 700)

map5 <- get_map("Jeonju", zoom=7, maptype = "roadmap")
map5 <- ggmap(map5)
map5 + stat_bin2d(aes(x=lon, y=lat, colour=data, fill=factor(data), size=data),data=df)
ggsave("pop5.png",scale=2, width = 7, height = 4, dpi = 700)

##shiny
# install.packages("shiny")
# library(shiny)
# 
# runExample("01_hello")
# runExample("02_text")
