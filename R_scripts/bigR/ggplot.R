library(ggplot2)
test.data <- data.frame(length=c(3,2,5,8),
                        width=c(4,3,6,9), depth=c(5,2,16,80), trt=c("a","a","b","b"))
ggplot(test.data, aes(x=length, y=width)) + geom_point(aes(colour=trt))
ggplot(test.data, aes(x=length, y=width)) + geom_point(aes(colour=trt)) + geom_smooth()

ggplot(data = diamonds, aes(x=carat, y=price)) + geom_point(aes(colour=clarity))
ggplot(data = diamonds, aes(x=carat, y=price)) + geom_point(aes(colour=clarity)) + geom_smooth()
ggplot(data = diamonds, aes(x=carat, y=price, colour=clarity)) + geom_point() + geom_smooth()

ggplot(data = diamonds, aes(x=carat, y=price)) + geom_smooth()
ggplot(data = diamonds, aes(x=carat, y=price)) + geom_smooth(aes(group=clarity))

ggplot(data = diamonds, aes(x=price)) + geom_bar()
ggplot(data = diamonds, aes(x=price)) + stat_bin(geom = "bar", fill="gold", col="white")

ggplot(data = diamonds, aes(clarity, fill=cut)) + geom_bar()
qplot(clarity, data = diamonds, fill=cut, geom="bar")

