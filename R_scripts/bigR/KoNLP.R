install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages("KoNLP")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useSejongDic()
library(wordcloud)

getwd()
setwd("C:/Rtest")

mergeUserDic(data.frame("빅데이터","ncn"))
txt <- file("bigdata.txt",encoding = "UTF-8")
text <- readLines(txt)
close(txt)
text
big <- sapply(text,extractNoun, USE.NAMES = F)
big

head(unlist(big),30)
f <- unlist(big)
big <- Filter(function(x) {nchar(x)>=3},f)

write(unlist(big),"bigdata_2.txt")
re <- read.table("bigdata_2.txt")
nrow(re)
textcount <- table(re)
head(sort(textcount,decreasing = T),30)

palete <- brewer.pal(9,"Set1")

wordcloud(names(textcount),freq = textcount, scale = c(5,1), rot.per = 0.25, min.freq = 1, random.order = F, random.color = T, colors = palete)

?brewer.pal
