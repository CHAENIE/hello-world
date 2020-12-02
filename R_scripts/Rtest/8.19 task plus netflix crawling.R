####wordcloud(1)############
library(KoNLP)
library(wordcloud)

useSejongDic()
mergeUserDic(data.frame("xxx","ncn"))

setwd("c://Rtest")
data1 <- readLines("seoul_new.txt")
data1

data2 <- sapply(data1, extractNoun, USE.NAMES = F)
data2
head(unlist(data2),30)
data3 <- unlist(data2)
data3
data3 <-gsub("\\d+","",data3)
data3 <- gsub("서울시","",data3)
data3 <- gsub("서울","",data3)
data3 <- gsub("요청","",data3)
data3 <- gsub("제안","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("-","",data3)
data3

write(unlist(data3),"seoul_2.txt")
data4 <- read.table("seoul_2.txt")
data4

nrow(data4)
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing = T), 20)

data3 <- gsub("OO","",data3)
data3 <- gsub("개선","",data3)
data3 <- gsub("문제","",data3)
data3 <- gsub("관리","",data3)
data3 <- gsub("민원","",data3)
data3 <- gsub("이용","",data3)
data3 <- gsub("관련","",data3)
data3 <- gsub("시장","",data3)
data3
write(unlist(data3),"seoul_3.txt")
data4 <- read.table("seoul_3.txt")
wordcount <- table(data4)
head(sort(wordcount, decreasing = T), 20)
data4

library(RColorBrewer)
palete <- brewer.pal(9,"Set3")
wordcloud(names(wordcount), freq = wordcount, scale = c(5,1), rot.per = 0.25, min.freq = 1,
          random.order = F, random.color = T, colors = palete)
legend(0.3,1,"서울시 응답소 요청사항 분석", cex=0.8, fill=NA, border=NA, bg="white", text.col="red", text.font=2, box.col="red")



#########wordcloud practice(1)#############
getwd()
data1 <- readLines("remake.txt")
data1

data2 <- sapply(data1, extractNoun, USE.NAMES = F)
data2

data3 <- unlist(data2)
data3 <- Filter(function(x) {nchar(x) <=10},data3)
head(unlist(data3),30)
data3

data3 <- gsub("\\d+","",data3)
data3 <- gsub("쌍수","쌍꺼풀",data3)
data3 <- gsub("쌍커풀","쌍꺼풀",data3)
data3 <- gsub("메부리코","매부리코",data3)
data3 <- gsub("\\.","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("\\'","",data3)
data3

write(unlist(data3),"remark_2.txt")
data4 <-read.table("remark_2.txt")
data4
nrow(data4)
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing = T),20)
txt <- readLines("성형gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
i <- 1
for(i in 1:cnt_txt){data3 <- gsub((txt[i]),"",data3)}
data3

data3 <- Filter(function(x) {nchar(x)>=2}, data3)
write(unlist(data3),"remake_2.txt")
data4 <- read.table("remake_2.txt")
data4
nrow(data4)
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing = T),30)

library(RColorBrewer)
palete <- brewer.pal(9, "Set3")
wordcloud(names(wordcount), freq = wordcount, scale = c(3,1), rot.per = 0.25, min.freq = 2,
          random.order = F, random.color = T, colors = palete)
legend(0.3,1, "여고생들이 선호하는 성형수술 부위", cex=0.8, fill=NA, border=NA, bg="white",text.col="black", text.font=2, box.col="red")



###english analysis###
steve <- readLines("steve.txt")
library(tm)
corp99 <- VCorpus(VectorSource(steve))
inspect(corp99)

corp100 <- tm_map(corp99, stripWhitespace)
corp100 <- tm_map(corp100, tolower)
corp100 <- tm_map(corp100, removePunctuation)
corp100 <- tm_map(corp100, removeNumbers)

stopword200 <- c(stopwords('en'),"and","but")
corp100 <- tm_map(corp100, removeWords, stopword200)
corp100 <- tm_map(corp100, PlainTextDocument)
corp101 <- TermDocumentMatrix(corp100)

findFreqTerms(corp101,3)
findAssocs(corp101,"apple",0.5)

corp102 <- as.matrix(corp101)
corp102

freq1 <- sort(rowSums(corp102), decreasing = T)
freq1

library(RColorBrewer)
palete <- brewer.pal(7,"Set3")
wordcloud(names(freq1), freq = freq1, scale = c(5,1), rot.per = 0.25, min.freq = 2,
          random.order = F, random.color = T, colors=palete)
legend(0.3,1, "steve jobs 연설문 분석",cex=0.8, fill=NA, border=NA, bg="white", text.col="red", text.font=2, box.col="red")



####wordcloud(2)##########
data=readLines("crawlling.txt")
head(data)

library(wordcloud)
library(RColorBrewer)
library(KoNLP)
useSejongDic()

data <- sapply(data, extractNoun, USE.NAMES = F)
data_unlist <- unlist(data)
data_unlist <- Filter(function(x) {nchar(x)<=2}, data_unlist)
data_unlist <- gsub('[~!@#$%^&*()_+=?<>]','',data_unlist)
data_unlist <- gsub("\\[","",data_unlist)
data_unlist <- gsub("[ㄱ-ㅎ]","",data_unlist)
data_unlist <- gsub("(ㅜ|ㅠ)","",data_unlist)
data_unlist <- gsub("\\d+","",data_unlist)
data_unlist <- gsub("버스","",data_unlist)
data_unlist <- gsub("대전","",data_unlist)
data_unlist <- gsub("기사","",data_unlist)
data_unlist <- gsub("안녕","",data_unlist)
data_unlist <- gsub("오늘","",data_unlist)
data_unlist <- gsub("감사","",data_unlist)
data_unlist <- gsub("다음","",data_unlist)
data_unlist <- gsub("미준","",data_unlist)
data_unlist <- gsub("하기","",data_unlist)
data_unlist <- gsub("해서","",data_unlist)
data_unlist <- gsub("하시","",data_unlist)
data_unlist <- gsub("때문","",data_unlist)
data_unlist <- gsub("말씀","",data_unlist)
data_unlist <- gsub(",","",data_unlist)
data_unlist <- gsub(" ","",data_unlist)
data_unlist <- gsub("  ","",data_unlist)
data_unlist <- gsub("   ","",data_unlist)
data_unlist <- gsub("     ","",data_unlist)
data_unlist <- gsub("      ","",data_unlist)

head(data_unlist)
wordcount <- table(data_unlist)
head(wordcount)
wordcount_top <- head(sort(wordcount, decreasing = T), 100)
wordcount_top

install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data=wordcount_top)


###########wordcloud(3)################
install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("devtools")
install.packages("htmlwidgets")
install.packages("htmltools")
install.packages("jsonlite")
install.packages("yaml")
library(twitteR)
library(ROAuth)
library(base64enc)
library(devtools)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(tm)
library(wordcloud2)
library(KoNLP)
library(wordcloud)
library(plyr)

consumerKey <- "bG1LSxVffqAcbsTLbkZbu7qx3"
consumerSecret <- "hzu5vaqda9LArKqTL0PXcvm1eacyD2fZy9sbbWvNN47gVjG07j"
accessToken <- "1295912933885583362-56pTRMKNSVcmgIRyvYSI5Shn4Vpy1H"
accessTokenSecret <- "8XiIAEBjBQFcNuHDDpTBXIZ8uBC3juKHA6q2qsYws0VMR"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

keyword <- enc2utf8("AI")
AI <- searchTwitter(keyword, n=5000, lang = "en")
length(AI)
head(AI)

AI_twitter.df <- twListToDF(AI)
AI_twiiter.text <- AI_twitter.df$text

getwd()
setwd("c:/Rtest")

write.table(AI_twiiter.text, "tw.txt")

options(max.print = 10000)

path <- paste0(getwd(),"/tw.txt")
tw_txt <- readLines(path)
tw_txt

tw_txt <- gsub("soleim","",tw_txt)
tw_txt <- gsub('[~!@#$%^&*()_+=?<>]','',tw_txt)

last_number_of_lines <- length(readLines(path))
line_numbers <- seq(1, last_number_of_lines,1)

doc_ids <- line_numbers
df <- data.frame(doc_id=doc_ids, text=tw_txt, stringsAsFactors = FALSE)
tw_data <- Corpus(DataframeSource(df))
inspect(tw_data)

text_delete=c("artificialintelligence","intelligence","artificial","soleim","<U+00A1>","<U+00A6>")
tw_data <- tm_map(tw_data, removeWords, text_delete)

Sys.setlocale(category = "LC_ALL", locale = "us")
tw_data <- tm_map(tw_data, stripWhitespace)
tw_data <- tm_map(tw_data, tolower)
tw_data <- tm_map(tw_data, removeNumbers)
tw_data <- tm_map(tw_data, removeWords, stopwords("english"))
tw_data <- tm_map(tw_data, removePunctuation)
inspect(tw_data)
stopwords("english")
inspect(tw_data)
tdm_tw <- TermDocumentMatrix(tw_data)
TDM1 <- as.matrix((tdm_tw))
v=sort(rowSums(TDM1), decreasing = T)
profile=data.frame(word=names(v), freq=v)
head(profile, 10)

path2 <- paste0(getwd(),"/tw4.csv")
write.csv(profile, path2)
data <- read.csv(path2)

data <- data[,-1]
data <- data[-1,]
data_pick <- subset(data, freq>=30)

head(data_pick, 20)
In_out_colors="function(word, weight){return(weight>100)? '#F3EF12':'#1EC612'}"

pal <- brewer.pal(12,"Paired")
wordcloud2(data_pick, shape = 'circle', size=0.5, color = pal, backgroundColor = "white")




########netflix crawlling###########
setwd("c:/Rtest")
data=readLines("Netflix_review.txt")
head(data)

path <- paste0(getwd(),"/Netflix_review.txt")
NF_txt <- readLines(path)
NF_txt
NF_txt <- gsub("Full Review","",NF_txt)
NF_txt <- Filter(function(x) {nchar(x)>=2}, NF_txt)
NF_txt <- gsub('[~!@#$%^&*()_+=?<>]','',NF_txt)

last_number_of_lines <- length(NF_txt)
line_numbers <- seq(1, last_number_of_lines,1)

doc_ids <- line_numbers
df <- data.frame(doc_id=doc_ids, text=NF_txt, stringsAsFactors = FALSE)
NF_data <- Corpus(DataframeSource(df))

Sys.setlocale(category = "LC_ALL",locale = "us")
NF_data <- tm_map(NF_data, stripWhitespace)
NF_data <- tm_map(NF_data, tolower)
NF_data <- tm_map(NF_data, removeNumbers)
NF_data <- tm_map(NF_data, removeWords, stopwords("english"))
NF_data <- tm_map(NF_data, removePunctuation)
inspect(NF_data)
stopwords("english")
inspect(NF_data)
tdm_NF <- TermDocumentMatrix(NF_data)
TDM1 <- as.matrix((tdm_NF))
v=sort(rowSums(TDM1), decreasing = T)
profile=data.frame(word=names(v), freq=v)
head(profile, 10)

path2 <- paste0(getwd(),"/NF.csv")
write.csv(profile, path2)
data <- read.csv(path2)

data <- data[,-1]
#data <- data[-1,]
data_pick <- subset(data, freq>=10)

head(data)
head(data_pick, 20)
In_out_colors="function(word, weight){return(weight>100)? '#F3EF12':'#1EC612'}"

pal <- brewer.pal(12,"Paired")
wordcloud2(data_pick, shape = 'circle', size=0.5, color = pal, backgroundColor = "white")
