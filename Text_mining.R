library(Rwordseg)
library(rjson)
library(tm)
library(wordcloud)

getwd()
article.news.path<-file.path("Apple_news","928550.txt")
data.news<-readLines("Apple_news/201608015_Apple_life新聞 .txt")

myStopWords <- c(toTrad(stopwordsCN()), "編輯", "時間", "標題", "發信", "實業", "作者", "要聞", "即時新聞", "聯合新聞網", "全文網址", "全文", "網址", 
                 "大家", "今天", "知道", "非常", "很多", "現在", "希望", "不要", "已經", "看到", "謝謝", "其實", "事情")


#data.news

#segment.options(isNameRecognion=TRUE) 會把疑似是人名的詞抓出來
#insertWords(c(toTrad("黃姓"))) 自己添詞彙

data.news.segmentCN<-segmentCN(data.news,nature = FALSE)
segmentCN(data.news)
data.news.Corpus<-Corpus(VectorSource(data.news.segmentCN))
inspect(data.news.Corpus)

#文字清理
data.news.Corpus <- tm_map(data.news.Corpus, removePunctuation) #清除標點符號
data.news.Corpus <- tm_map(data.news.Corpus, removeNumbers) #清除數字
data.news.Corpus <- tm_map(data.news.Corpus, function(word) { #清除英文字母
  gsub("[A-Za-z0-9]", "", word)
})


data.news.Corpus <- Corpus(DataframeSource(data.frame(as.character(data.news.Corpus))))
tdm <- TermDocumentMatrix(data.news.Corpus, control = list(wordLengths = c(2, Inf)))
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
write.table(data.frame(freq=d$freq, word=d$word), file="R_apple_news",quote=FALSE,sep="\t",row.names=FALSE, col.names=FALSE)
data.frame(freq=d$freq, word=d$word)


data.news.DTM<-DocumentTermMatrix(data.news.Corpus)
View(as.matrix(data.news.DTM))

findFreqTerms(data.news.DTM,lowfreq =20)



#文字雲

#====================================================================

#TFID 字詞獨特性