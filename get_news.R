install.packages("XML")
install.packages("Rcurl")
install.packages("vegan")
install.packages("RCurl")

library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)
library(vegan)

rm(fau)

words <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9912")
words <- toTrad(words)
insertWords(words)


#insertWords(words)
myStopWords <- c(toTrad(stopwordsCN()), "編輯", "時間", "標題", "發信", "實業", "作者", "要聞", "即時新聞", "聯合新聞網", "全文網址", "全文", "網址", 
                                 "大家", "今天", "知道", "非常", "很多", "現在", "希望", "不要", "已經", "看到", "謝謝", "其實", "事情")

head(myStopWords,100)


getwd()

#apple news 抓取
setwd("C:/Users/user/Documents/Apple_news")
for( i in 928855:928957){ #蘋果文章編號
  tmp <- paste(i, '/', sep='')
  url <- paste('http://www.appledaily.com.tw/realtimenews/article/hot/20160815/', tmp, sep='')
  html <- htmlParse(getURL(url),encoding='UTF-8')
  doc <- xpathSApply(html, "//p[@id='summary']", xmlValue)
  write(doc, paste("201608015_Apple_life新聞",'.txt',sep=" "),append=TRUE)
  #write(doc, paste(i,'.txt',sep=""))
}
setwd("../")


#把文章進行分詞, 匯出名詞的頻率

myText <- function(fileDir="C:/Users/user/Documents/Apple_news", output="apple-freq.txt"){
d.corpus <- Corpus(DirSource(fileDir), list(language = NA))
d.corpus <- tm_map(d.corpus, removePunctuation) #清除標點符號
d.corpus <- tm_map(d.corpus, removeNumbers) #清除數字
d.corpus <- tm_map(d.corpus, function(word) { #清除英文字母
  gsub("[A-Za-z0-9]", "", word)
})
d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[(names(w) == "n")] # 只比較名詞
  })
  unlist(noun)
})


#d.corpus <- Corpus(VectorSource(d.corpus))
d.corpus <- Corpus(DataframeSource(data.frame(as.character(d.corpus))))
d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
write.table(data.frame(freq=d$freq, word=d$word), file=output,quote=FALSE,sep="\t",row.names=FALSE, col.names=FALSE)
data.frame(freq=d$freq, word=d$word)
}


fau<-list()
fau[[1]]<-myText(fileDir="Apple_news", output="apple-freq.txt")


#文字雲
myPng <- function(input=fau0, output="wordcloud.png",min.freq=2){
  library(RColorBrewer)
  pal2 <- brewer.pal(8,"Dark2")
  par(family="Arial Unicode MS")
  png(output, width=800,height=800)
  wordcloud(input$word,input$freq, scale=c(10,.4),min.freq=min.freq,
            max.words=Inf, random.order=FALSE, colors=pal2)
  dev.off()
}
myPng(fau[[1]], "applenews.png")
#====================================================================
u <- as.character(unique(unlist(lapply(fau, function(x)x$word))))
tmp <- out <- list()
tmp <- lapply(fau, function(x) rep(x$word, x$freq))
out <- lapply(tmp, function(x) table(factor(x, levels=u, labels=u)))
tab <- do.call("rbind", out)
sim <- c(1 - vegdist(tab, method="morisita")) #兩兩相似矩陣
k <- 0
edges <- data.frame()
for(i in 1:(nrow(tab)-1)){
  for(j in (i+1):nrow(tab)){
    k <- k +1
    tmp <- data.frame("Source"=i, "Target"=j, Type="Undirected", "Weight"=sim[k])
    edges <- rbind(edges, tmp)
  }
}

edges

node <- data.frame("id"=1:nrow(tab), "x"=rep(1/nrow(tab),nrow(tab)))

write.csv(node, "node.csv")
write.csv(edges, "edges.csv")

