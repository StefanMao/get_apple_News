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
myStopWords <- c(toTrad(stopwordsCN()), "�s��", "�ɶ�", "���D", "�o�H", "��~", "�@��", "�n�D", "�Y�ɷs�D", "�p�X�s�D��", "������}", "����", "���}", 
                                 "�j�a", "����", "���D", "�D�`", "�ܦh", "�{�b", "�Ʊ�", "���n", "�w�g", "�ݨ�", "����", "���", "�Ʊ�")

head(myStopWords,100)


getwd()

#apple news ���
setwd("C:/Users/user/Documents/Apple_news")
for( i in 928855:928957){ #ī�G�峹�s��
  tmp <- paste(i, '/', sep='')
  url <- paste('http://www.appledaily.com.tw/realtimenews/article/hot/20160815/', tmp, sep='')
  html <- htmlParse(getURL(url),encoding='UTF-8')
  doc <- xpathSApply(html, "//p[@id='summary']", xmlValue)
  write(doc, paste("201608015_Apple_life�s�D",'.txt',sep=" "),append=TRUE)
  #write(doc, paste(i,'.txt',sep=""))
}
setwd("../")


#��峹�i�����, �ץX�W�����W�v

myText <- function(fileDir="C:/Users/user/Documents/Apple_news", output="apple-freq.txt"){
d.corpus <- Corpus(DirSource(fileDir), list(language = NA))
d.corpus <- tm_map(d.corpus, removePunctuation) #�M�����I�Ÿ�
d.corpus <- tm_map(d.corpus, removeNumbers) #�M���Ʀr
d.corpus <- tm_map(d.corpus, function(word) { #�M���^��r��
  gsub("[A-Za-z0-9]", "", word)
})
d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[(names(w) == "n")] # �u����W��
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


#��r��
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
sim <- c(1 - vegdist(tab, method="morisita")) #���ۦ��x�}
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
