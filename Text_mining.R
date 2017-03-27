library(Rwordseg)
library(rjson)
library(tm)
library(wordcloud)

getwd()
article.news.path<-file.path("Apple_news","928550.txt")
data.news<-readLines("Apple_news/201608015_Apple_life�s�D .txt")

myStopWords <- c(toTrad(stopwordsCN()), "�s��", "�ɶ�", "���D", "�o�H", "��~", "�@��", "�n�D", "�Y�ɷs�D", "�p�X�s�D��", "������}", "����", "���}", 
                 "�j�a", "����", "���D", "�D�`", "�ܦh", "�{�b", "�Ʊ�", "���n", "�w�g", "�ݨ�", "����", "���", "�Ʊ�")


#data.news

#segment.options(isNameRecognion=TRUE) �|��æ��O�H�W������X��
#insertWords(c(toTrad("���m"))) �ۤv�K���J

data.news.segmentCN<-segmentCN(data.news,nature = FALSE)
segmentCN(data.news)
data.news.Corpus<-Corpus(VectorSource(data.news.segmentCN))
inspect(data.news.Corpus)

#��r�M�z
data.news.Corpus <- tm_map(data.news.Corpus, removePunctuation) #�M�����I�Ÿ�
data.news.Corpus <- tm_map(data.news.Corpus, removeNumbers) #�M���Ʀr
data.news.Corpus <- tm_map(data.news.Corpus, function(word) { #�M���^��r��
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



#��r��

#====================================================================

#TFID �r���W�S��