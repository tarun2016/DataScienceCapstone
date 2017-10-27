#Load Library
library(tm)
library(openNLP)
library(qdap)
library(RWeka)
library(shinythemes)
library(shiny)
library(tidyr)
library(dplyr)
#Import data 
blogs <- readLines("C:\\Users\\emfxv\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt", encoding="UTF-8")
twitter <- readLines("C:\\Users\\emfxv\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt", encoding="UTF-8")
news <- readLines("C:\\Users\\emfxv\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt", encoding="UTF-8")
#Sample data
sample_blogs   <- sample(blogs, 1000)
sample_news    <- sample(news, 1000)
sample_twitter <- sample(twitter, 1000)
sample <- c(sample_blogs,sample_news,sample_twitter)
sample<- iconv(sample, from = "UTF-8", to = "ASCII", sub = "")
#Create Corpus and clean data 
corpus <- VCorpus(VectorSource(sample)) 
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
#Create unigram matrix
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
tdm1 <- TermDocumentMatrix(corpus, control=list(tokenize=UnigramTokenizer))
tdm1_matrix<-as.matrix(tdm1)
#Create Bigram matrix
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
tdm2 <- TermDocumentMatrix(corpus, control=list(tokenize=BigramTokenizer))
tdm2_matrix<-as.matrix(tdm2)
#Create Trigram matrix
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tdm3 <- TermDocumentMatrix(corpus, control=list(tokenize=TrigramTokenizer))
tdm3_matrix<-as.matrix(tdm3)
#Create Quadgram matrix
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
tdm4 <- TermDocumentMatrix(corpus, control=list(tokenize=QuadgramTokenizer))
tdm4_matrix<-as.matrix(tdm4)
#Create Pentgram matrix
PentgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=5, max=5))
tdm5 <- TermDocumentMatrix(corpus, control=list(tokenize=PentgramTokenizer))
tdm5_matrix<-as.matrix(tdm5)
#Create data frame with freq
uni_freq<-data.frame(rowSums(tdm1_matrix))
uni_freq$uni<-rownames(uni_freq)
colnames(uni_freq)[1]<-"Frequency"
rownames(uni_freq)<-NULL
uni_freq$Frequency<-as.numeric(uni_freq$Frequency)
uni_data<-uni_freq[order(uni_freq$Frequency,decreasing = TRUE),]
bi_freq<-data.frame(rowSums(tdm2_matrix))
bi_freq$bi<-rownames(bi_freq)
colnames(bi_freq)[1]<-"Frequency"
rownames(bi_freq)<-NULL
bi_data<-bi_freq %>% separate(bi, into = c("word1","word2"))
bi_data$Frequency<-as.numeric(bi_data$Frequency)
bi_data<-bi_data[order(bi_data$Frequency,decreasing = TRUE),]
tri_freq<-data.frame(rowSums(tdm3_matrix))
tri_freq$tri<-rownames(tri_freq)
colnames(tri_freq)[1]<-"Frequency"
rownames(tri_freq)<-NULL
tri_data<-tri_freq %>% separate(tri, into = c("word1","word2","word3"))
tri_data$Frequency<-as.numeric(tri_data$Frequency)
tri_data<-tri_data[order(tri_data$Frequency,decreasing = TRUE),]
quad_freq<-data.frame(rowSums(tdm4_matrix))
quad_freq$quad<-rownames(quad_freq)
colnames(quad_freq)[1]<-"Frequency"
rownames(quad_freq)<-NULL
quad_data<-quad_freq %>% separate(quad, into = c("word1","word2","word3","word4"))
quad_data$Frequency<-as.numeric(quad_data$Frequency)
quad_data<-quad_data[order(quad_data$Frequency,decreasing = TRUE),]
pent_freq<-data.frame(rowSums(tdm5_matrix))
pent_freq$pent<-rownames(pent_freq)
colnames(pent_freq)[1]<-"Frequency"
rownames(pent_freq)<-NULL
pent_data<-pent_freq %>% separate(pent, into = c("word1","word2","word3","word4","word5"))
pent_data$Frequency<-as.numeric(pent_data$Frequency)
pent_data<-pent_data[order(pent_data$Frequency,decreasing = TRUE),]
save(uni_data,file="uni_data.Rda")
save(bi_data,file="bi_data.Rda")
save(tri_data,file="tri_data.Rda")
save(quad_data,file="quad_data.Rda")
save(pent_data,file="pent_data.Rda")

