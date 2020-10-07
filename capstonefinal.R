# set up workspace
setwd("~")
source("tpsetup.R")
library("ngram")
library("dplyr")
library("ggplot2")
library("tm")
library("wordcloud")

# load samples to predict from
samples<-readLines("samples.txt",encoding = "UTF-8")

# text prediction function
tp<-function(sentence) {

# if no text is entered, return 5 most common unigrams from sample
if (missing("sentence")||!exists("sentence")||trimws(sentence)=="") {
(unlist(strsplit("the to and a of"," ")))
} else {
  
# clean input sentence
  full<-stripWhitespace(sentence)
  full<-tolower(full)
  full<-removePunctuation(full)
  full<-gsub("[^[:alpha:][:space:]]*","", full)
  full<-stripWhitespace(full)
  full<-unlist(strsplit(full," "))

# find predictive end(s) of phrase (of different lengths) and
# subset out sample lines that include predictive end(s) of phrase
if(isTRUE(length(full)>=1)) {
endof1<-paste(full[(length(full)-(1-1)):(length(full))],collapse=" ")
search1<-paste("\\<",endof1,"\\s","\\w",sep="")
sentencesamples1<-samples[grep(search1, samples)]
}
if(isTRUE(length(full)>=2)) {
endof2<-paste(full[(length(full)-(2-1)):(length(full))],collapse=" ")
search2<-paste("\\<",endof2,"\\s","\\w",sep="")
sentencesamples2<-sentencesamples1[grep(search2, sentencesamples1)]
}
if(isTRUE(length(full)>=3)) {
endof3<-paste(full[(length(full)-(3-1)):(length(full))],collapse=" ")
search3<-paste("\\<",endof3,"\\s","\\w",sep="")
sentencesamples3<-sentencesamples2[grep(search3, sentencesamples2)]
}
if(isTRUE(length(full)>=4)) {
endof4<-paste(full[(length(full)-(4-1)):(length(full))],collapse=" ")
search4<-paste("\\<",endof4,"\\s","\\w",sep="")
sentencesamples4<-sentencesamples3[grep(search4, sentencesamples3)]
}

# find different order ngrams within subset samples, 
# subset out just predictive end(s) of phrase,
# then calculate weighted score for each predicted next word,
# giving greater weight for higher order ngrams
if(!exists("ng")&&exists("sentencesamples4")&&length(sentencesamples4)>0) {
ngram4<-get.phrasetable(ngram(sentencesamples4,4+1))
ngram4<-ngram4[grep(search4,ngram4$ngrams),]
ngram4$nextword<-trimws(removeWords(ngram4$ngrams,endof4))
ngram4$score<-(ngram4$freq/sum(ngram4$freq))
ngram4<-cbind(ngram4$nextword,ngram4$score)
ng<-rbind.data.frame(ngram4, stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples3"))&&length(sentencesamples3)>0||
      (exists("ng")&&nrow(ng)<5)) {
ngram3<-get.phrasetable(ngram(sentencesamples3,3+1))
ngram3<-ngram3[grep(search3,ngram3$ngrams),]
ngram3$nextword<-trimws(removeWords(ngram3$ngrams,endof3))
ngram3$score<-(ngram3$freq/sum(ngram3$freq))*.5
ngram3<-cbind(ngram3$nextword,ngram3$score)
ng<-rbind.data.frame(ngram3,stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples2"))&&length(sentencesamples2)>0||
      (exists("ng")&&nrow(ng)<5)) {
ngram2<-get.phrasetable(ngram(sentencesamples2,2+1))
ngram2<-ngram2[grep(search2,ngram2$ngrams),]
ngram2$nextword<-trimws(removeWords(ngram2$ngrams,endof2))
ngram2$score<-(ngram2$freq/sum(ngram2$freq))*.25
ngram2<-cbind(ngram2$nextword,ngram2$score)
ng<-rbind.data.frame(ngram2, stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples1"))&&length(sentencesamples1)>0||
      (exists("ng")&&nrow(ng)<5)) {
ngram1<-get.phrasetable(ngram(sentencesamples1,1+1))
ngram1<-ngram1[grep(search1,ngram1$ngrams),]
ngram1$nextword<-trimws(removeWords(ngram1$ngrams,endof1))
ngram1$score<-(ngram1$freq/sum(ngram1$freq))*.125
ngram1<-cbind(ngram1$nextword,ngram1$score)
ng<-rbind.data.frame(ngram1, stringsAsFactors = F)
}

if(exists("ng")) {
# make new ngram dataframe with nextwords, weighted scores
colnames(ng)<-c("nextword","score")
ng$score<-as.numeric(ng$score)

# group to combine repeat words, print words with 5 highest mean scores
ng<-group_by(ng, nextword)
suppressMessages(ng<-summarize(ng,mean(score)))
colnames(ng)<-c("nextword","meanscore")
ng<-arrange(ng,desc(meanscore))
result<-ng$nextword[1:5]
}
  
# if no ngrams from sentence ends found, return sample's most common unigrams  
if(!exists("ng")) {
result<-(unlist(strsplit("the to and a of"," ")))
}

result
}}







# text prediction word plot  function
# this is the same as tp but outputs wordplot of up to 25 candidates 
# for next word vs mean score for that word
tpwordplot<-function(sentence) {

# if no text is entered, send message
if (missing("sentence")||!exists("sentence")||trimws(sentence)=="") {
wordplot<-ggplot()+theme_void()+
    labs(title="No unique ngram predictions for this phrase. Please enter new phrase.")
} else {
  
# clean input sentence
  full<-stripWhitespace(sentence)
  full<-tolower(full)
  full<-removePunctuation(full)
  full<-gsub("[^[:alpha:][:space:]]*","", full)
  full<-stripWhitespace(full)
  full<-unlist(strsplit(full," "))

# find predictive end(s) of phrase (of different lengths) and
# subset out sample lines that include predictive end(s) of phrase
if(isTRUE(length(full)>=1)) {
endof1<-paste(full[(length(full)-(1-1)):(length(full))],collapse=" ")
search1<-paste("\\<",endof1,"\\s","\\w",sep="")
sentencesamples1<-samples[grep(search1, samples)]
}
if(isTRUE(length(full)>=2)) {
endof2<-paste(full[(length(full)-(2-1)):(length(full))],collapse=" ")
search2<-paste("\\<",endof2,"\\s","\\w",sep="")
sentencesamples2<-sentencesamples1[grep(search2, sentencesamples1)]
}
if(isTRUE(length(full)>=3)) {
endof3<-paste(full[(length(full)-(3-1)):(length(full))],collapse=" ")
search3<-paste("\\<",endof3,"\\s","\\w",sep="")
sentencesamples3<-sentencesamples2[grep(search3, sentencesamples2)]
}
if(isTRUE(length(full)>=4)) {
endof4<-paste(full[(length(full)-(4-1)):(length(full))],collapse=" ")
search4<-paste("\\<",endof4,"\\s","\\w",sep="")
sentencesamples4<-sentencesamples3[grep(search4, sentencesamples3)]
}

# find different order ngrams within subset samples, 
# subset out just predictive end(s) of phrase,
# then calculate weighted score for each predicted next word,
# giving greater weight for higher order ngrams
if(!exists("ng")&&exists("sentencesamples4")&&length(sentencesamples4)>0) {
ngram4<-get.phrasetable(ngram(sentencesamples4,4+1))
ngram4<-ngram4[grep(search4,ngram4$ngrams),]
ngram4$nextword<-trimws(removeWords(ngram4$ngrams,endof4))
ngram4$score<-(ngram4$freq/sum(ngram4$freq))
ngram4<-cbind(ngram4$nextword,ngram4$score)
ng<-rbind.data.frame(ngram4, stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples3"))&&length(sentencesamples3)>0||
   (exists("ng")&&nrow(ng)<5)) {
ngram3<-get.phrasetable(ngram(sentencesamples3,3+1))
ngram3<-ngram3[grep(search3,ngram3$ngrams),]
ngram3$nextword<-trimws(removeWords(ngram3$ngrams,endof3))
ngram3$score<-(ngram3$freq/sum(ngram3$freq))*.5
ngram3<-cbind(ngram3$nextword,ngram3$score)
ng<-rbind.data.frame(ngram3,stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples2"))&&length(sentencesamples2)>0||
      (exists("ng")&&nrow(ng)<5)) {
ngram2<-get.phrasetable(ngram(sentencesamples2,2+1))
ngram2<-ngram2[grep(search2,ngram2$ngrams),]
ngram2$nextword<-trimws(removeWords(ngram2$ngrams,endof2))
ngram2$score<-(ngram2$freq/sum(ngram2$freq))*.25
ngram2<-cbind(ngram2$nextword,ngram2$score)
ng<-rbind.data.frame(ngram2, stringsAsFactors = F)
}
if((!exists("ng")&&exists("sentencesamples1"))&&length(sentencesamples1)>0||
      (exists("ng")&&nrow(ng)<5)) {
ngram1<-get.phrasetable(ngram(sentencesamples1,1+1))
ngram1<-ngram1[grep(search1,ngram1$ngrams),]
ngram1$nextword<-trimws(removeWords(ngram1$ngrams,endof1))
ngram1$score<-(ngram1$freq/sum(ngram1$freq))*.125
ngram1<-cbind(ngram1$nextword,ngram1$score)
ng<-rbind.data.frame(ngram1, stringsAsFactors = F)
}

if(exists("ng")) {
# make new ngram dataframe with nextwords, weighted scores
colnames(ng)<-c("nextword","score")
ng$score<-as.numeric(ng$score)

# group to combine same words, creat ggplot of up to 25 next words with
# highest mean scores
ng<-group_by(ng, nextword)
suppressMessages(ng<-summarize(ng,mean(score)))
colnames(ng)<-c("nextword","meanscore")
ng<-arrange(ng,desc(meanscore))
wordplot<-ggplot(top_n(ng,25,meanscore),
                aes(reorder(nextword,-meanscore), meanscore))+
                geom_bar(stat="identity",fill="deeppink")+
                labs(x="Word",y="Mean Score",title="Most Likely Next Words For This Sentence")+
                theme(axis.text.x=element_text(angle=45, hjust=1))
}
  
# if still no ngrams from sentence, send message
if(!exists("ng")) {
wordplot<-ggplot()+theme_void()+
    labs(title="No unique ngram predictions for this phrase. Please enter new phrase.")

}
}
wordplot}