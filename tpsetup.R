# set up
# add libraries, download data
setwd("~")
library("ngram")
library("dplyr")
library("ggplot2")
library("tm")
library("wordcloud")

if(!file.exists("samples.txt")) {
url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url, destfile = "data.zip", method="curl")
unzip("data.zip")

suppressWarnings(blogs<-readLines(con = "final/en_US/en_US.blogs.txt", encoding = "UTF-8"))
suppressWarnings(twitter<-readLines(con = "final/en_US/en_US.twitter.txt", encoding = "UTF-8"))
suppressWarnings(news<-readLines(con = "final/en_US/en_US.news.txt", encoding = "UTF-8"))

# create samples, clean samples
set.seed(1239)
bsample<-sample(blogs,size=500000,replace=F)
tsample<-sample(twitter,size=500000,replace=F)
nsample<-sample(news,size=500000,replace=F)
samples<-paste(bsample,tsample,bsample)

profurl<-"http://www.bannedwordlist.com/lists/swearWords.txt"
download.file(profurl, destfile = "swearWords.txt")
profanity<-read.table("swearWords.txt")

samples<-gsub(" ?(f|[Hh]t)tp(s?)://(.*)[.][a-z]+", " ", samples)
samples<-tolower(samples)
samples<-gsub("\\-", " ", samples)
samples<-removePunctuation(samples)
samples<-removeWords(samples, as.character(profanity[,1]))
samples<-gsub("[^[:alpha:][:space:]]*", "", samples)
samples<-stripWhitespace(samples)
samples<-trimws(samples)

write(samples,"samples.txt")
}
