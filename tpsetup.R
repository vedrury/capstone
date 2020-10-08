# set up
# add libraries, download data
setwd("~")
library("ngram")
library("dplyr")
library("ggplot2")
library("tm")

if(!file.exists("samples.zip")) {
        
if(!file.exists("data.zip")) {
        print("Downloading data...")
url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url, destfile = "data.zip", method="curl")
unzip("data.zip")  }
suppressWarnings(blogs<-readLines(con = "final/en_US/en_US.blogs.txt", encoding = "UTF-8"))
suppressWarnings(twitter<-readLines(con = "final/en_US/en_US.twitter.txt", encoding = "UTF-8"))
suppressWarnings(news<-readLines(con = "final/en_US/en_US.news.txt", encoding = "UTF-8"))


# create samples, clean samples
set.seed(1239)
bsample<-sample(blogs,size=round(length(blogs)/10),replace=F)
tsample<-sample(twitter,size=round(length(twitter)/10),replace=F)
nsample<-sample(news,size=round(length(news)/10),replace=F)
samples<-c(bsample,tsample,nsample)

print("Cleaning data...")
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
zip("samples.zip","samples.txt")
unlink("samples.txt")


} else {print("sample.zip already exists")}
