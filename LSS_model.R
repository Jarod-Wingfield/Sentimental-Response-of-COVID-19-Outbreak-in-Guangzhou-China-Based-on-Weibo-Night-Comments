# clear the working environment
rm(list=ls())

# set up the working directory
setwd("F:/BIGDATA_R/Covid_project")
getwd()

library(pacman)
p_load("dplyr","tidytext","tidyr","reshape2","wordcloud","here", "tidyverse", "ggplot2",
       "lubridate","quanteda", "quanteda.textplots", "quanteda.textstats","patchwork",
       "readxl","plyr","dygraphs","xts","hrbrthemes","topicmodels")
# install.packages("jiebaR")
library("jiebaR")

# Import data
wb_comment <- readRDS("wb_comment_covidGZ(New).rds")
# 一级评论
# filter(Keywords=="广州疫情") %>%

wb_comment <- wb_comment %>%
  filter(!duplicated(text)) %>%
  select(created_at,text)
wb_comment$created_at <-as.Date(substring(wb_comment$created_at,1,6), format = "%m月%d日")
colnames(wb_comment)[1] <- 'date'

wb_comment <- corpus(wb_comment)

########################################## LSS ###########################################

# tokenize text corpus and remove various features
corp_sent <- corpus_reshape(wb_comment, to =  "sentences")
toks_sent <- corp_sent %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_numbers = TRUE, remove_url = TRUE) %>%
  tokens_remove(stopwords::stopwords("zh_cn", source = "marimo"), min_nchar = 2) %>%
  tokens_keep(pattern = "^\\p{script=Hani}+$", valuetype = 'regex') %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com"))
# 去除tokens内的空白文本（但其字符串字数却不为0）,所以使用ntoken()
toks_sent<-toks_sent[ntoken(toks_sent)!=0]

# create a document feature matrix from the tokens object
dfmat_sent <- toks_sent %>%
  dfm() %>%
  dfm_remove(pattern = "") %>%
  dfm_trim(min_termfreq = 5)

# 去除所有均为0的行
raw.sum<-apply(dfmat_sent,1,FUN=sum) #sum by raw each raw of the table
dfmat_sent<-dfmat_sent[raw.sum!=0,]

topfeatures(dfmat_sent, 20)

# install.packages("LSX")
require(LSX)

# Making the sentiment seed words dictionary
dllg <- read_excel("./DLUT-Emotionontology-master/情感词汇/情感词汇.xlsx")
neg<- dllg %>%
  filter(sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA"))%>%
  select(words,power)
neg$power <- -neg$power
pos<- dllg %>%
  filter(!sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA"))%>%
  select(words,power)

key<-c("positive","negative")
value<-c(list(pos$words),list(neg$words))
# Generate a dictionary
# vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")
# initialize hash
hash = new.env(hash = TRUE, parent = emptyenv(), size = 100L)
# assign values to keys
d<-assign_hash(key,value, hash)

# Seed words
# seed1: 1 and -1 ; seed2: 大连理工情感词典的power
seed1<-as.seedwords(d)

# x1<-pos$power
# attr(x1,"names")<-pos$words
# x2<-neg$power
# attr(x2,"names")<-neg$words
# seed2<-c(x1,x2)

# run LSS model
set.seed(624)
tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed1,
                          k = 300, cache = TRUE)
tmod_lss <- readRDS("F:/BIGDATA_R/Covid_project/lss_cache/svds_9d2fa9648401c519.RDS")
# tmod_lss2 <- textmodel_lss(dfmat_sent, seeds = seed2,
#                            k = 300, cache = TRUE)
head(coef(tmod_lss), 20) # most positive words
tail(coef(tmod_lss), 20) # most negative words

# textplot_terms(tmod_lss, tail(coef(tmod_lss), 20))

# Calculate the score
dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss, newdata = dfmat_doc)

dat_smooth <- smooth_lss(dat, engine = "locfit")
head(dat_smooth)

# Figure of LSS result and covid situation from 2022/9/1 to 2022/12/1
Sys.setlocale("LC_TIME","English")
LSS_curve<-
  ggplot()+
  geom_point(data=dat,aes(x=date,y=fit),alpha=0.3,color="#66CCFF")+
  geom_line(data=dat_smooth,aes(x=date,y=fit),size=1)+
  geom_line(data=dat_smooth,aes(x=date,y=fit-2*se.fit))+
  geom_line(data=dat_smooth,aes(x=date,y=fit+2*se.fit))+
  scale_linetype_manual(values = c('solid', 'dashed', 'dashed'))+
  coord_cartesian(ylim = c(-0.5,0.5),xlim = c(as_date("2022-09-01"),as_date("2022-12-01")))+
  xlab("Data")+
  ylab("Score")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = as_date("2022-10-27"),size=1,colour = "red")+
  geom_vline(xintercept = as_date("2022-11-17"),size=1,colour = "red")+
  geom_text(aes(x = as_date("2022-11-7"),y=0.4), label="Soaring phase",size = 6, colour = "red")+
  ggtitle("Sentiment Score by LSS")+
  theme_bw() +
  theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x=element_text(vjust=1,
                              size=14),  # X axis title
    axis.title.y=element_text(size=14),  # Y axis title
    axis.text.x=element_text(size=11,
                             angle = 45,
                             vjust=.5),  # X axis text
    axis.text.y=element_text(size=12))  # Y axis text

# 将sentiment_net与疫情数据结合到一个图
covid <- read_excel("covid_Guangzhou(New).xls")

# Then you can create the xts necessary to use dygraph
NLCC <- xts(x = covid$New_local_confirmed_cases, order.by = covid$Time)
NLAI <- xts(x = covid$New_local_asymptomatic_infections, order.by = covid$Time)
nlcc<-data.frame(date = index(NLCC),
                 NLCC, row.names=NULL)
nlai<-data.frame(date = index(NLAI),
                 NLAI, row.names=NULL)
m1 <- merge(nlcc, nlai, by.x = "date", by.y = "date")
m1$date<-as.Date(m1$date)

covid <- ggplot(m1,aes(x=date)) +
  geom_line(aes(y=NLAI,colour="New local asymptomatic infections"),size=0.8) +
  geom_line(aes(y=NLCC,colour="New local confirmed cases"),size=0.8) +
  theme_bw() +
  scale_y_continuous(expand = c(0,0),limits = c(0,9000),name = "Amount(Person)")+
  xlab("Data")+
  ggtitle("Covid-19 in Guangzhou China")+
  theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x=element_text(vjust=1,
                              size=14),  # X axis title
    axis.title.y=element_text(size=14),  # Y axis title
    axis.text.x=element_text(size=11,
                             angle = 45,
                             vjust=.5),  # X axis text
    axis.text.y=element_text(size=12))  # Y axis text

LSS_curve/covid


# ####################################### Average Sentiment Score ###############################################
# pos.words <- d[['positive']]
# neg.words <- d[['negative']]
# # a look at a random sample of positive and negative words
# sample(pos.words, 10)
# sample(neg.words, 10)
#
# dfm<-dfm_tfidf(dfmat_sent)
#
# # number of positive terms in sentence
# positive_terms_in_dfm <- intersect(colnames(dfm), pos.words)
# counts_positive <- rowSums(dfm[, positive_terms_in_dfm])
#
# # number of negative terms in sentence
# negative_terms_in_dfm <- intersect(colnames(dfm), neg.words)
# counts_negative <- rowSums(dfm[, negative_terms_in_dfm])
#
# # total number of terms
# counts_all_terms <- rowSums(dfm)
#
# # sentiment score by docs
# relative_sentiment_frequencies <- data.frame(
#   positive = counts_positive / counts_all_terms,
#   negative = counts_negative / counts_all_terms,
#   score = (counts_positive - counts_negative) / counts_all_terms)
# # average sentiment score by date
# sentiments_per_day <- aggregate(relative_sentiment_frequencies, by=list(day=dfm@docvars$date), mean)
#
# head(sentiments_per_day)
#
# # plot the average sentiment score by date
# p1<-ggplot(sentiments_per_day, aes(x=day, y=score)) +
#   geom_point()+
#   geom_smooth()+
#   theme_minimal()
#
# p1

# ##################################### Weighted Sentiment Score #############################################
# p_load("sentometrics")
# dict <- rbind(tibble(word=pos$words,score=pos$power),
#               tibble(word=neg$words,score=neg$power))
# lex<-sento_lexicons(list(L=dict))
# corpusSample<-corp_sent
# sentW<-compute_sentiment(corpusSample,lex,how="TFIDF")
# sentW<-cbind(as.data.frame(sentW),docvars(corpusSample))
#
# sentW_per_day <- aggregate(sentW, by=list(day=sentW$date), mean)
#
# p2<-ggplot(sentW,aes(x=date,y=sentW[,3],alpha=0.3))+
#   geom_point()+
#   geom_smooth(method="loess",span=0.1,se=F)+
#   theme(legend.position = "none") +
#   coord_cartesian(ylim = c(-1,10))+
#   xlim(as_date("2022-09-30"),as_date("2022-12-01"))+
#   xlab("Data")+
#   ylab("Score")+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = as_date("2022-10-27"))+
#   geom_vline(xintercept = as_date("2022-11-17"))+
#   geom_text(aes(x = as_date("2022-11-8"),y=0.4), label="Soaring phase")+
#   ggtitle("Weighted Sentiment Score")+
#   theme_bw() +
#   theme(
#     legend.title = element_text(size=12),
#     legend.text = element_text(size=12),
#     plot.title = element_text(size = 20, face = "bold"),
#     axis.title.x=element_text(vjust=1,
#                               size=14),  # X axis title
#     axis.title.y=element_text(size=14),  # Y axis title
#     axis.text.x=element_text(size=11,
#                              angle = 45,
#                              vjust=.5),  # X axis text
#     axis.text.y=element_text(size=12))  # Y axis text
#
# p3<-ggplot(sentW_per_day, aes(x=date, y=L)) +
#   geom_point()+
#   geom_smooth()+
#   theme_minimal()+
#   xlim(as_date("2022-09-30"),as_date("2022-12-01"))+
#   xlab("Data")+
#   ylab("Score")+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = as_date("2022-10-27"))+
#   geom_vline(xintercept = as_date("2022-11-17"))+
#   geom_text(aes(x = as_date("2022-11-8"),y=0.4), label="Soaring phase")+
#   ggtitle("Sentiment Score by LSS")+
#   theme_bw() +
#   theme(
#     legend.title = element_text(size=12),
#     legend.text = element_text(size=12),
#     plot.title = element_text(size = 20, face = "bold"),
#     axis.title.x=element_text(vjust=1,
#                               size=14),  # X axis title
#     axis.title.y=element_text(size=14),  # Y axis title
#     axis.text.x=element_text(size=11,
#                              angle = 45,
#                              vjust=.5),  # X axis text
#     axis.text.y=element_text(size=12))  # Y axis text
#
#
# p2/covid
