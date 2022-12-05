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
wb_comment <- wb_comment %>%
  filter(Keywords=="广州疫情") %>%
  filter(!duplicated(text)) %>%
  select(created_at,text)
wb_comment$created_at <-as.Date(substring(wb_comment$created_at,1,6), format = "%m月%d日")
colnames(wb_comment)[1] <- 'date'

wb_comment <- corpus(wb_comment)

########################################## LDA ###########################################
toks_news <- wb_comment %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_numbers = TRUE, remove_url = TRUE) %>%
  tokens_remove(stopwords::stopwords("zh_cn", source = "marimo"), min_nchar = 2) %>%
  tokens_keep(pattern = "^\\p{script=Hani}+$", valuetype = 'regex') %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com"))

# 去除tokens内的空白文本（但其字符串字数却不为0）,所以使用ntoken()
toks_news<-toks_news[ntoken(toks_news)!=0]

# # 去除空白方法二
# dfmat<-dfm(toks_news)
# dfmatsub <- dfm_subset(dfmat, ntoken(dfmat) > 0)

dfmat_news <- dfm(toks_news) %>%
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
# 去除所有均为0的行
raw.sum<-apply(dfmat_news,1,FUN=sum) #sum by raw each raw of the table
dfmat_news<-dfmat_news[raw.sum!=0,]

# require(seededlda)
# tmod_lda <- textmodel_lda(dfmat_news, k = 10)
# terms(tmod_lda, 10)
# topic <- terms(tmod_lda, 10)

# 各月份
df9<-dfm_subset(dfmat_news,date<"2022-10-01")
df10<-dfm_subset(dfmat_news,date>="2022-10-01")
df10<-dfm_subset(df10,date<"2022-11-01")
df11<-dfm_subset(dfmat_news,date>="2022-11-01")

tmod_lda1 <- LDA(df9, k = 4,control = list(estimate.alpha = FALSE, seed = 624))
tmod_lda2 <- LDA(df10, k = 4,control = list(estimate.alpha = FALSE, seed = 624))
tmod_lda3 <- LDA(df11, k = 4,control = list(estimate.alpha = FALSE, seed = 624))

AP_topics1 <- tidy(tmod_lda1, matrix = "beta")
AP_topics2 <- tidy(tmod_lda2, matrix = "beta")
AP_topics3 <- tidy(tmod_lda3, matrix = "beta")

ap_top_terms1 <- AP_topics1 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
p1<-ap_top_terms1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>% #reorder within each topic
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4,scales="free") +
  coord_flip()+
  xlab("Topic")+
  ylab("Beta")+
  ggtitle("Topics in September")+
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


ap_top_terms2 <- AP_topics2 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
p2<-ap_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>% #reorder within each topic
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4,scales="free") +
  coord_flip()+
  xlab("Topic")+
  ylab("Beta")+
  ggtitle("Topics in October")+
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

ap_top_terms3 <- AP_topics3 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
p3<-ap_top_terms3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>% #reorder within each topic
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4,scales="free") +
  coord_flip()+
  xlab("Topic")+
  ylab("Beta")+
  ggtitle("Topics in November")+
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

p1/p2/p3
