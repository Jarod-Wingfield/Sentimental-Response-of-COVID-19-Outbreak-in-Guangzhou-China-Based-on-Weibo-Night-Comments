# clear the working environment
rm(list=ls())

# set up the working directory
setwd("F:/BIGDATA_R/Covid_project")
getwd()

library(pacman)
p_load("dplyr","tidytext","tidyr","reshape2","wordcloud","here", "tidyverse", "ggplot2",
       "lubridate","quanteda", "quanteda.textplots", "quanteda.textstats","patchwork",
       "readxl","plyr","dygraphs","xts","hrbrthemes")
# install.packages("jiebaR")
library("jiebaR")

# Import data
wb_comment <- readRDS("wb_comment_covidGZ.rds")
head(wb_comment)

##################################### Basic process ################################################
#去除重复值
wb_comment<- filter(wb_comment,!duplicated(wb_comment$text))

wk <- worker(user='user.utf8',stop_word='F:\\BIGDATA_R\\Covid_project\\stopWord\\stopwords.txt')

# Sentiment analysis with time trend
comment<- wb_comment %>%
  mutate(words = map(text,segment,jieba = wk)) %>%
  select(created_at,words) %>%
  unnest(cols=words)

# 修改created_at的格式为-月-日
comment$created_at <-substring(comment$created_at,1,6)
# 剔除一些特殊符号
comment$words <- gsub("\\d","",comment$words)
comment$words <- gsub("[a-z A-Z -]","",comment$words)
# 地区后面统一去掉"区"，如天河区改成天河
comment$words <- gsub("区","",comment$words)
# 去除字符串中的空值
comment <-subset(comment,words!="")


################################### Word analysis #######################################

# 计算频数
freq_table <-  comment %>%
  dplyr::count(created_at,words)

# 问题：仍然有一些字符去不掉

# TF-IDF
tf_idf_table <- freq_table %>%
  bind_tf_idf(term = words,document = created_at,n = n)

##################################### 词频绘图  ##########################################
# 绘制三时期的高频词柱状图
freq_table1 <- freq_table %>%
  filter(created_at<="10月27日")
freq_table1 <- aggregate(n ~ words, data = freq_table1, sum)
freq_table1<-freq_table1[order(-freq_table1$n),][1:20,]
f1<-ggplot(freq_table1, aes(x = reorder(words, n), y = n, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The most frequency words in flat period") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

freq_table2 <- freq_table %>%
  filter(created_at>="11月04日",created_at<="11月08日")
freq_table2 <- aggregate(n ~ words, data = freq_table2, sum)
freq_table2<-freq_table2[order(-freq_table2$n),][1:20,]
f2<-ggplot(freq_table2, aes(x = reorder(words, n), y = n, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The most frequency words in soaring phase Ⅰ") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

freq_table3 <- freq_table %>%
  filter(created_at>="11月10日",created_at<="11月17日")
freq_table3 <- aggregate(n ~ words, data = freq_table3, sum)
freq_table3<-freq_table3[order(-freq_table3$n),][1:20,]
f3<-ggplot(freq_table3, aes(x = reorder(words, n), y = n, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The most frequency words in soaring phase Ⅱ") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

# Combine
(f1+f2)/f3


##################################### TF-IDF绘图  ##########################################
# 绘制三时期的高TF-IDF词柱状图
tf_idf_table1 <- tf_idf_table %>%
  filter(created_at<="10月27日")
tf_idf_table1 <- aggregate(tf_idf ~ words, data = tf_idf_table1, sum)
tf_idf_table1<-tf_idf_table1[order(-tf_idf_table1$tf_idf),][1:20,]
t1<-ggplot(tf_idf_table1, aes(x = reorder(words, tf_idf), y = tf_idf, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The highest TF-IDF words in flat period") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

tf_idf_table2 <- tf_idf_table %>%
  filter(created_at>="11月04日",created_at<="11月08日")
tf_idf_table2 <- aggregate(tf_idf ~ words, data = tf_idf_table2, sum)
tf_idf_table2<-tf_idf_table2[order(-tf_idf_table2$tf_idf),][1:20,]
t2<-ggplot(tf_idf_table2, aes(x = reorder(words, tf_idf), y = tf_idf, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The highest TF-IDF words in soaring phase Ⅰ") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

tf_idf_table3 <- tf_idf_table %>%
  filter(created_at>="11月10日",created_at<="11月17日")
tf_idf_table3 <- aggregate(tf_idf ~ words, data = tf_idf_table3, sum)
tf_idf_table3<-tf_idf_table3[order(-tf_idf_table3$tf_idf),][1:20,]
t3<-ggplot(tf_idf_table3, aes(x = reorder(words, tf_idf), y = tf_idf, fill = words, alpha = 0.7)) +
  geom_bar(stat = "identity") +
  theme_minimal() + coord_flip() +
  ylab("The highest TF-IDF words in soaring phase Ⅱ") +
  xlab("") +
  guides(fill = FALSE, alpha = FALSE)

# Combine
(t1+t2)/t3


###################################### 词云图 ########################################
# plot a dfm object as a wordcloud
# construct a dfm
df <- data.frame(comment$words)
colnames(df)<-c("text")
df <- corpus(df)

comment_dfm <- dfm(df)
class(comment_dfm)
topfeatures(comment_dfm)

set.seed(624)
textplot_wordcloud(comment_dfm, min_count = 500, random_order = FALSE,
                   rotation = .25, max_words = 300,
                   min_size = 1.5, max_size = 4.5,
                   col = brewer.pal(n = 9, name = "Set1"))





#################################### Sentiment Analysis ######################################

# 大连理工大学中文情感词汇
dllg <- read_excel("./DLUT-Emotionontology-master/情感词汇/情感词汇.xlsx")
dllg <- dllg[,c(1,5,6,7)]
comment_sen_dllg <- comment %>%
  inner_join(dllg,by="words")

# 将消极情感的power取负号
comment_sen_dllg$power[comment_sen_dllg$sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA")]<- -comment_sen_dllg$power[comment_sen_dllg$sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA")]
# 拆分为积极和消极数据集
sen_pos <- comment_sen_dllg[!comment_sen_dllg$sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA"),]
sen_neg <- comment_sen_dllg[comment_sen_dllg$sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA"),]

# 积极和消极情感按时间加总
dictresult_positive <- aggregate(power ~ created_at, data = sen_pos, sum)
dictresult_negative <- aggregate(power ~ created_at, data = sen_neg, sum)
# 净情感按时间加总
dictresult_net <- aggregate(power ~ created_at, data = comment_sen_dllg, sum)

# 因为采集评论的时候，每天的评论数是不一样的，故取平均看看结果如何！！！
# 积极和消极情感按时间加总并取平均
dictresult_positive_avg <- aggregate(power ~ created_at, data = sen_pos, mean)
dictresult_negative_avg <- aggregate(power ~ created_at, data = sen_neg, mean)
# 净情感按时间加总并取平均
dictresult_net_avg <- aggregate(power ~ created_at, data = comment_sen_dllg, mean)


##################################### 情感分析绘图——词云图 #######################################
# 生成一列positive和negative
comment_sen_dllg <- comment_sen_dllg %>%
  mutate(sentiments="positive")
comment_sen_dllg$sentiments[comment_sen_dllg$sentiment %in% c("NL","NK","NN","ND","NE","NG","NC","NI","PF","NH","NJ","NB","NA")] <- "negative"

set.seed(624)
options(repr.plot.width=15, repr.plot.height=15,margin = 0.1)
comment_sen_dllg %>%
  dplyr::count(words,sentiments,sort=TRUE) %>%
  acast(words ~ sentiments, value.var = "n", fill=0) %>%
  # wordcloud
  comparison.cloud(title.size=1,colors=c("#F8766D","#05A4C0"),max.words =200,
                   scale = c(2.5,.5))


##################################### 情感分析绘图——sum #######################################
presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = FALSE, width = 110)
}

# 设置time
dictresult_positive <- xts(x = dictresult_positive[,2],order.by = seq(as.Date("2022-10-1"),length = length(dictresult_positive[,2]),by = "days"))
dictresult_negative <- xts(x = dictresult_negative[,2],order.by = seq(as.Date("2022-10-1"),length = length(dictresult_negative[,2]),by = "days"))
dictresult_net <- xts(x = dictresult_net[,2] ,order.by = seq(as.Date("2022-10-1"),length = length(dictresult_net[,2]),by = "days"))

p_polar <- dygraph(cbind(dictresult_positive,dictresult_negative),main = "Sentimental analysis Covid-19 in Guangzhou China (Positive&Negative)") %>%
  dyCSS("dygraph.css") %>%
  dyAxis("y", label = "Power") %>%
  dySeries("dictresult_positive", label = "Positive sentiment") %>%
  dySeries("dictresult_negative", label = "Negative sentiment") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(2, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")


p_net <- dygraph(dictresult_net,main = "Sentimental analysis Covid-19 in Guangzhou China (Net)") %>%
  dyCSS("dygraph.css") %>%
  dyAxis("y", label = "Power") %>%
  dySeries("V1", label = "Net sentiment") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p_polar
p_net

# 将sentiment_net与疫情数据结合到一个图
covid <- read_excel("covid_Guangzhou.xls")

# Then you can create the xts necessary to use dygraph
NLCC <- xts(x = covid$New_local_confirmed_cases, order.by = covid$Time)
NLAI <- xts(x = covid$New_local_asymptomatic_infections, order.by = covid$Time)

p_comb1 <- dygraph(cbind(dictresult_net,NLCC,NLAI),main = "Net Sentimental Score and Covid-19 situation in Guangzhou China") %>%
  dyCSS("dygraph.css") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries("dictresult_net", label = "Net sentiment",axis="y2") %>%
  dyAxis("y2", label = "Power") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=500) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p_comb1

p_comb2 <- dygraph(cbind(dictresult_negative,NLCC,NLAI),main = "Negative Sentimental Score and Covid-19 situation in Guangzhou China") %>%
  dyCSS("dygraph.css") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries("dictresult_negative", label = "Negative sentiment",axis="y2") %>%
  dyAxis("y2", label = "Power") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=510) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")
p_comb2





###################################### 情感分析绘图——avg #######################################
presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = FALSE, width = 110)
}

# 设置time
dictresult_positive_avg <- xts(x = dictresult_positive_avg[,2],order.by = seq(as.Date("2022-10-1"),length = length(dictresult_positive_avg[,2]),by = "days"))
dictresult_negative_avg <- xts(x = dictresult_negative_avg[,2],order.by = seq(as.Date("2022-10-1"),length = length(dictresult_negative_avg[,2]),by = "days"))
dictresult_net_avg <- xts(x = dictresult_net_avg[,2] ,order.by = seq(as.Date("2022-10-1"),length = length(dictresult_net_avg[,2]),by = "days"))

p_polar_avg <- dygraph(cbind(dictresult_positive_avg,dictresult_negative_avg),main = "Sentimental analysis Covid-19 in Guangzhou China (Positive&Negative Average)") %>%
  dyCSS("dygraph.css") %>%
  dyAxis("y", label = "Power") %>%
  dySeries("dictresult_positive_avg", label = "Average Positive sentiment") %>%
  dySeries("dictresult_negative_avg", label = "Average Negative sentiment") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(2, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")


p_net_avg <- dygraph(dictresult_net_avg,main = "Sentimental analysis Covid-19 in Guangzhou China (Net Average)") %>%
  dyCSS("dygraph.css") %>%
  dyAxis("y", label = "Power") %>%
  dySeries("V1", label = "Average Net sentiment") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p_polar_avg
# 平均的净情感变化比较能随疫情变动
p_net_avg

p_comb1_avg <- dygraph(cbind(dictresult_net_avg,NLCC,NLAI),main = "Net Average Sentimental Score and Covid-19 situation in Guangzhou China") %>%
  dyCSS("dygraph.css") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries("dictresult_net_avg", label = "Average Net sentiment",axis="y2") %>%
  dyAxis("y2", label = "Power") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p_comb1_avg


p_comb2_avg <- dygraph(cbind(dictresult_negative_avg,NLCC,NLAI),main = "Negative Average Sentimental Score and Covid-19 situation in Guangzhou China") %>%
  dyCSS("dygraph.css") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries("dictresult_negative_avg", label = "Average Negative sentiment",axis="y2") %>%
  dyAxis("y2", label = "Power") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")
p_comb2_avg


###################################### 情感分析绘图—— ggplot ########################################
net_avg <- aggregate(power ~ created_at, data = comment_sen_dllg, mean)
net_avg$created_at <- as.Date(net_avg$created_at, format = "%m月%d日")

nlcc<-data.frame(date = index(NLCC),
                 NLCC, row.names=NULL)
nlai<-data.frame(date = index(NLAI),
                 NLAI, row.names=NULL)
m1 <- merge(nlcc, nlai, by.x = "date", by.y = "date")
m1$date<-as.Date(m1$date)
m2 <- merge(m1, net_avg, by.x = "date", by.y = "created_at")

# smooth data
net_avg <- net_avg%>%
  mutate(t=seq(1,nrow(net_avg)))
d<-loess(power~t,net_avg)
sm_net<-data.frame(predict(d, data.matrix(data.frame(k = seq(1, nrow(net_avg), 1))), se = TRUE))
sm_net$lwr<-sm_net$fit-sm_net$se.fit
sm_net$upr<-sm_net$fit+sm_net$se.fit
sm_net$date<-m2$date

all <- merge(m2, sm_net, by.x = "date", by.y = "date")
all <- select(all,-c("se.fit","residual.scale","df","power"))
all <- xts(all,order.by=as.Date(all[,1]))

p <- dygraph(all,main = "Sentimental analysis Covid-19 in Guangzhou China (Net)") %>%
  dyCSS("dygraph.css") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries(c("fit"), label = "Smooth score",axis="y2") %>%
  dyAxis("y2", label = "Power") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical")
# %>%
#   dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
#   dyShading(from = "2022-10-17", to = "2022-10-27") %>%
#   dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
#   dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
#   presAnnotation("2022-10-22", text = "flat period") %>%
#   presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
#   presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p




Sys.setlocale("LC_TIME","English")
p1 <- ggplot(data = net_avg, aes(x = created_at, y = power)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = 'loess', span = 1, se = TRUE, level = 0.95)+
  theme(legend.position = "right")+
  xlab("Data")+
  ggtitle("Smooth fit of Net sentiment score")+
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x=element_text(vjust=1,
                              size=14),  # X axis title
    axis.title.y=element_text(size=14),  # Y axis title
    axis.text.x=element_text(size=11,
                             angle = 45,
                             vjust=.5),  # X axis text
    axis.text.y=element_text(size=12))  # Y axis text
p1


p2 <- ggplot(m2,aes(x=date)) +
  geom_line(aes(y=NLAI,colour="New local asymptomatic infections"),size=0.8) +
  geom_line(aes(y=NLCC,colour="New local confirmed cases"),size=0.8) +
  theme_bw() +
  # # scale_x_continuous(limits = c(2001,2020),breaks=c(2001,2005,2010,2015,2020)) +
  # # stat_smooth(aes(y=power,colour="Net sentiment power"),method = 'loess',formula = "y~x", span = 1, se = TRUE, level = 0.95)+
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
p2

p1/p2

