# set up the working directory
setwd("F:/BIGDATA_R/Covid_project/Data_process")
getwd()
# prepare packages
library(tidyverse)
library(xlsx)
library(readxl)

# Loan the data of comments
comment_Step <- read_excel("covidGZwb_Sept.xls")
comment_9_10 <- read_excel("covidGZwb_9to10.xls")
comment_Oct <- read_excel("covidGZwb_Oct.xls")
comment_10_11 <- read_excel("covidGzwb_10to11.xls")
comment_Nov <- read_excel("covidGZwb_Nov.xls")
comment_11_12 <- read_excel("covidGZwb_11to12.xls")


# 以下是10月1日到11月数据
# Combine the data
comment_all <- rbind(comment_Oct,comment_10_11,comment_Nov,comment_11_12)

comment_all <- select(comment_all,-c("...1",'rid'))
colnames(comment_all) <- c("User_ID","text","Forwarding_amount","Comment_amount","Likes","created_at","Keywords")

comment <-subset(comment_all,text!="NA")
# 剔除带“今天”的数据
comment <- comment %>% filter(created_at <"12月01日")

saveRDS(comment, "F://BIGDATA_R//Covid_project//wb_comment_covidGZ.rds")

# 以下是9月1日到11月数据
# Combine the data
comment_2 <- rbind(comment_Step,comment_9_10,comment_Oct,comment_10_11,comment_Nov,comment_11_12)

comment_2 <- select(comment_2,-c("...1",'rid'))
colnames(comment_2) <- c("User_ID","text","Forwarding_amount","Comment_amount","Likes","created_at","Keywords")

comment <-subset(comment_2,text!="NA")
# 剔除带“今天”的数据
comment <- comment %>% filter(created_at <"12月01日")

saveRDS(comment, "F://BIGDATA_R//Covid_project//wb_comment_covidGZ(New).rds")
