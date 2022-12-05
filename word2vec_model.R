# clear the working environment
rm(list=ls())

library(pacman)
p_load("dplyr","tidytext","tidyr","reshape2","wordcloud","here", "tidyverse", "ggplot2",
       "lubridate","quanteda", "quanteda.textplots", "quanteda.textstats","patchwork",
       "readxl","plyr","dygraphs","xts","hrbrthemes","word2vec","Rtsne", "stringr", "tm")
# install.packages("jiebaR")
library("jiebaR")

# set up the working directory
setwd("F:/BIGDATA_R/Covid_project")
getwd()

# Import data
wb_comment <- readRDS("wb_comment_covidGZ(New).rds")
head(wb_comment)

########################################## Basic process ###########################################
#去除重复值
wb_comment<- filter(wb_comment,!duplicated(wb_comment$text))

# 这里与前面不同，stopwords2.txt没有去除"疫情，广州市，广州"
wk <- worker(user='user.utf8',stop_word='F:\\BIGDATA_R\\Covid_project\\stopWord\\stopwords2.txt')

# 去停词
comment<- wb_comment %>%
  mutate(words = map(text,segment,jieba = wk)) %>%
  select(words)

# 去除特殊符号
comment$words <- gsub("\\d","",comment$words)
comment$words <- gsub('[a-z A-Z - () "]',"",comment$words)
comment$words <- gsub(","," ",comment$words)
# 地区后面统一去掉"区"，如天河区改成天河
comment$words <- gsub("区","",comment$words)
# 地区后面统一去掉"市"，如广州市改成广州
comment$words <- gsub("市","",comment$words)

comment$words <- gsub("新冠疫情","疫情",comment$words)
comment$words <- gsub("新冠","疫情",comment$words)
comment <- subset(comment,nchar(comment$words) > 0)


####################################### Word to Vector #######################################
word2vec_model <- word2vec(x = comment$words, type = "skip-gram", dim = 300, window = 10)
write.word2vec(word2vec_model, file = "covidcomment_model.bin")

embedding_matrix <- as.matrix(word2vec_model)
head(embedding_matrix[,1:10])

# Text model
predict(word2vec_model, newdata = c("广州"), type = "nearest", top_n = 5)
predict(word2vec_model, newdata = c("疫情"), type = "nearest", top_n = 5)
predict(word2vec_model, newdata = c("希望"), type = "nearest", top_n = 5)


#################################### Analysis 地区疫情 #########################################
exemplary_words <- c("疫情","广州","越秀","海珠","荔湾",
                     "天河","白云","黄埔","南沙","番禺","花都","增城","从化"
)

exemplary_vectors <- embedding_matrix[exemplary_words,]

# Similarity
similarities <- embedding_matrix%*%embedding_matrix["疫情",]
similarities<-data.frame(similarities)
name <- row.names(similarities)
similarities <- cbind(similarities,name)
area<-similarities[exemplary_words,]
area<-area[order(-area$similarities),]
area

##################################### Visualisations #########################################
library("stringr")
# tSNE plot
exemplary_vectors <- normalize_input(exemplary_vectors)
set.seed(624)
tsne_out <- Rtsne(exemplary_vectors, dims = 2, perplexity =3 )
tsne_plot <- tsne_out$Y
rownames(tsne_plot) <- exemplary_words
plot(tsne_plot)
text(tsne_plot, labels=rownames(tsne_plot), cex= 0.7, pos = 1)
abline(h = 0,lty=2,col="red")
abline(v = 0,lty=2,col="blue",lwd=1)

# 使用ggplot2包可视化tSNE降维的结果
library(ggplot2)

tsne_res <- as.data.frame(tsne_out$Y)
colnames(tsne_res) <- c("tSNE1","tSNE2")
head(tsne_res)

# 使用ggplot2可视化tSNE降维的结果
ggplot(tsne_res,aes(tSNE1,tSNE2)) +
  geom_point() + theme_bw() +
  geom_hline(yintercept = 0,lty=2,col="red") +
  geom_vline(xintercept = 0,lty=2,col="blue",lwd=1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "tSNE plot",color="Species") +
  text(tsne_plot, labels=rownames(tsne_plot), cex= 0.7, pos = 1)

# PCA
pca_out <- prcomp(exemplary_vectors, scale = TRUE)
pca_plot <- pca_out$x[,1:2]
rownames(pca_plot) <- exemplary_words
plot(pca_plot)
text(pca_plot, labels=rownames(pca_plot), cex= 0.7, pos = 2)

