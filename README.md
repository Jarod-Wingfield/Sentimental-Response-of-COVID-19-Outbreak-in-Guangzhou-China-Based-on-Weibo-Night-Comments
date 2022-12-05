# Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments
Advantages: 
1. 使用python爬取了微博的高级搜索界面一级和二级评论数据；
2. 文本分析的对象为中文文本，R语言对中文文本进行分析的案例较少，而这里使用了大连理工情感词典进行情感分析。\
Word2vec：根据词向量探究不同地区的疫情情况； Topic model：LDA； Sentiment model：LSS；\
其中处理了中文分词问题，使用jiebaR包可以更好地进行分词且可以自定义；\
处理了corpus为空值的情况等

## 1.Python to scawl the advanced searching comments of Weibo
**crawl_wb_comments_for_R**

## 2.Text analysis of Weibo comments during Covid-19 outbreak in Guangzhou China

### Data_process:

Due to the form of weibo comments crawled by python, I have to combine them and adjust. The post-process data were output as **wb_comment_covidGZ.rds (data Oct. to Nov.)**

**wb_comment_covidGZ(New).rds (data Sept. to Nov.)**

### covidGZ_line:

The file is used to generate two line of covid situation in Guangzhou China from 2022/9/5 to 2022/12/3./
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/covid_Guangzhou.png)

### Word_and_Sentiment_Analysis.R:

This fire does the normal text analysis on weibo comments. Using the Chinese emotional dictionary to analysis the Weibo comments, I drew a graph of sentiment situation along time.
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/wordcloud%20comments.png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Highest%20TF-IDF%20words%20in%20three%20period.png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Sentimental%20analysis%20Covid-19%20in%20Guangzhou%20China%20(Positive%26Negative).png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Sentimental%20analysis%20Covid-19%20in%20Guangzhou%20China%20(Net).png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Negative%20Sentimental%20Score%20and%20Covid-19%20situation%20in%20Guangzhou%20China.png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Net%20Average%20Sentimental%20Score%20and%20Covid-19%20situation%20in%20Guangzhou%20China.png)
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/Smooth%20fit.png)

### Word2vec_model.R:

Using the Word2vec package to analysis the covid-19 situation in diffect areas.
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/word2vec%20of%20area.png)

### LSS_model.R:

Using the Latent Semantic Scaling to analysis the sentiment. This method is trained for evaluating the word's sentiment, which is better than counting the power of comments.
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/LSS_model.png)
### Topic_model.R:

Using the LDA to find the topic, I could observe the changes in prevention and control efforts.
![image](https://github.com/Wyatt624/Sentimental-Response-of-COVID-19-Outbreak-in-Guangzhou-China-Based-on-Weibo-Night-Comments/blob/main/Fig/LDA_model.png)
