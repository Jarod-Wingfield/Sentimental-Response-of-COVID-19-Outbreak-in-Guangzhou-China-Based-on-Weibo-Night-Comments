# Sentimental Response of COVID-19 Outbreak in Guangzhou, China
Contributions: 
1. I used python to crawl the first and second level comment data of the advanced search interface on Weibo(a popular social media platform in China).
2. The object of text analysis is Chinese text, and there are fewer cases of R language analyzing Chinese text. Meanwhile, the Dalian Polytechnic Sentiment Dictionary (Chinese dictionary) was used here for the first time for sentiment analysis. This project contains most of the problems that will be encountered when dealing with Chinese word separation. Using the jiebaR package allows for better and customizable segmentation, and I dealt with the case where corpus is null etc.


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
