---
title: "RHOA Reunion"
author: "Nadine"
date: "May 7, 2017"
output:github_document

---

# Overview

The Real Housewives of Atlanta's Reunion, Part IV was on last night (5/7/17). I had to wait until this morning at 6am, when it was posted online, to watch it. But last night, as it was going on, I checked out the #rhoareunion hashtag on Twitter to see what was happening. It seemed like the consensus was that Kandi had been wronged and that most people could agree that Phaedra was a liar. Rarely has there been such a consensus on one side about any fight on the RH, that I've seen. 

I took to the R Twitter pacakge, not to answer the question of whether that consensus was actually true (but if I could, I would) but to see what the general sentiment about the #rhoareunion was. It seems that the majority of people reflected the tense, stressful, sad, and angry mood of the reunion. After all, it was a crazy episode, heavy on deception, and lots and lots of under-the-bus-throwing. 

* 20% of the words used in tweets were positive
* 13% of the words used in tweets were negative
* 12% of the words used in tweets were angry
* 11% of the words used in tweets were fearful
* 11% of the words used in tweets were categorized as sad

More positive sentiments of joy, anticipation, trust, and surprise, were used the least. 

I used a comparison of the #rhobhreunion tweets, even though that aired last week, and I wasn't able to pull that many tweets from it this morning. Its the other franchise I watch, so I was semi-interested, even though compared to Atlanta, BH is not much to see. Here is just the sentiments for the most recent 5,000 tweets for #rhoareunion. The comparison with #rhobhreunion is further down in this post. 

![rhoatweets](http://khasachi.com/images/rhoa1.png)
![rhoatweets](http://khasachi.com/images/rhoa_table1.PNG)

<table>
  
  <tr>
    <th>sentiment</th>
    <th>words</th>
    <th>percent</th>
  </tr>
  <tr>
    <th>negative</th>
    <th>1,835</th>
    <th>20%</th>
  </tr>
  <tr>
    <th>positive</th>
    <th>1,150</th>
    <th>13%</th>
  </tr>
  <tr>
    <th>anger</th>
    <th>1,069</th>
    <th>12%/th>
  </tr>
  <tr>
    <th>sadness</th>
    <th>1,006</th>
    <th>12%/th>
  </tr>
  <tr>
    <th>fear</th>
    <th>997</th>
    <th>11%/th>
  </tr>
  <tr>
    <th>trust</th>
    <th>853</th>
    <th>9%/th>
  </tr>
  <tr>
    <th>disgust</th>
    <th>795</th>
    <th>9%/th>
  </tr>
  <tr>
    <th>anticipation</th>
    <th>630</th>
    <th>7%/th>
  </tr>
  <tr>
    <th>joy</th>
    <th>407</th>
    <th>3%/th>
  </tr>
  <tr>
    <th>surprise</th>
    <th>284</th>
    <th>3%</th>
  </tr>
  
  </table>

# R CODE
## Packages

*  library(dplyr)
*  library(purrr)
*  library(twitteR)
*  library(tidyr)
*  library(lubridate)
*  library(scales)
*  library(ggplot2)
*  library(stringr)
*  library(tidytext)
*  library(reshape2)
*  library(scales)

## Setup

Authenicate app. 

## Get rhoareunion tweets

Get the 5,000 most recent tweets with the #rhoareunion hashtag. 

    {r rhoatweets}
    
    rr <- searchTwitter("#rhoareunion", n=5000)
    
    rr2 <- do.call("rbind", lapply(rr, as.data.frame))
    
    head(rr2, n=10)



## Tidytext

Use the [tidytext](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html) package to get a list of words and their sentiments. 

    {r tidytext_words}
    
    nrc <- sentiments %>%
      filter(lexicon == "nrc") %>%
      dplyr::select(word, sentiment)
    
    head(nrc, n=20)



## Just the Text, Just the Words

Get just the text, and then just the words from that text, from the returned 5,000 most recent #rhoareunion tweets. 

    {r text_and_words}
    
    rr3 <- rr2[c(1)]
    
    str(rr3)
    
    just_words <- rr3 %>%
      filter(!str_detect(text, '^"')) %>%
      unnest_tokens(word, text, token = "regex") %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "[a-z]"))
    
    str(just_words)


### Merge Tweeted Words to Sentiments

Join the words from `just_words` with their sentiments from the `nrc` dataframe; group by sentiment, and sum the count of words for each sentiment. 

    {r tw_words_sentiments}
    
    trend_words_sentiment <- just_words %>%
      inner_join(nrc, by = "word") %>%
      count(sentiment) %>%
      ungroup() %>%
      complete(sentiment,  fill = list(n = 0)) %>%
      group_by(sentiment) %>%
      summarize(words = sum(n)) %>%
      ungroup()

### Sentiments as Percentage of Total

Add a variable with the percentage frequency of each sentiment to the total. 

    {r sent_perc}
    
    trend_words_sentiment$perc <- trend_words_sentiment$words/(sum(trend_words_sentiment$words))
    
    head(trend_words_sentiment, n=20)


### Sentiment Analysis #rhoareunion Bar Chart

This bar chart and table below show the sentiments most to least expressed in the most recent 5,000 tweets, as of 5/8/17 in the morning (the morning after the episode aired). The most expressed sentiment was negative (21%), followed by positive(13%), then anger(12%), sadness(11%), and fear(11%). MOre positive sentiments were least frequently used (trust, anticipation, joy, and surprise). 

    {r rhoa_sentiment_bar_chart}
    
    ggplot(trend_words_sentiment, aes(x=sentiment, y=perc)) + geom_bar(aes(fill=sentiment), stat="identity", position="dodge")
    
    trend_words_sentiment <- trend_words_sentiment %>%
                              arrange(desc(perc))


    trend_words_sentiment$perc <- paste(as.numeric(format(round(trend_words_sentiment$perc,2), nsmall = 2)) * 100, "%", sep="")
    
    kable(trend_words_sentiment)

### A comparison

For sake of comparison, I also included the most recent tweets (as of 5/8/17 morning) with the #rhobhreunion hashtag. It aired last week, so not surprisingly, there were significantly fewer tweets, and only 266 were pulled. I'm putting all the code to get to the RHOBH Sentiment barchart, because it's the same as above, just with a different set of tweets.

    {r bh_comparison}
    
    bh <- searchTwitter("#rhobhreunion", n=5000)
    
    bh2 <- do.call("rbind", lapply(bh, as.data.frame))
    
    str(bh2)
    
    bh3 <- bh2[c(1)]
    
    
    nrc <- sentiments %>%
      filter(lexicon == "nrc") %>%
      dplyr::select(word, sentiment)
    
    
    bh_just_words <- bh3 %>%
      filter(!str_detect(text, '^"')) %>%
      unnest_tokens(word, text, token = "regex") %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "[a-z]"))
    
    str(bh_just_words)
    
    bh_trend_words_sentiment <- bh_just_words %>%
      inner_join(nrc, by = "word") %>%
      count(sentiment) %>%
      ungroup() %>%
      complete(sentiment,  fill = list(n = 0)) %>%
      group_by(sentiment) %>%
      summarize(words = sum(n)) %>%
      ungroup()
    
    bh_trend_words_sentiment$perc <- bh_trend_words_sentiment$words/(sum(bh_trend_words_sentiment$words))
    
    head(bh_trend_words_sentiment)
    
    ggplot(bh_trend_words_sentiment, aes(x=sentiment, y=perc)) + geom_bar(aes(fill=sentiment), stat="identity", position="dodge")
    
    bh_trend_words_sentiment <- bh_trend_words_sentiment %>%
                              arrange(desc(perc))
    
    
    bh_trend_words_sentiment$perc <- paste(as.numeric(format(round(bh_trend_words_sentiment$perc,2), nsmall = 2)) * 100, "%", sep="")
    
    kable(bh_trend_words_sentiment)




![rhoatweets](http://khasachi.com/images/rhoa2.png)
![rhoatweets](http://khasachi.com/images/rhoa_table2.PNG)

<table>
  
  <tr>
    <th>sentiment</th>
    <th>words</th>
    <th>percent</th>
  </tr>
  <tr>
    <th>negative</th>
    <th>105</th>
    <th>20%</th>
  </tr>
  <tr>
    <th>positive</th>
    <th>71</th>
    <th>13%</th>
  </tr>
  <tr>
    <th>disgust</th>
    <th>63</th>
    <th>12%/th>
  </tr>
  <tr>
    <th>anger</th>
    <th>59</th>
    <th>11%<th>
  </tr>
  <tr>
    <th>sadness</th>
    <th>52</th>
    <th>10%/th>
  </tr>
  <tr>
    <th>fear</th>
    <th>50</th>
    <th>9%/th>
  </tr>
  <tr>
    <th>trust</th>
    <th>42</th>
    <th>8%/th>
  </tr>
  <tr>
    <th>anticipation</th>
    <th>41</th>
    <th>8%/th>
  </tr>
  <tr>
    <th>joy</th>
    <th>30</th>
    <th>6%/th>
  </tr>
    <tr>
    <th>surprise</th>
    <th>14</th>
    <th>3%</th>
  </tr>
  
  </table>


### Combine the Bar Charts

The following combines the bar charts and tables for the #rhoareunion and #rhobhreunion sentiment analyses, to provide a side-by-side comparision. 

    {r combine_bar_charts}
    
    mt <- merge(trend_words_sentiment, bh_trend_words_sentiment, by="sentiment")
    
    str(mt)
    
    mt$perc.x <- as.numeric(gsub("%$", "", mt$perc.x))
    mt$perc.y <- as.numeric(gsub("%$", "", mt$perc.y))
    
    mt$var <- mt$perc.x - mt$perc.y
    
    mt_melt <- melt(mt[c(1,3,5)], id=c('sentiment'))
    str(mt_melt)
    mt_melt$variable <- gsub("^perc.x$", "rhoareunion", mt_melt$variable)
    mt_melt$variable <- gsub("^perc.y$", "rhobhreunion", mt_melt$variable)
    mt_melt$sentiment <- gsub("^negative$", "01-negative", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^positive$", "02-positive", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^anger$", "03-anger", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^sadness$", "04-sadness", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^fear$", "05-fear", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^trust$", "06-trust", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^disgust$", "07-disgust", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^anticipation$", "08-anticipation", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^joy$", "09-joy", mt_melt$sentiment)
    mt_melt$sentiment <- gsub("^surprise$", "10-surprise", mt_melt$sentiment)
    
    mt_melt
    
    
    
    mt_melt <- mt_melt %>%
                arrange(sentiment, desc(value))
    
    ggplot(mt_melt, aes(x=sentiment, y=value)) + 
                    geom_bar(aes(fill=variable), stat="identity", position="dodge") + 
                   theme(axis.text.x = element_text(angle=45, hjust=1)) 
    str(mt)
    
    
    
    names(mt)[2] <- "rhoa_words"
    names(mt)[3] <- "rhoa_perc"
    names(mt)[4] <- "rhobh_words"
    names(mt)[5] <- "rhobh_perc"
    
    str(mt)
    
    mt$rhoa_perc <- paste(mt$rhoa_perc, "%", sep="")
    mt$rhobh_perc <- paste(mt$rhobh_perc, "%", sep="")
    
    kable(mt)


![rhoatweets](http://khasachi.com/images/rhoa3.png)
![rhoatweets](http://khasachi.com/images/rhoa_table3.PNG)

<table>
  
  <tr>
    <th>sentiment</th>
    <th>rhoa_words</th>
    <th>rhoa_percent</th>
    <th>rhobh_words</th>
    <th>rhobh_percent</th>
  </tr>
  <tr>
    <th>negative</th>
    <th>1,835</th>
    <th>20%</th>
    <th>105</th>
    <th>20%</th>
  </tr>
  <tr>
    <th>positive</th>
    <th>1,150</th>
    <th>13%</th>
    <th>71</th>
    <th>13%</th>
  </tr>
  <tr>
    <th>anger</th>
    <th>1,069</th>
    <th>12%/th>
    <th>59</th>
    <th>11%<th>
  </tr>
  <tr>
    <th>sadness</th>
    <th>1,006</th>
    <th>12%/th>
    <th>52</th>
    <th>10%/th>
  </tr>
  <tr>
    <th>fear</th>
    <th>997</th>
    <th>11%/th>
    <th>50</th>
    <th>9%/th>
  </tr>
  <tr>
    <th>trust</th>
    <th>853</th>
    <th>9%/th>
     <th>42</th>
    <th>8%/th>
  </tr>
  <tr>
    <th>disgust</th>
    <th>795</th>
    <th>9%/th>
        <th>63</th>
    <th>12%/th>
  </tr>
  <tr>
    <th>anticipation</th>
    <th>630</th>
    <th>7%/th>
    <th>41</th>
    <th>8%/th>
  </tr>
  <tr>
    <th>joy</th>
    <th>407</th>
    <th>3%/th>
      <th>30</th>
    <th>6%/th>
  </tr>
  <tr>
    <th>surprise</th>
    <th>284</th>
    <th>3%/th>
     <th>14</th>
    <th>3%</th>
  </tr>
  
  </table>

