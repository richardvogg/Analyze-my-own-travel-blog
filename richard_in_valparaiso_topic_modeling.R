library(tidyverse)
library(patchwork)

#Shortcut if the file is already stored somewhere
#Otherwise run richard_in_valparaiso_scraping.R first
blogs <- read.csv("C:/Richard/R and Python/Experiments/text analysis/Blogtexts.csv")


blogs_final <- blogs %>% 
  mutate(weekday = factor(weekday) %>% 
           forcats::fct_relevel("Montag","Dienstag","Mittwoch","Donnerstag",
                                "Freitag","Samstag","Sonntag")) %>% 
  separate(date,into = c("day","monthyear"),sep = "[.] ",) %>%
  separate(monthyear,into=c("month","year"),sep = " ") %>% 
  mutate(month = factor(month) %>% 
           fct_relevel("Januar","Februar","März","April","Mai","Juni","Juli",
                       "August","September","Oktober","November","Dezember"))


#When did I post?
blogs_final %>% 
  ggplot(aes(x=month))+
  geom_bar()+
  facet_wrap(~year)+
  labs(title="Number of blogposts per month and year",x="",y="#Posts")+
  theme(axis.text.x = element_text(angle = 25))

#Which days of the week?
blogs_final %>% 
  ggplot(aes(x=weekday))+
  geom_bar()+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle = 25))

#idea for other representation
blogs_final %>%
  count(year,weekday) %>%
  group_by(year) %>%
  mutate(perc=n/sum(n)) %>%
  ggplot(aes(x=weekday,y=1,fill=perc))+
  geom_tile()+
  geom_text(aes(label=round(perc,2)))+
  geom_text(aes(x=0.7,y=1.4,label=year),size=4,col="white")+
  facet_grid(year~.)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank())

#Text Analysis

library(topicmodels)
library(tidytext)
library(tidylo)
library(ggwordcloud)

#German stopwords
stopWords <- c(tm::stopwords("de")) %>% 
  tibble(txt=.)

blog_words <- blogs_final %>% tibble() %>% 
  unnest_tokens(word,contents_clean) %>% 
  anti_join(stopWords,by=c("word"="txt")) %>% 
  count(titles,word,sort=TRUE)

too_freq <- blog_words %>%
  count(word) %>%
  filter(n>50)
  
too_unfreq <- blog_words %>%
  count(word) %>%
  filter(n<2)

blog_words <- blog_words %>% 
  anti_join(too_unfreq,by="word") %>%
  anti_join(too_freq,by="word")

#Longest articles
{
  
  total_words <- blog_words %>% 
    group_by(titles) %>% summarise(total=sum(n))
  
  long <- total_words %>% top_n(10,total) %>% 
    ggplot(aes(x=total,y=reorder(titles,total)))+geom_col()+
    xlim(c(0,650))+
    labs(x="",y="")+
    theme(axis.text.x=element_blank())
  
  short <- total_words %>% top_n(10,-total) %>% 
    ggplot(aes(x=total,y=reorder(titles,total)))+geom_col()+
    xlim(c(0,650))+
    labs(x="words",y="")
  
  
  
  long / short
}


#Perform LDA
word_dtm <- cast_dtm(blog_words,titles,word,n)

blog_lda <- LDA(word_dtm,k=6,control=list(seed=1))



#Check the topics

blog_topics <- tidy(blog_lda,matrix="beta")

top_terms <- blog_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



#Plot top words
{
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() 
}

#Wordcloud for each topic
ggplot(top_terms,aes(label=term,size=beta,col=factor(topic)))+
  geom_text_wordcloud()+
  facet_wrap(~topic)+
  scale_size_area(max_size=14)+
  theme_minimal()


#Add main topic to each article

blogpost_topics <- tidy(blog_lda, matrix = "gamma") %>%
  group_by(document) %>%
  mutate(rank=rank(gamma)) %>%
  filter(rank==6)

final <- blog_words %>% 
  left_join(blogpost_topics,by=c("titles"="document"))