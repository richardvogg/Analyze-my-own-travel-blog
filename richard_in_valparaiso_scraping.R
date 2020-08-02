library(rvest)
library(dplyr)
library(polite)



#Put together a vector with all the article links
titles_list <- list()

#Go through all possible months and get the links from the pages
#If there are no articles for a given month, it will return the most recent links.
for(month in c(paste0('0',1:9),'10','11','12')) {
  for(year in c('2014','2015','2017','2018','2019','2020'))
  {
    url <- paste0("http://richard-in-valparaiso.blogspot.com/",year,"/",month)
    
    session <- polite::bow(url,user_agent = "Richs tester")
    
    webpage <- scrape(session)
    
    titles <- html_nodes(webpage,".posts > li > a") %>% 
      html_attr('href')
    
    titles_list <- append(titles_list,titles)
    cat(month)
  }
}

#Remove duplicates
titles <- unique(titles_list)

#Scrape dates
get_date <- function(url) {
  session <- polite::bow(url,user_agent = "Richs tester")
  
  webpage <- scrape(session)
  
  date <- html_nodes(webpage,".date-header") %>% 
    html_text()
  
  return(date)
}

#Function to get contents
get_content <- function(url) {
  session <- polite::bow(url,user_agent = "Richs tester")
  
  webpage <- scrape(session)
  
  date <- html_nodes(webpage,"div.post.hentry.uncustomized-post-template") %>% 
    html_text()
  
  cat(1)
  
  return(date)
}

dates <- sapply(titles,get_date)

contents <- lapply(titles,get_content)

#remove artifacts from html
contents_clean <- gsub("\\n"," ",contents)

contents_clean <- gsub("P \\{.*?\\}","",contents_clean) %>% trimws()

#function to get titles
get_titles <- function(text) {
  title <- strsplit(text,"   ") %>% {.[[1]]} %>% {.[[1]]}
  return(title)
}

titles <- lapply(contents_clean,get_titles) %>% unlist()

#build dataframe
blogs <- data.frame(dates,titles,contents_clean) %>% 
  tidyr::separate(dates,into=c("weekday","date"),sep=",")

write.csv(blogs,"Blogtexts.csv",row.names=FALSE)

