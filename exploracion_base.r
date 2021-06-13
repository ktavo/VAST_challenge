#Exploración Base VAST Challenge 1

setwd("E:/Git Projects/UBA_2021/Visualización/MC1")

#Añado librerías necesarias
rm( list=ls() )  #remove all objects
gc()             #garbage collection


library("tidyverse")
library("corrr")
library("ggplot2")
library("dplyr")

library("rJava")
library("plyr")
library("qdap")
library("corpus")
library("stringr")
library("arules")
library("stopwords")
library("readtext")
library("tm")
library("wordcloud")
library("wordcloud2")
library("webshot")
library("htmlwidgets")


#webshot::install_phantomjs()



################################# Palabras en Emails ############################################################
datosEmails <- read.csv("email headers.csv", sep = ",")
#glimpse(datosEmails)

datosEmails <- datosEmails %>%
mutate_if(sapply(datosEmails, is.character), as.factor)

glimpse(datosEmails)
summary(datosEmails)
emailsStopWords <- c("re")
emailsStopWords <- c(stopwords_en, emailsStopWords)

frequent_terms_emails <- freq_terms(datosEmails$Subject, 35, stopwords = emailsStopWords)
plot(frequent_terms_emails)
################################# END Palabras en Emails############################################################



################################# Palabras en Noticias ############################################################

newsPath = "E:/Git Projects/UBA_2021/Visualización/MC1/News\ Articles"
newsSourcesPaths <- list.dirs(path = newsPath, full.names = TRUE, recursive = TRUE)
newsSourcesPaths <- newsSourcesPaths[2:30]
newsSourcesPaths
newsSources <- newsSourcesPaths[1:29]

######################################## Map this ########################################

for(i in 0:29) ###Purrr::map :(
{
  newsSources[i] <- str_remove(newsSources[i], newsPath)
  newsSources[i] <- str_remove(newsSources[i], "/")
}
newsSources

######################################## Map this ########################################

list_of_files <- list.files(path = newsPath, recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
# 
# df <- list_of_files %>%
#   set_names(.) %>%
#   map_df(read_table2, .id = "FileName")


#News contain:
#FILEPATH - list_of_files
#FULLTEXT
#SOURCE
#TITTLE
#PUBLISHED
#LOCATION
#NOTE


#WORLD SOURCE 787 - KINDNAPED

newsDataFrame <- data.frame(matrix(ncol = 8, nrow = 845))
colnames(newsDataFrame) <- c("filePath","fileName", "fullText","source", "tittle", "publishedDate", "location", "note")

newsDataFrame$filePath <- list_of_files 

newsDataFrame$fileName[1] <- toString(readtext(newsDataFrame$filePath[1]))
newsDataFrame$fileName[1]

for(i in 1:nrow(newsDataFrame))
{
  newsDataFrame$fileName[i] = basename(newsDataFrame$filePath[i])
  newsDataFrame$fullText[i] = readChar(newsDataFrame$filePath[i], file.info(newsDataFrame$filePath[i])$size)
  newsDataFrame$source[i] = dirname(newsDataFrame$filePath[i])
  newsDataFrame$source[i] <- str_remove(newsDataFrame$source[i], newsPath)
  newsDataFrame$source[i] <- str_remove(newsDataFrame$source[i], "/")
}
rm(i)
glimpse(newsDataFrame)


newsSources


#AllNewsTodayFullText = ""
# for(i in 1:nrow(newsDataFrame_AllNewsToday))
# {
#   AllNewsTodayFullText <- paste(AllNewsTodayFullText, newsDataFrame_AllNewsToday$fullText[i])
# }
# AllNewsTodayFullText
################################# END Palabras en Noticias ############################################################



################################# Plot palabras en Noticias ############################################################
# AllNewsTodayFullText <- AllNewsTodayFullText %>%
#   mutate_if(sapply(AllNewsTodayFullText, is.character), as.factor)
# glimpse(AllNewsTodayFullText)
# summary(AllNewsTodayFullText)

newsSources


makeItCloud <- function(newsDataFrame, sourceFilter){
  sourceFilter
  filteredDataFrame <- newsDataFrame %>% 
      filter(source == sourceFilter)
  #glimpse(filteredDataFrame)
  
  newsStopWords <- c("")
  newsStopWords <- c(stopwords_en, newsStopWords)
  
  frequent_terms_news <- freq_terms(filteredDataFrame$fullText, 35, stopwords = newsStopWords)
  plot(frequent_terms_news)
  
  # Make a vector source
  newsSource <- VectorSource(filteredDataFrame$fullText)
  
  # Make a volatile corpus
  newsCorpus <- VCorpus(newsSource)
  
  # Clean the corpus
  clean_corpus <- function(corpus){
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    return(corpus)
  }
  newsCleanCorpus <- clean_corpus(newsCorpus)
  
  # Convert TDM to matrix
  newsTDM <- TermDocumentMatrix(newsCleanCorpus)
  newsAsMatrix <- as.matrix(newsTDM)
  
  # Sum rows and frequency data frame
  newsFreqTerms <- rowSums(newsAsMatrix)
  
  newsWordsFreq <- data.frame(
    term = names(newsFreqTerms),
    num = newsFreqTerms
  )
  glimpse(newsWordsFreq)
  
  newsWordsFreq <- newsWordsFreq %>% 
    filter(num > 4)
  
  #newsWordCloud <- wordcloud(newsWordsFreq$term, newsWordsFreq$num,
  #          max.words = 100, colors = "#52a802") #69039c 
  
  wordcloud(words = newsWordsFreq$term, freq = newsWordsFreq$num, min.freq = 1, max.words=50, random.order=FALSE, 
            rot.per=0.35, colors=brewer.pal(4, "BrBG"))
  
  # wordcloud(words = newsWordsFreq$term, freq = newsWordsFreq$num, min.freq = 1, max.words=100, random.order=FALSE, 
  #           rot.per=0.35, colors=brewer.pal(5, "RdYlGn"))
  
  
  # 
  # # Make the graph
  # my_graph=wordcloud2(newsWordsFreq, size=0.5, minSize = sourceFilter, color = brewer.pal(8, "Dark2"), 
  #                     minRotation = -pi/2, maxRotation = pi/2, shape = 'circle')
  # # save it in html
  # saveWidget(my_graph,"tmp.html",selfcontained = F)
  # # and in png
  # webshot("tmp.html","fig_1.png", delay =5, vwidth = 480, vheight=480) # changed to png. 
}
#newsWordCloud

# textFilter <- newsSources[1]
# textFilter
# makeItCloud(newsDataFrame,textFilter)


for(i in 1:29) ###Purrr::map :(
{
  textFilter <- newsSources[i]
  makeItCloud(newsDataFrame,textFilter)
}


help(wordcloud)

################################# END Plot palabras en Noticias ############################################################








