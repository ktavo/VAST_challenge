#Exploraci�n Base VAST Challenge 1

setwd("E:/Git Projects/UBA_2021/Visualizaci�n/MC1")

#A�ado librer�as necesarias
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

newsPath = "E:/Git Projects/UBA_2021/Visualizaci�n/MC1/News\ Articles"
newsSourcesPaths <- list.dirs(path = newsPath, full.names = TRUE, recursive = TRUE)
newsSourcesPaths <- newsSourcesPaths[2:30]
newsSourcesPaths
newsSources <- newsSourcesPaths[2:30]

######################################## Map this ########################################

for(i in 0:30) ###Purrr::map :(
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
glimpse(newsDataFrame)


newsDataFrame$fullText[18]

newsDataFrame$filePath[18]

array(as.numeric(unlist(newsDataFrame$fileName[1,840]))
################################# END Palabras en Noticias ############################################################







