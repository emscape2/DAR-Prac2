#gaat nog ergens fout

#leest csv files in afhankelijk van path

query <- read.csv("C:\\Users\\Debby Lam\\Documents\\UU jaar 2\\R\\query_product.csv",stringsAsFactors=FALSE)
description <- read.csv("C:\\Users\\Debby Lam\\Documents\\UU jaar 2\\R\\product_descriptions.csv", stringsAsFactors = FALSE)

total <- merge(query, description, by="product_uid")

library(tau)
library(tm)
library(MASS)
library(nnet)
library(SnowballC)

#Sla hele kolom van producttitel van data op in variabele
product <- query[,3]  

#Sla hele kolom van search terms op in variabele
searchTerm <- query[,4] 

relevanceScore <- query[,5]

productDescription <- description[,2]

# telt aantal unique values voorkomens van product
productTable <- table(product)  

#convert matrix product naar een vector
vector <- as.vector(product)

vecSource <- VectorSource(vector)

#maakt een corpus aan
docs <- VCorpus(vecSource) 

#Verwijder punctuatie, stopwoorden, getallen en hoofdletters uit corpus
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("en"))
docs <- tm_map(docs, content_transformer(tolower))

writeLines(as.character(docs[1]))

#verwijder overbodige whitespace
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)

tdm <- TermDocumentMatrix(docs)

#verwijder sparse terms en maak matrix max 25% leeg
dtms <- removeSparseTerms(dtm,0.25)

