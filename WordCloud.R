#My first WordCloud
setwd("C:/Users/Aravind Atreya/Desktop/WordCloud")

library(tm)
library(wordcloud)
library(RWeka)
library(qdap)


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),c("and","he","me","you","the")))
  return(corpus)
}

tokenizer <-  function (x) {
  NGramTokenizer(x,Weka_control(min=1,max=2))
}

docs <- Corpus(DirSource("C:/Users/Aravind Atreya/Desktop/WordCloud/File"))
for(j in 1:length(docs))   
{   
  docs[[j]][[1]] <- gsub("/", " ", docs[[j]][[1]])   
  docs[[j]][[1]] <- gsub(":", " ", docs[[j]][[1]])   
  docs[[j]][[1]] <- gsub("/", " ", docs[[j]][[1]])
  docs[[j]][[1]] <- gsub("<", " ", docs[[j]][[1]])
  docs[[j]][[1]] <- gsub(">", " ", docs[[j]][[1]])
  docs[[j]][[1]] <- gsub("-", " ", docs[[j]][[1]])
}  

docs_clean_corpus <- clean_corpus(docs)

df_tdm <- TermDocumentMatrix(docs_clean_corpus,control = list(tokenize=tokenizer))
df_m <- as.matrix(df_tdm)
termSums <- rowSums(df_m)
termSums <- sort(termSums,decreasing = T)
barplot(termSums[1:6])
prod_freqs <- data.frame(term = names(termSums),Frequency = termSums)
wordcloud(prod_freqs$term,prod_freqs$Frequency,max.words = 25,col=colors()[35:40])


