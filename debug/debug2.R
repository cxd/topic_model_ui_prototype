
source("../explore_lda.R")

zipSrc <- "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/abcnews-date-text-small.csv"

data <- read.csv(zipSrc,header=TRUE)

textColName <- "headline_text"
labelColName <- "X.1"

dataSet <-    defineDataSet(data, 
                            textColName, 
                            labelColName)
numTopics <- 30
numTerms <- 10

textData <- makeTextDataSet(dataSet)
docMat <- makeDocumentTermMatrix(textData)
model <- buildLDA(docMat, numTopics)

top <- topNTerms(model, numTerms)

topCnt <- top %>% group_by(topic) %>% count()

ggplot(topCnt, aes(x=topic, y=n)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=topCnt$topic)

topics <- data.frame(topic=1:numTopics,
                     label=rep("unknown", numTopics),
                     stringsAsFactors = FALSE)

topics

labelledDocs <- clusterDocuments(dataSet, model, docMat, topics) 

clusterSizes <- labelledDocs %>% group_by(topic) %>% count()

ggplot(clusterSizes, aes(x=topic, y=n)) +
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=clusterSizes$topic)


# now for each cluster we need to identify the top bigrams
bigrams <- labelledDocs %>%
  unnest_tokens(bigram, text, token="ngrams", n=2) %>%
  count(bigram, topic, sort=TRUE)
  

bigramCounts <- bigrams %>% group_by(topic) %>% count()

ggplot(bigramCounts, aes(x=topic, y=nn)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=bigramCounts$topic)



head(bigrams)

head(top)

## select the top terms in each topic
max_top <- top %>% group_by(topic) %>% filter(beta == max(beta))

plot(max_top$beta ~ max_top$topic, type="b")


beginsWithTopTerm <- function(item) {
  idx <- which(startsWith(item, max_top$term))
  len <- length(idx)
  len > 0
}

max_bigrams <- bigrams %>% filter(beginsWithTopTerm(bigram))



max_bigrams <- inner_join(top, max_bigrams, by="topic")

temp <- max_bigrams %>% group_by(topic) %>% count()
ggplot(temp, aes(x=topic, y=nn)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=temp$topic)

max_bigrams <- max_bigrams[startsWith(max_bigrams$bigram, max_bigrams$term),]


max_bigrams <- max_bigrams %>% group_by(topic) %>% 
  filter(beta == max(beta)) %>%
  filter(n == max(n))

max_bigrams

suggestions <- max_bigrams %>% group_by(topic) %>% summarise(suggestions=paste(bigram,collapse=","))

suggestions


write.csv(suggestions, "temp.csv")
