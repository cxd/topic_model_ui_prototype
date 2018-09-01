
source("../explore_lda.R")

zipSrc <- "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/lda_export_temp_1535274680.95233.zip"

modelResult <- importLDAModel(zipSrc)

numTerms <- 10

top <- topNTerms(modelResult$ldaModel, numTerms)

top


dataSet <- modelResult$dataSet
model <- modelResult$ldaModel
docMat <- modelResult$termMat
numTopics <- modelResult$metaData$numTopics
topics <- data.frame(topic=1:numTopics,
                     label=rep("unknown", numTopics),
                     stringsAsFactors = FALSE)

labelledDocs <- clusterDocuments(dataSet, model, docMat, topics) 

# now for each cluster we need to identify the top bigrams
bigrams <- labelledDocs %>%
  unnest_tokens(bigram, text, token="ngrams", n=2) %>%
  count(bigram, topic, sort=TRUE)

head(bigrams)

head(top)

## select the top terms in each topic
max_top <- top %>% group_by(topic) %>% filter(beta == max(beta))

max_bigrams <- bigrams[startsWith(bigrams$bigram, top$term),]
max_bigrams <- inner_join(top, max_bigrams, by="topic")

max_bigrams <- max_bigrams[startsWith(max_bigrams$bigram, max_bigrams$term),]

max_bigrams <- max_bigrams %>% group_by(topic) %>% filter(beta == max(beta))

max_bigrams

suggestions <- max_bigrams %>% group_by(topic) %>% summarise(suggestions=paste(bigram,collapse=","))

suggestions
