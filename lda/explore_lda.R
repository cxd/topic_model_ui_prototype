require(dplyr)
require(tidyr)
require(tidytext)
require(tm)
require(LDAvis)
require(topicmodels)
require(stringr)
require(ggplot2)


defineDataSet <- function(inData, textColName, labelName, rowId ="row", genRowIds=TRUE) {
  if (genRowIds) {
    inData[,rowId] <- as.character(seq(from=1,to=nrow(inData),  by=1))  
  }
  dataSet <- data.frame(docid=inData[,rowId], 
                        text=as.character(inData[,textColName]), 
                        appTag=inData[,labelName],
                        stringsAsFactors = FALSE)
  
  dataSet
}

makeTextDataSet <- function(dataSet) {
  textSet <- dataSet %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(docid, word, sort=TRUE) %>%
    ungroup()
  
  textSet
}

makeDocumentTermMatrix <- function(textSet) {
  docMat <- textSet %>%
    bind_tf_idf(word, docid, n)
  
  docTermMat <- textSet %>%
    cast_dtm(docid, word, n)
  
  docTermMat
}

buildLDA <- function(docTermMat, numTopics) {
   # generate the LDA model
  docLDA <- LDA(docTermMat, k=numTopics, control=list(seed=1234))
  docLDA
}

topNTerms <- function(ldaModel, topN) {
  
  docTopics <- tidy(ldaModel, matrix="beta")
  
  top_terms <- docTopics %>%
    group_by(topic) %>%
    top_n(topN, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms
}

# take a range of topics 1:N and the top N terms and produce a fact wrapped bar plot
# of each topic.
plotNTermsInTopicRange <- function(top_terms, topicRange) {
  p <- top_terms %>% filter(topic %in% topicRange) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  p
}

## this method plots the log ratio of the top terms for a start topic
## it is used to compare the topN terms of the start topic
## by log ratio to their occurance in the other topics.
## values further away from 0 are better.
plotNTermsLogRatioInRange <- function(ldaModel, startTopic, topN, numTopics, topicRange) {
  docTopics <- tidy(ldaModel, matrix="beta")
  
  top_terms <- docTopics %>%
    group_by(topic) %>%
    top_n(topN, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  topics <- seq(from=1, to=numTopics)
  others <- which(topics %in% c(startTopic))
  others <- topics[-others]
  
  
  # build a matrix with the log ratio where topic1 is assigned against topicN
  fromData <- docTopics[docTopics$topic == startTopic,]
  terms <- docTopics %>% count(term)
  
  subsetFrom <- docTopics[docTopics$topic == startTopic,]
  
  logData <- subsetFrom[,c("term")]
  
  for(j in 1:length(others)) {
    otherTopic <- others[j]
    subsetTo <- docTopics[docTopics$topic == otherTopic,]
    
    tempRatio <- inner_join(subsetFrom, subsetTo, by="term")  
    tempRatio <- tempRatio %>% mutate(log_ratio = log2(beta.y / beta.x))
    newName <- paste0("",otherTopic)
    logData <- inner_join(logData, tempRatio[,c("term","log_ratio")], by="term")
    names(logData)[names(logData) == "log_ratio"] <- newName
  }
  
  start_terms <- top_terms %>% filter(topic %in% startTopic)
  log_start_terms <- logData %>% filter(term %in% start_terms$term)
  log_start_terms <- log_start_terms %>% group_by(term) %>% gather(topic, log_ratio, 2:ncol(log_start_terms))
  
  p <- log_start_terms %>% filter(topic %in% topicRange) %>%
    arrange(as.numeric(topic)) %>%
    ggplot(aes(term, log_ratio, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  p
}

## cluster documents.
## given the data set and the set of manually assigned labels
## generate cluster assignments for each document and create a new dataset
## with ranks applied to each document and their associated cluster.
clusterDocuments <- function(dataSet, ldaModel, docTermMat, topicLabels) {
  probs_doc_topic <- topicmodels::posterior(ldaModel, docTermMat)
  docProbs <- as.data.frame(probs_doc_topic$topics)
  docProbs$docid <- as.character(row.names(docProbs))
  
  topTopics <- data.frame(apply(probs_doc_topic$topics, 1, which.max))
  topTopics$docid <- row.names(topTopics)
  colnames(topTopics) <- c("topic", "docid")
  
  nTopics <- nrow(topicLabels)
  ranks <- docProbs %>% gather(topic, probability, 1:nTopics)
  
  ranked <- inner_join(topTopics, ranks, by=c("docid")) %>% 
    filter(topic.x == topic.y)
  
  head(ranked)
  tempData <- dataSet
  tempData$docid <- as.character(dataSet$docid)
  
  tempData <- inner_join(ranked, tempData, by="docid")
  tempData$topic <- tempData$topic.x
  tempData <- inner_join(tempData, topicLabels, by="topic")
  
  data.frame(topic=tempData$topic,
             label=tempData$label,
             text=tempData$text,
             probability=tempData$probability,
             stringsAsFactors = FALSE)
}

## get the sizes for the cluster
getClusterSizes <- function(clusteredDocs) {
  cluster_sizes <- clusteredDocs %>% group_by(topic,label) %>% count(topic,label)
  cluster_sizes
}

boxPlotClusterSizes <- function(clusteredDocs) {
  cluster_sizes <- getClusterSizes(clusteredDocs)
  ids <- as.character(cluster_sizes$topic)
  labels <- cluster_sizes$label
  labels <- paste(ids,labels," ")
  
  p <- ggplot(cluster_sizes, aes(x=topic, y=n)) +
    geom_bar(stat="identity") + 
    scale_x_discrete(limits=labels) +
    coord_flip() +
    ggtitle("Number of documents assigned to each topic")
  p
}