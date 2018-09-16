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

# for a range of number of topics, run lda and then calculate the model perplexity.
# return a list containing the set of models
# the set of perplexity scores for each model
# the set of log perplexity scores for each model.
assessModelPerplexity <- function(docTermMat, numTopicsRange) {
  docLDA <- sapply(numTopicsRange, 
                   function(kN) { 
                     print(paste("Building model with",kN,"topics"))
                     LDA(docTermMat, k=kN, control=list(seed=1234))
                     })
  perplexity1 <- sapply(docLDA, function(lda) {
    perplexity(lda)
  }) 
  list(perplexity=perplexity1,
       logPerplexity=log(perplexity1),
       range=numTopicsRange,
       models=docLDA)
}
# plot the perplexity
plotPerplexity <- function(pDataResult) {
  numTopics <- pDataResult$range
  logP <- pDataResult$logPerplexity
  pdata <- data.frame(topicCount=numTopics, logPerplexity=logP)
  p <- ggplot(pdata, aes(x=topicCount,y=logPerplexity)) +
    geom_line(stat="identity") +
    geom_point() + 
    ggtitle("Log Perplexity Per Number of Topic")
  p
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
             docid=tempData$docid,
             text=tempData$text,
             probability=tempData$probability,
             stringsAsFactors = FALSE)
}

## get the sizes for the cluster
getClusterSizes <- function(clusteredDocs) {
  cluster_sizes <- clusteredDocs %>% group_by(topic,label) %>% 
    count(topic,label)
  cluster_sizes
}

# plot cluster sizes
boxPlotClusterSizes <- function(clusteredDocs) {
  cluster_sizes <- getClusterSizes(clusteredDocs)
  ids <- cluster_sizes$topic
  labels <- cluster_sizes$label
  labels <- paste(ids,labels," ")
  
  p <- ggplot(cluster_sizes, aes(x=topic, y=n)) +
    geom_bar(stat="identity") + 
    scale_x_discrete(limits=labels) +
    coord_flip() +
    ggtitle("Number of documents assigned to each topic")
  p
}

suggestLabels <- function(ldaModel, numTerms, labelledDocs, topics) {
  
  #
  top <- topNTerms(ldaModel, numTerms)
  
  # now for each cluster we need to identify the top bigrams
  bigrams <- labelledDocs %>%
    unnest_tokens(bigram, text, token="ngrams", n=3) %>%
    count(bigram, topic, sort=TRUE)
  
  ## select the top terms in each topic
  max_top <- top %>% group_by(topic) %>% filter(beta == max(beta))
  
  
  
  beginsWithTopTerm <- function(item) {
    idx <- which(startsWith(item, max_top$term))
    len <- length(idx)
    len > 0
  }
  
  max_bigrams <- bigrams %>% filter(beginsWithTopTerm(bigram))
  
  max_bigrams <- inner_join(top, max_bigrams, by="topic")
  
  max_bigrams <- max_bigrams[startsWith(max_bigrams$bigram, max_bigrams$term),]
  
  max_bigrams <- max_bigrams %>% group_by(topic) %>% 
    filter(beta == max(beta)) %>%
    filter(n == max(n)) %>%
    arrange(-beta)
  
  suggestions <- max_bigrams %>% 
    group_by(topic) %>% 
    summarise(suggestions=paste(bigram,collapse=","))
  
  topics <- inner_join(topics, suggestions, by="topic")
  
  topics 
}

addUtteranceToTfIdf <- function(newUtterances=list(), dataSet, textSet) {
  newDocId <- max(as.numeric(dataSet$docid)) + seq(from=1, to=length(newUtterances))
  
  newUtterance <- data.frame(docid=as.character(newDocId),
                             text=newUtterances,
                             stringsAsFactors = FALSE)
  
  newCorpus <- data.frame(docid=dataSet$docid,
                          text=dataSet$text,
                          stringsAsFactors = FALSE)
  
  newCorpus <- rbind(newCorpus, newUtterance)
  
  newTextSet <- newCorpus %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(docid, word, sort=TRUE) %>%
    ungroup()
  
  # remove records from the newTextSet that did not exist in the original textSet
  newTextSet <- newTextSet %>%
    filter(word %in% unique(textSet$word))
  
  # build the tf_idf matrix
  docMat <- newTextSet %>%
    bind_tf_idf(word, docid, n)
  
  # create a matrix for the new document.
  newDocTermMat <- newTextSet %>%
    filter(docid == newDocId) %>%
    cast_dtm(docid, word, n)
  
  list(newDocTermMat=newDocTermMat,
       newDocMat=docMat,
       newRowIds=newDocId)
}

classifyNewTermMat <- function(newDocTermMat, newRowIds, ldaModel, numTopics) {
  new_probs_doc_topic <- topicmodels::posterior(ldaModel, newDocTermMat)
  newDocProbs <- as.data.frame(new_probs_doc_topic$topics)
  newDocProbs$docid <- as.character(row.names(newDocProbs))
  
  newTopTopics <- data.frame(apply(new_probs_doc_topic$topics, 1, which.max))
  newTopTopics$docid <- row.names(newTopTopics)
  colnames(newTopTopics) <- c("topic", "docid")
  
  newRanks <- newDocProbs %>% 
    gather(topic, probability, 1:numTopics) %>% 
    arrange(-probability)
  
  newRanked <- inner_join(newTopTopics, newRanks, by=c("docid")) %>% 
    filter(topic.x == topic.y)
  
  newRanked$topic <- newRanked$topic.x
  idx <- which(colnames(newRanked) == "topic.x" || colnames(newRanked) == "topic.y")
  newRanked <- newRanked[,-idx]
  
  
  list(ranked=newRanked,
       allTopicsRanked=newRanks)
}

classifyNewExamples <- function(newUtterances=list(), dataSet, textSet, ldaModel, numTopics) {
  
  termMatResult <- addUtteranceToTfIdf(newUtterances, dataSet, textSet)
  classifyNewTermMat(termMatResult$newDocTermMat, 
                     termMatResult$newRowIds, 
                     ldaModel, 
                     numTopics) 
}

## Write an lda model to a file


exportLDAModel <- function(targetZipFile, dataSet, ldaModel, textData, termMat, metaData) {
  part <- .Platform$file.sep
  temp <- tempdir()
  folder <- paste0("lda_export_temp_",as.numeric(Sys.time()))
  target <- paste(temp, folder, sep=part)
  if (dir.exists(target)) {
    unlink(target, recursive=TRUE)
  }
  print(target)
  dir.create(target)
  modelFile <- paste(target,"ldamodel.RData",sep=part)
  dataSetFile <- paste(target, "dataSet.RData", sep=part)
  textDataFile <- paste(target, "textSet.RData", sep=part)
  termMatFile <- paste(target, "termMat.RData", sep=part)
  metaDataFile <- paste(target, "metaData.RData", sep=part)
  saveRDS(ldaModel, modelFile)
  saveRDS(dataSet, dataSetFile)
  saveRDS(textData, textDataFile)
  saveRDS(termMat, termMatFile)
  saveRDS(metaData, metaDataFile)
  zipOut <- zip(targetZipFile, c(modelFile, dataSetFile, textDataFile, termMatFile, metaDataFile))
  unlink(target)
  zipOut
}

## Import an LDA model that has been unzipped.
importLDAModel <- function(zipInFile) {
  part <- .Platform$file.sep
  temp <- tempdir()
  folder <- paste0("lda_export_temp_",as.numeric(Sys.time()))
  target <- paste(temp, folder, sep=part)
  if (dir.exists(target)) {
    unlink(target)
  }
  dir.create(target)
  unzip(zipInFile, exdir=target,junkpaths=TRUE)
  modelFile <- paste(target,"ldamodel.RData",sep=part)
  dataSetFile <- paste(target, "dataSet.RData", sep=part)
  textDataFile <- paste(target, "textSet.RData", sep=part)
  termMatFile <- paste(target, "termMat.RData", sep=part)
  metaDataFile <- paste(target, "metaData.RData", sep=part)
  
  ldaModel <- readRDS(modelFile)
  dataSet <- readRDS(dataSetFile)
  textData <- readRDS(textDataFile)
  termMat <- readRDS(termMatFile)
  metaData <- readRDS(metaDataFile)
  #modelResult$textData <- loadFileResult$result$textData
  #modelResult$termMat <- loadFileResult$result$termMat
  
  result <- list(
    ldaModel=ldaModel,
    dataSet=dataSet,
    textData=textData,
    termMat=termMat,
    metaData=metaData
  )
  unlink(target)
  result
}
