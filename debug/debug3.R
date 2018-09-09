source("../lda/explore_lda.R")


zipSrc <- "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/abcnews-date-text-small.csv"

data <- read.csv(zipSrc,header=TRUE)

textColName <- "headline_text"
labelColName <- "X.1"

dataSet <-    defineDataSet(data, 
                            textColName, 
                            labelColName)
numTopics <- 10
numTerms <- 10

textData <- makeTextDataSet(dataSet)
docMat <- makeDocumentTermMatrix(textData)
model <- buildLDA(docMat, numTopics)

labels <- sapply(1:10, function(i) paste0("topic",i))

topics <- data.frame(topic=1:numTopics,
                     label=labels,
                     stringsAsFactors = FALSE)

labelledDocs <- clusterDocuments(dataSet, model, docMat, topics) 

write.csv(labelledDocs, "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/test_example.csv",
          row.names=FALSE)
