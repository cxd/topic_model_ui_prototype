library(caret)

## The model data set is a collection of document term vectors
## for each example and a onehot encoded matrix for the labels assigned to the data frame. 
##
## the splits parameter contains the 3 partitions to make
## Test training set defaults to 60%
## validation defaults to 15%
## test defaults to 25%
makeModelDataSet <- function(labelledDataSet, docTermMat, labelName, splits=c(0.6, 0.15, 0.25)) {
  temp <- as.data.frame(docTermMat)
  docDataFrame <- reshape2::dcast(temp, docid ~ word, value.var="tf_idf", fill=0.0)
  
  formula <- as.formula(paste0("~", labelName))
  oneHotClasses <- stats::model.matrix(forumla, labelledDataSet)
  
  oneHotClasses <- cbind(data.frame(docid=as.character(labelledDataSet$docid),
                                    stringsAsFactors = FALSE), oneHotClasses)
  combinedData <- inner_join(docDataFrame, oneHotClasses, by=c("docid"))
  
  # remove the docid from the data
  cidx1 <- which(colnames(combinedData) == "docid")
  # remove the docid from the data
  cidx2 <- which(colnames(oneHotClasses) == "docid")
  # remove the intercept from the data.
  intercept <- which(colnames(oneHotClasses) == "(Intercept)")
  # remove the docid from the data  
  cidx3 <- which(colnames(docDataFrame) == "docid")
  
  # get the unique classes
  classColNames <- (colnames(oneHotClasses))[-c(cidx2,intercept)]
  
  # get the vocabulary in order of columns
  termVocab <- (colnames(docDataFrame))[-cidx3]
  
  # store the class column index
  classColumnIdx <- which(colnames(combinedData) %in% classColNames)
  
  # store the term data index.
  termDataIdx <- which(colnames(combinedData) %in% termVocab)
  
  ## number of outputs in our network corresponds to number of classes
  nClasses <- length(classColumnIdx)
  
  ## number of features in the network for inputs.
  nFeatures <- length(termDataIdx)
  
  textIdx <- which(colnames(labelledDataSet) %in% "text")
  
  combinedData <- inner_join(combinedData, dataSet[,-textIdx], by="docid")
  
  appTagIdx <- which(colnames(combinedData) == labelName)
  
  # only keep complete cases.
  combinedData <- combinedData[complete.cases(combinedData),]
  
  # we create the partitions
  trainPercent <- splits[1]
  valPercent <- splits[2]
  testPercent <- splits[3]
  
  
  trainIndex <- createDataPartition(combinedData[,labelName], p=trainPercent, list=FALSE, times=1)
  trainData <- combinedData[trainIndex,]
  testData <- combinedData[-trainIndex,]
  
  totalRows <- nrow(combinedData)
  testRows <- nrow(testData)
  ratio <- totalRows / testRows
  ## what is the remaining percentage of the test data?
  valPercent <- ratio * valPercent
  testPercent <- ratio * testPercent
  
  valIndex <- createDataPartition(testData[,labelName], p=valPercent, list=FALSE, times=1)
  
  valData <- testData[valIndex,]
  testData <- testData[-valIndex,]
  
  trainProps <- trainData %>% group_by(trainData[,labelName]) %>% count()
  colnames(trainProps) <- c(labelName,"n")
  
  valProps <- valData %>% group_by(valData[,labelName]) %>% count()
  colnames(valProps) <- c(labelName,"n")
  
  
  testProps <- testData %>% group_by(testData[,labelName]) %>% count()
  colnames(testProps) <- c(labelName,"n")
  
  
  trainProps$pc <- trainProps$n / nrow(trainProps)
  
  valProps$pc <- valProps$n / nrow(valProps)
  testProps$pc <- testProps$n / nrow(testProps)
  
  list(
    trainData=trainData,
    valData=valData,
    testData=testData,
    trainProps=trainProps,
    valProps=valProps,
    testProps=testProps,
    combinedData=combinedData,
    classColNames=classColNames,
    termVocab=termVocab,
    termDataIdx=termDataIdx,
    appTagIdx=appTagIdx,
    nClasses=nClasses,
    nFeatures=nFeatures
  )
}