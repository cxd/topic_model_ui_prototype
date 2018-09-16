library(caret)
library(keras)

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
  oneHotClasses <- stats::model.matrix(formula, labelledDataSet)
  
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
  
  print(paste("test percent", testPercent))
  print(paste("valid percent", valPercent))
  
  print(length(testData[,labelName]))
  
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
    classColumnIdx=classColumnIdx,
    appTagIdx=appTagIdx,
    nClasses=nClasses,
    nFeatures=nFeatures
  )
}

## define the architecture of the model
## provide the number of input features and the number of classes.
## the model is then compiled ready for training.
makeModelArchitectureAndCompile <- function(nFeatures, nClasses, dropOut=TRUE, dropRate=0.2) {
  model <- if (dropOut == TRUE) {
    keras_model_sequential() %>% 
      layer_dense(units=nClasses/2, activation="relu", input_shape=c(nFeatures)) %>%
      layer_dense(units=nClasses/4, activation="relu") %>%
      layer_dropout(dropRate) %>%
      layer_dense(units=nClasses, activation="sigmoid")
  } else {
    keras_model_sequential() %>% 
      layer_dense(units=nClasses/2, activation="relu", input_shape=c(nFeatures)) %>%
      layer_dense(units=nClasses/4, activation="relu") %>%
      layer_dense(units=nClasses, activation="sigmoid")
  }
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
  model
}

getTrainAndTestSet <- function(modelData) {
  
  train_x <-as.matrix(modelData$trainData[,modelData$termDataIdx])
  train_y <- as.matrix(modelData$trainData[,modelData$classColumnIdx])
  
  val_x <- as.matrix(modelData$valData[,modelData$termDataIdx])
  val_y <- as.matrix(modelData$valData[,modelData$classColumnIdx])
  
  test_x <- as.matrix(modelData$testData[,modelData$termDataIdx])
  test_y <- as.matrix(modelData$testData[,modelData$classColumnIdx])
  
  list(
    train_x=train_x,
    train_y=train_y,
    val_x=val_x,
    val_y=val_y,
    test_x=test_x,
    test_y=test_y
  )
}

## train the model and return the history
## The history can be plotted.
trainModel <- function(model, train_x, train_y, val_x, val_y, numEpochs=50, logdir="logs/run", withTensorBoard=TRUE, port=5000) {
  unlink(logdir, recursive=TRUE)
  
  if (withTensorBoard == TRUE) {
    
    tensorboard(logdir, port=port, launch_browser = FALSE)
    
    history <- model %>% fit(
      as.matrix(train_x),
      as.matrix(train_y),
      epochs = numEpochs,
      validation_data = list(as.matrix(val_x), as.matrix(val_y)),
      
      callbacks = callback_tensorboard(logdir)
    )
    return (history)
  } else {
    history <- model %>% fit(
      as.matrix(train_x),
      as.matrix(train_y),
      epochs = numEpochs,
      validation_data = list(as.matrix(val_x), as.matrix(val_y))
    )
    return (history)
  }
} 

trainingHistory <- function(history) {
  spread(as.data.frame(history), metric, value) %>% filter(data=="training")
}


validationHistory <- function(history) {
  spread(as.data.frame(history), metric, value) %>% filter(data=="validation")
}

## Evaluate the supplied model.
evaluateModel <- function(model, test_x, test_y) {
  test <- model %>%
    evaluate(
      test_x,
      test_y
    )
  test
}

testPredictions <- function(model, test_x, classColNames, testData, labelName) {
  predictions <- model %>% predict(test_x)
  
  preddf <- as.data.frame(predictions)
  
  colnames(preddf) <- classColNames
  
  testAppTags <- sapply(1:nrow(preddf), function(i) {
    idx <- which(preddf[i,] == max(preddf[i,]))
    tag <- classColNames[idx]
    tag
  })
  testAppTags <- stringr::str_replace(testAppTags, labelName, "")
  testData$predictTag <- testAppTags
  testResults <- testData[,c("docid",labelName, "predictTag")]
  
  testResults <- inner_join(testResults, dataSet, by="docid")
  
  testResults
  
}

saveModel <- function(model, path) {
  export_savedmodel(model, path)
}

## Using the TFIDF feature matrix
## classify new examples via the model
classifyNewExamplesDnnTfIdf <- function(newUtterances=list(), dataSet, textSet, model, termDataIdx, classColNames, labelName) {
  
  newResult <- addUtteranceToTfIdf(newUtterances, dataSet, textSet)
  
  
  newRowId <- newResult$newRowIds
  
  docDataFrame <- reshape2::dcast(newResult$newDocMat, docid ~ word, value.var="tf_idf", fill=0.0)
  
  
  newTermMat <- docDataFrame[docDataFrame$docid == newRowId,]
  example <- newTermMat[,termDataIdx]
  
  example <- as.matrix(example)
  examplePredict <- model %>% predict(example)
  
  idx <- which(examplePredict == max(examplePredict))
  probability <- examplePredict[idx]              
  tag <- stringr::str_replace(classColNames[idx], labelName, "")
  
  d1 <- data.frame(tag=c(tag),probability=c(probability))
  
  tempDf <- data.frame(tags=stringr::str_replace(classColNames, labelName, ""), probability=t(examplePredict))
  tempDf <- tempDf %>% arrange(-probability)
  
  list(prediction=d1,
       allPredictions=tempDf)
}

## Use onehot encoding for the feature vector of the new utterances.
## this gives an approximation of the TFIDF in terms of 1 being maximum weight for the term 
## where it appears in the term vector, and 0 for terms that do not appear.
## hence terms in the new utterances to be classified are given maximum TFIDF in the new example.
## the classifier which was trained on the TFIDF then approximates using the maximal value of tfidf for the new examples.
## This method of encoding the input feature vector is faster than calculating a new tfidf matrix for new inputs.
## but is an approximation.
classifyNewExamplesDnnOneHot <- function(newUtterances=list(), model, maxDocId, termVocab, classColNames, labelName) {
  newUtterance <- data.frame(docid=seq(from=maxDocId+1, to=maxDocId+length(newUtterances), by=1),
                             text=newUtterances,
                             stringsAsFactors = FALSE)
  newText <- newUtterance %>% unnest_tokens(word, text)
  
  ## Keep only the text that is found in the known vocabulary of the model
  
  idx <- newText$word %in% termVocab
  
  newText <- newText[idx,]
  
  newFeature <- data.frame(feature=rep(0,length(termVocab)))
  newFeature$vocab <- termVocab
  idx <- which(newFeature$vocab %in% newText$word)
  newFeature[idx,]$feature <- 1.0
  example <- as.numeric(newFeature$feature)
  
  example <- t(as.matrix(example))
  
  examplePredict <- model %>% predict(example)
  
  idx <- which(examplePredict == max(examplePredict))
  
  probability <- examplePredict[idx]              
  
  tag <- stringr::str_replace(classColNames[idx], labelName, "")
  
  d1 <- data.frame(tag=c(tag),probability=c(probability))
  
  tempDf <- data.frame(tags=stringr::str_replace(classColNames, labelName, ""), probability=t(examplePredict))
  tempDf <- tempDf %>% arrange(-probability)
  
  list(prediction=d1,
       allPredictions=tempDf)
  
}

## export the model to zip file.
## The model is stored in hdf5 format which will save all weights and details of optimizer
## This will allow the model to be retrained at a later stage.
exportModel <- function(targetZipFile, model, modelData) {
  part <- .Platform$file.sep
  temp <- tempdir()
  folder <- paste0("dnn_export_temp_",as.numeric(Sys.time()))
  target <- paste(temp, folder, sep=part)
  if (dir.exists(target)) {
    unlink(target)
  }
  print(target)
  dir.create(target)
  
  modeltarget <- paste(target, "exported_model.hdf5", sep=part)
  modelDataFile <- paste(target, "modelData.RData", sep=part)
  
  save_model_hdf5(model, modeltarget, include_optimizer = TRUE)
  saveRDS(modelData, modelDataFile)
  
  zipOut <- zip(targetZipFile, c(modeltarget, modelDataFile))
 
  zipOut
}

## export the model to zip file.
## Export the tensorflow model is stored in a format suitable for use with tensorflow serving.

exportToModelDirectory <- function(targetZipFile, model, modelData) {
  part <- .Platform$file.sep
  temp <- tempdir()
  folder <- paste0("dnn_export_temp_",as.numeric(Sys.time()))
  target <- paste(temp, folder, sep=part)
  if (dir.exists(target)) {
    unlink(target, recursive=TRUE)
  }
  print(target)
  dir.create(target)
  
  modeltarget <- paste(target, "exported_model", sep=part)
  
  export_savedmodel(model, modeltarget, versioned=TRUE)
  
  zipOut <- zip(targetZipFile, c(modeltarget))
  
  zipOut
}

## Load the model from hdf5 along with model data.
## Returns a list with properties
## result$model - the model loaded from zip file
## result$modelData - the model data used to train the original model.
importModel <- function(inputZipFile) {
  part <- .Platform$file.sep
  temp <- tempdir()
  folder <- paste0("lda_export_temp_",as.numeric(Sys.time()))
  target <- paste(temp, folder, sep=part)
  if (dir.exists(target)) {
    unlink(target, recursive=TRUE)
  }
  dir.create(target)
  unzip(zipInFile, exdir=target,junkpaths=TRUE)
  
  modeltarget <- paste(target, "exported_model.hdf5", sep=part)
  
  modelDataFile <- paste(target, "modelData.RData", sep=part)
  
  modelData<- readRDS(modelDataFile)
  # load the hdf5 model
  model <- load_model_hdf5(modeltarget, compile=TRUE)
  
  unlink(target, recursive=TRUE)
  
  list(
    model=model,
    modelData=modelData
  )
}
