source("lda/explore_lda.R")
source("dnn/model_dnn.R")

zipSrc <- "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/export_10_clusters.csv"

labelledData <- read.csv(zipSrc,header=TRUE)
data <- labelledData

textColName <- "text"
labelColName <- "label"

dataSet <-    defineDataSet(data, 
                            textColName, 
                            labelColName)


textData <- makeTextDataSet(dataSet)
docMat <- makeDocumentTermMatrix(textData)

docTermMat <- textData %>%
  bind_tf_idf(word, docid, n)

labelName <- "label"


## Step 1. Define partitions for training and testing.
## Report sizes.
modelData <- makeModelDataSet(labelledData, docTermMat, labelName, splits=c(0.7, 0.15, 0.15))
labelName = modelData$partitionLabelName
names(modelData)

trainData <- getTrainAndTestSet(modelData)

## Step 2. Define the model architecture.
model <- makeModelArchitectureAndCompile(modelData$nFeatures, modelData$nClasses, dropOut=TRUE, dropRate=0.2)

unlink("logs/run", recursive=TRUE)

## Step 3. Train the model.
history <- trainModel(model, 
                      trainData$train_x, 
                      trainData$train_y, 
                      trainData$val_x, 
                      trainData$val_y, 
                      numEpochs=150, 
                      logdir="logs/run", 
                      withTensorBoard=TRUE, 
                      port=5000)

plot(history)

trainingHistory(history)

validationHistory(history)

## Step 4. Evaluate the model.
test <- evaluateModel(model,
    trainData$test_x,
    trainData$test_y
  )

test

pred <- testPredictions(model, trainData$test_x, modelData$classColNames, modelData$testData, labelName, dataSet)

head(pred)

summary(model)

## Step 5. Interactive examples.
d1 <- classifyNewExamplesDnnTfIdf(newUtterances=c("nsw police start wearing bright uniforms"), 
                       dataSet, 
                       textData, 
                       model, 
                       modelData$termDataIdx,
                       modelData$classColNames, 
                       labelName)

d1

## Example using one hot encoding to approximate classification
maxDocId <- nrow(dataSet)
d2 <- classifyNewExamplesDnnOneHot(newUtterances=c("nsw police start wearing bright uniforms"), 
                             model,
                             maxDocId,
                             modelData$termVocab,
                             modelData$classColNames,
                             labelName)

d2

## Step 6. Save model.
tempFile <-  paste0("dnn_export_temp_",as.numeric(Sys.time()),".zip")
exportModel(tempFile, model, trainData)

tempFile2 <-  paste0("dnn_export_temp2_",as.numeric(Sys.time()),".zip")
exportToModelDirectory(tempFile2, model, trainData)

  