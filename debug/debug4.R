source("lda/explore_lda.R")
source("dnn/model_dnn.R")

zipSrc <- "/Users/cd/Projects/RStudio/topic_model_tools/data/abc_news/test_example.csv"

data <- read.csv(zipSrc,header=TRUE)


textColName <- "text"
labelColName <- "label"

dataSet <-    defineDataSet(data, 
                            textColName, 
                            labelColName)


textData <- makeTextDataSet(dataSet)
docMat <- makeDocumentTermMatrix(textData)

docTermMat <- textData %>%
  bind_tf_idf(word, docid, n)

labelName <- "appTag"

modelData <- makeModelDataSet(dataSet, docTermMat, labelName, splits=c(0.6, 0.15, 0.25))

names(modelData)

trainData <- getTrainAndTestSet(modelData)

model <- makeModelArchitectureAndCompile(modelData$nFeatures, modelData$nClasses, dropOut=TRUE, dropRate=0.2)



history <- trainModel(model, trainData$train_x, trainData$train_y, 
                      trainData$val_x, trainData$val_y, numEpochs=150, logdir="logs/run", 
                      withTensorBoard=TRUE, port=5000)

plot(history)

trainingHistory(history)

validationHistory(history)

test <- evaluateModel(model,
    trainData$test_x,
    trainData$test_y
  )

test

pred <- testPredictions(model, trainData$test_x, modelData$classColNames, modelData$testData, labelName)

head(pred)

summary(model)

d1 <- classifyNewExamplesDnn(newUtterances=c("nsw police start wearing bright uniforms"), 
                       dataSet, 
                       textData, 
                       model, 
                       modelData$termDataIdx,
                       modelData$classColNames, 
                       labelName)

d1

  