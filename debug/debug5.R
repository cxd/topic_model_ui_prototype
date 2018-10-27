source("lda/explore_lda.R")
source("dnn/model_dnn.R")

Rprof(filename="profile.out", memory.profiling=TRUE)

zipSrc <- "data/ABC_news_example/export_10_clusters.csv"

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
print("Step 1. make data")
modelData <- makeModelDataSet(labelledData, docTermMat, labelName, splits=c(0.7, 0.15, 0.15))
labelName = modelData$partitionLabelName

print("get partitions from data")
trainData <- getTrainAndTestSet(modelData)

## Step 2. Define the model architecture.
print("Step2. Define model architecture")
model <- makeModelArchitectureAndCompile(modelData$nFeatures, modelData$nClasses, dropOut=TRUE, dropRate=0.2)

unlink("logs/run", recursive=TRUE)

print("Step 3. Train model")
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

png("history.png", width=800,height=640)
plot(history)
dev.off()

trainingHistory(history)

validationHistory(history)

print("Step 4 evaluate model")
## Step 4. Evaluate the model.
test <- evaluateModel(model,
                      trainData$test_x,
                      trainData$test_y
)

test

pred <- testPredictions(model, trainData$test_x, modelData$classColNames, modelData$testData, labelName, dataSet)

head(pred)

summary(model)

print("Step 5. Examples")
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

print("Step 6 export model")

## Step 6. Save model.
tempFile <-  paste0("dnn_export_temp_",as.numeric(Sys.time()),".zip")
exportModel(tempFile, model, trainData)

tempFile2 <-  paste0("dnn_export_temp2_",as.numeric(Sys.time()),".zip")
exportToModelDirectory(tempFile2, model, trainData)
