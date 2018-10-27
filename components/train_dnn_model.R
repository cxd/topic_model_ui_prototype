source("lda/explore_lda.R")
source("dnn/model_dnn.R")
require(ggplot2)
require(ggiraph)
require(magrittr)

train_model_ui <- function(id) {
  ns <- NS(id)
  verticalLayout(
    
    inputPanel(
      h6("Define training parameters"),
      numericInput(ns("trainPartition"), 
                   "Training Proportion", 
                   value=0.7, min=0.5, max=0.95),
      numericInput(ns("testPartition"), 
                   "Test and Validate Proportions", 
                   value=0.3, min=0.5, max=0.95),
      
      numericInput(ns("epochs"),
                   "Number of training epochs",
                   value=100, min=50),
      
      numericInput(ns("listenPort"),
                   "Network Port for tensorboard",
                   value=5000),
      
      actionButton(ns("trainBtn"), "Train Network")
    ),
    
    inputPanel(
      h6("Download trained model."),
      downloadButton(ns("downloadNNModel"), label="Download DNN Model and Data"),
      
      downloadButton(ns("downloadTSOnly"), label="Download Tensorflow Model Only")
    ),
    
    h3("Training results"),
    
    plotOutput(ns("historyPlot"), width="900px", height="700px"),
    
    dataTableOutput(ns("historyTable")),
    
    h3("Test results"),
    
    dataTableOutput(ns("testTable"))
    
  )
}

train_model_srv <- function(input, output, session, loadFileResult, modelResult, labelledResult, clusterResults) {
  

  
  
  ## A realtime callback for training the DNN
  HistoryAggregator <- R6::R6Class("HistoryAggregator", 
                                   inherit=KerasCallback,
                                  
                                   public=list(
                                     
                                     trainAccuracy=list(),
                                     trainLoss=list(),
                                     valAccuracy=list(),
                                     valLoss=list(),
                                     epochs=list(),
                                    
                                     reset = function() {
                                       
                                       self$trainAccuracy=list()
                                       self$trainLoss=list()
                                       self$valAccuracy=list()
                                       self$valLoss=list()
                                       self$epochs=list()
                                     },
                                     
                                     on_epoch_end = function(epoch, logs=list()) {
                                       ##browser()
                                       
                                       self$epochs <- c(self$epochs, c(epoch))
                                       acc <- logs[["acc"]]
                                       self$trainAccuracy <- c(self$trainAccuracy, c(acc))
                                       loss <- logs[["loss"]]
                                       self$trainLoss <- c(self$trainLoss, c(loss))
                                       val_acc <- logs[["val_acc"]]
                                       self$valAccuracy <- c(self$valAccuracy, c(val_acc))
                                       val_loss <- logs[["val_loss"]]
                                       self$valLoss <- c(self$valLoss, c(val_loss))
                                    
                                       print(paste(self$epochs))
                                     }
                                   ))
  
  
  
  aggregator <- HistoryAggregator$new()
  
  trainResult <- reactiveValues()
  
  ## train history
  trainHistory <- reactive({
    invalidateLater(1000, session)
    df <- data.frame(
    epoch=aggregator$epochs,
    trainAccuracy=aggregator$trainAccuracy,
    trainLoss=aggregator$trainLoss,
    valAccuracy=aggregator$valAccuracy,
    valLoss=aggregator$valLoss)
  
    if (nrow(df) == 0) {
      df <- data.frame(
        epoch=c(0),
        trainAccuracy=c(0),
        trainLoss=c(0),
        valAccuracy=c(0),
        valLoss=c(0))
      
    }
    ##browser()
    
  colnames(df) <- c("epoch",
                    "trainAccuracy",
                    "trainLoss",
                    "valAccuracy",
                    "valLoss")
  return (df)})
  
  labelColName <- reactive({
    validate(need(loadFileResult, message=FALSE))
    validate(need(loadFileResult$result, message=FALSE))
    print("labels")
    ## label from topics
    ## TODO: need to provide option to use label from data
    ## return (loadFileResult$result$labelColName)
    return ("label")
  })
  
  inputDataSet <- reactive({
    validate(need(modelResult, message=FALSE))
    validate(need(modelResult$dataSet, message=FALSE))
    print("dataset")
    return (modelResult$dataSet) 
  })
  
  inputLabelledData <- reactive({
    validate(need(clusterResults$labelledDocs, message=FALSE))
    print("labelled docs")
    return (clusterResults$labelledDocs)
  })
  
  inputDocTermMat <- reactive({
    validate(need(modelResult, message=FALSE))
    validate(need(modelResult$textData, message=FALSE))
    
    print("doc term mat")
    
    docTermMat <- modelResult$textData %>%
      bind_tf_idf(word, docid, n)
    
    return (docTermMat) 
  })
  
  observe({
    ns <- session$ns
    tp <- input$trainPartition
    testp <- 1.0 - tp
    updateNumericInput(session, ns("testPartition"), value=testp)
  })
  
  observe({
    ns <- session$ns
    tp <- input$testPartition
    trainp <- 1.0 - tp
    updateNumericInput(session, ns("trainPartition"), value=trainp)
  })
  
  
  observeEvent(input$trainBtn, {
    ## R break function
    ## browser()
    trainP <- input$trainPartition
    validP <- input$testPartition/2.0
    testP <- input$testPartition/2.0
    
    labelName <- labelColName()
    dataSet <- inputDataSet()
    docTermMat <- inputDocTermMat()
    
    labelledData <- inputLabelledData()
    listenPort <- input$listenPort
    epochs <- input$epochs
    unlink("logs/run", recursive = TRUE)
    
    aggregator$reset()
    
    runTraining <- function() {
    
    result <- list()
    
    print(paste("Make data for training", trainP, validP, testP))
    
    ## Step 1. Define partitions for training and testing.
    ## Report sizes.
    modelData <- makeModelDataSet(labelledData, docTermMat, labelName, splits=c(trainP, validP, testP))
    
    result$modelData <- modelData
    result$docTermMat <- docTermMat
    result$labelName <- labelName
    
    trainData <- getTrainAndTestSet(modelData)
    
    result$trainData <- trainData
    
    print("Make model architecture")
    ## Step 2. Define the model architecture.
    model <- makeModelArchitectureAndCompile(modelData$nFeatures, 
                                             modelData$nClasses, 
                                             dropOut=TRUE, 
                                             dropRate=0.2)
    
  
    result$model <- model
    
    result$modelfile <- saveModelHDF5(model)
    
    print("Train model")
    ## Step 3. Train the model.
    
    withTensorBoardFlag <- interactive()
    
    history <- trainModel(model, 
                          trainData$train_x, 
                          trainData$train_y, 
                          trainData$val_x, 
                          trainData$val_y, 
                          numEpochs=epochs, 
                          logdir="logs/run", 
                          withTensorBoard=withTensorBoardFlag, 
                          port=listenPort,
                          callbackSet=list(aggregator))
    
    result$history <- history
    
    test <- evaluateModel(model,
            trainData$test_x,
            trainData$test_y)
    
    result$testResults <- test
    
    return (result)
    }
    
    assignResult <- function(result) {
      trainResult$model <- result$model
      trainResult$modelfile <- result$modelfile
      trainResult$modelData <- result$modelData
      trainResult$docTermMat <- result$docTermMat
      trainResult$labelName <- result$labelName
      trainResult$trainData <- result$trainData
      trainResult$history <- result$history
      trainResult$testResults <- result$testResults
      trainResult
    }
    withProgress(message="Building Model", {
      incProgress()
      runTraining() %>%
        assignResult()
      setProgress(1)
    })
  })
  
 
  
  output$historyPlot <- renderPlot({
    validate(need(trainResult$history, message=FALSE))
    plot(history)
  })
  
  output$historyTable <- renderDataTable({
    validate(need(trainResult$history, message=FALSE))
    t1 <- trainingHistory(history)
    t2 <- validationHistory(history)
    t <- inner_join(t1,t2, by=c("epoch"))
    return (t)
  }, options=list(
    pageLength=10
  ))
  
  output$testTable <- renderDataTable({
    validate(need(trainResult$testResults, message=FALSE))
    test <- trainResult$testResults
    return(data.frame(loss=c(test$loss),
                      accuracy=c(test$acc)))
  })
  
  output$downloadNNModel <- downloadHandler(
    filename=function() {
      paste0("dnn_export_temp_",as.numeric(Sys.time()),".zip")
    },
    content = function(conn) {
      if (!is.null(trainResult$model)) {
        exportModel(conn, trainResult$model, trainResult$modelData)
      }
    },
    contentType="application/zip")
  
  output$downloadTSOnly <- downloadHandler(
    filename=function() {
      paste0("dnn_export_tensorflow_",as.numeric(Sys.time()),".zip")
    },
    content=function(conn) {
      if (!is.null(trainResult$model)) {
        exportToModelDirectory(conn, trainResult$model, trainResult$modelData)
      }
    },
    contentType="application/zip")
  
  return (trainResult)
}
  