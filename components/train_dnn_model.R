source("lda/explore_lda.R")
source("dnn/model_dnn.R")


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
    
    h6("Training results"),
    plotOutput(ns("historyPlot"), width="900px", height="700px"),
    
    dataTableOutput(ns("historyTable")),
    
    h6("Test results"),
    
    dataTableOutput(ns("testTable"))
    
  )
}

train_model_srv <- function(input, output, session, loadFileResult, modelResult, labelledResult, clusterResults) {
  
  trainResult <- reactiveValues()
  
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
    trainP <- input$trainPartition
    validP <- input$testPartition/2.0
    testP <- input$testPartition/2.0
    
    labelName <- labelColName()
    dataSet <- inputDataSet()
    docTermMat <- inputDocTermMat()
    
    labelledData <- inputLabelledData()
    
    print(paste("Make data for training", trainP, validP, testP))
    
    ## Step 1. Define partitions for training and testing.
    ## Report sizes.
    modelData <- makeModelDataSet(labelledData, docTermMat, labelName, splits=c(trainP, validP, testP))
    
    trainResult$modelData <- modelData
    trainResult$docTermMat <- docTermMat
    trainResult$labelName <- labelName
    
    trainData <- getTrainAndTestSet(modelData)
    
    trainResult$trainData <- trainData
    
    print("Make model architecture")
    ## Step 2. Define the model architecture.
    model <- makeModelArchitectureAndCompile(modelData$nFeatures, 
                                             modelData$nClasses, 
                                             dropOut=TRUE, 
                                             dropRate=0.2)
    
    unlink("logs/run", recursive = TRUE)
    
    print("Train model")
    ## Step 3. Train the model.
    history <- trainModel(model, 
                          trainData$train_x, 
                          trainData$train_y, 
                          trainData$val_x, 
                          trainData$val_y, 
                          numEpochs=input$epochs, 
                          logdir="logs/run", 
                          withTensorBoard=TRUE, 
                          port=input$listenPort)
    
    trainResult$history <- history
    
    test <- evaluateModel(model,
            trainData$test_x,
            trainData$test_y)
    
    trainResult$testResults <- test
    
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
  