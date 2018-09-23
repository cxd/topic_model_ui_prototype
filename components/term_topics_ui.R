

source("lda/explore_lda.R")


term_topics_ui <- function(id) {
  ns <- NS(id)
  
  verticalLayout(
    inputPanel(
      numericInput(ns("numTopics"), 
                   "Number of preferred topics.", 
                   value=10, min=2),
      numericInput(ns("numTerms"),
                   "Numer of terms per topic",
                   value=10),
      uiOutput(ns("chooseTopic"))
    ),
    inputPanel(
      h6("Log Perplexity for N-Topics"),
      textOutput(ns("logPerplexity"))
    ),
    downloadButton(ns("downloadModel"), label="Download LDA Model"),
    h5("Review Top N Terms"),
    plotOutput(ns("topicPlot"), width="900px", height="1024px")
    
  )
}


## the load file result is a list with the properties for the source file
## 
term_topics_observer <- function(input, output, session, loadFileResult=list(), tabPanel, tabName) {
  
  modelResult <- reactiveValues()
  
  inputDataSet <- reactive({
    validate(need(loadFileResult$result, message=FALSE))
    
    if (!is.null(loadFileResult$result$dataSet)) {
      dataSet <- loadFileResult$result$dataSet
      modelResult$dataSet <- dataSet 
      return(dataSet)
    }
    
    validate(need(loadFileResult$result$srcFile, message=FALSE))
      
      result <- loadFileResult$result
      
      data <- read.csv(result$srcFile, header=TRUE)
      
      dataSet <- if (result$hasDocIdCol) {
        defineDataSet(data, 
                      result$textColName, 
                      result$labelColName, 
                      result$docIdName, 
                      genRowIds=FALSE)
      } else {
        defineDataSet(data, 
                      result$textColName, 
                      result$labelColName)
      }
      modelResult$dataSet <- dataSet 
      return(dataSet)
  })
  
  textDataSet <- reactive({
    inputDataSet <- inputDataSet()
    textData <- makeTextDataSet(inputDataSet)
    modelResult$textData <- textData
    return(textData)
  })
  
  docTermMatrix <- reactive({
    termMat <- makeDocumentTermMatrix(textDataSet())
    modelResult$termMat <- termMat
    
    return(termMat)
  })
  
  ldaModel <- reactive({
    ns <- session$ns
    
    if (!is.null(loadFileResult$result$ldaModel)) {
      modelResult$model <- loadFileResult$result$ldaModel
      
      modelResult$textData <- loadFileResult$result$textData
      modelResult$termMat <- loadFileResult$result$termMat
      modelResult$numTopics <- loadFileResult$result$metaData$numTopics
      updateNumericInput(session, ns("numTopics"), value=modelResult$numTopics)
      
      if (!is.null(modelResult$hasChanged) && modelResult$hasChanged == FALSE) {
        modelResult$numTerms <- input$numTerms
        
        return (modelResult$model)
      } else if (is.null(modelResult$hasChanged)) {
        return (modelResult$model)
      }
      
      
    } 
    
    model <- buildLDA(docTermMatrix(), input$numTopics)
    modelResult$model <- model
    
    return (model)
  })
  
  calclogPerplexity <- reactive({
    model <- ldaModel()
    modelResult$logPerplexity <- log(perplexity(model))
    return(modelResult$logPerplexity)
    })
  
  
  top_terms <- reactive({
    
    
    if (!is.null(modelResult$numTerms) && modelResult$numTerms != input$numTerms) {
      modelResult$hasChanged <- TRUE
    }
    
    
    top <- topNTerms(ldaModel(), input$numTerms)
    modelResult$topTerms <- top
    
    modelResult$numTerms <- input$numTerms
    return(top)
    })
  
  output$chooseTopic <- renderUI({
   inputDataSet <- inputDataSet() 
   ns <- session$ns
   
   numTopics <- input$numTopics
   
   modelResult$numTopics <- numTopics
   
   ranges <- c("1..10")
   if (numTopics > 10) {
     ranges <- sapply(seq(from=1,to=numTopics, by=10), 
            function(i) {
       
       start=i
       end=i+10-1
       
       paste0(start, "..", end)
     })
   }
   
   
   selectInput(inputId=ns("chooseTopic"),
                                     label="Select Topic Range",
                                     choices=ranges)
  })
  
  
  rangeSelection <- reactive({
    validate(need(loadFileResult$result, message=FALSE))
    validate(need(input$chooseTopic, message=FALSE))
    
    t <- input$chooseTopic
    
    range <- as.numeric(unlist(stringr::str_split(t, "\\.\\.")))
    return(range)
  })
  output$logPerplexity <- renderText({
    validate(need(modelResult$model, message=FALSE))
    ns <- session$ns
    calclogPerplexity()
  })
  
  output$topicPlot <- renderPlot({
    validate(need(input$chooseTopic, message=FALSE))
    withProgress(message="Building Model",
                 {
                   incProgress()
                   top_terms()
                   setProgress(1)
                 }
    )
    inputDataSet <- inputDataSet()
    selectedRange <- rangeSelection()
    top <- top_terms()
    range <- seq(from=selectedRange[1], to=selectedRange[2])
    plotNTermsInTopicRange(top, range)
  })
  
  output$downloadModel <- downloadHandler(
    filename=function() {
      folder <- paste0("lda_export_temp_",as.numeric(Sys.time()),".zip")
      folder
    },
    content=function(targetFile) {
    dataSet <- inputDataSet()  
    ldaModel <- modelResult$model
    textData <- modelResult$textData
    termMat <- modelResult$termMat
    metaData <- list(numTopics=modelResult$numTopics)
    
    exportLDAModel(targetFile, dataSet, ldaModel, textData, termMat, metaData)
  },
  contentType="application/zip")
  
  return (modelResult)
}