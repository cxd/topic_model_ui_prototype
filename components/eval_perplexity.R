

eval_perplexity_ui <- function(id) {
  
  ns <- NS(id)
  
  verticalLayout(
    h5("Assess Model Perplexity"),
    p("Enter the minimum and maximum number of topics and the step size."),
    p("For each step size the process will then build an LDA model and provide a report on the perplexity of the model."),
    p("Models with lower perplexity are better."),
    p("Running this process will take a while, especially on large numbers of topics and large document collections. This is an optional but recommended step."),
    inputPanel(
      numericInput(ns("startNumTopics"), 
                   "Min Number of Topics.", 
                   value=10, min=2),
      
      numericInput(ns("endNumTopics"), 
                   "Max Number of Topics.", 
                   value=20, min=3),
      
      
      numericInput(ns("stepSize"), 
                   "Step Size", 
                   value=10, min=2)
    ),
    h5("Evaluation Result"),
    plotOutput(ns("logPerplexityPlot"), width="800px", height="600px"),
    dataTableOutput(ns("logPerlexityTable"))
    
  )
}

eval_perplexity_srv <- function(input, output, session, loadFileResult=list()) {
  evalPerplexity <- reactiveValues()
  modelResult <- reactiveValues()
  
  inputDataSet <- reactive({
    validate(need(loadFileResult$result, message=FALSE))
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
  
  evalPerplexityData <- reactive({
    docTermMat <- docTermMatrix()
    startN <- input$startNumTopics
    endN <- input$endNumTopics
    step <- input$stepSize
    print(paste("Start",startN,"End",endN,"Step",step))
    range <- seq(from=startN, to=endN, by=step)
    pData <- assessModelPerplexity(docTermMat, range)
    evalPerplexity$perplexityData <- pData
    return(pData)
  })
  
  output$logPerplexityPlot <- renderPlot({
    startN <- input$startNumTopics
    endN <- input$endNumTopics
    step <- input$stepSize
    range <- seq(from=startN, to=endN, by=step)
    totalSteps <- length(range)
    withProgress(message=paste("Running Evaluation of", totalSteps, "steps"),
                 {
                   incProgress()
                   evalPerplexityData()
                   setProgress(1)
                 }
    )
    pData <- evalPerplexityData()
    plotPerplexity(pData)
  })
  output$logPerlexityTable <- renderDataTable({
    pData <- evalPerplexityData()
    data.frame(topicCount=pData$range,
               perplexity=pData$perplexity,
               logPerplexity=pData$logPerplexity)
  },
  options=list(
    pageLength=50
  ))
  
  return (evalPerplexity)
  }
  