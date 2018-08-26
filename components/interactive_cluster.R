

example_cluster_docs_ui <- function(id) {
  ns <- NS(id)
  
  verticalLayout(
    inputPanel(
      textInput(ns("enterExample"), "Example Text"),     
      actionButton(ns("classifyExample"), "Classify Example")
    ),
    h2("Nearest Topics"),
    dataTableOutput(ns("exampleOutput")),
    h2("Documents from Closest Topic"),
    dataTableOutput(ns("relatedDocs"))
  )
  
}

example_cluster_docs_srv <- function(input, output, session, topicModelResult=list(), labelledResult=list(), clusterResult=list()) {
  
  inputDataSet <- reactive({
    validate(need(topicModelResult$dataSet, message=FALSE))
    return(topicModelResult$dataSet)
  })
  
  textDataSet <- reactive({
    validate(need(topicModelResult$textData, message=FALSE))
    return(topicModelResult$textData)
  })
  
  ldaModel <- reactive({
    validate(need(topicModelResult$model, message=FALSE))
    return(topicModelResult$model)
  })
  
  termDocMat <- reactive({
    validate(need(topicModelResult$termMat, message=FALSE))
    return(topicModelResult$termMat)
  })
  
  labelledTopics <- reactive({
    validate(need(labelledResult$labelledTopics, message=FALSE))
    return(labelledResult$labelledTopics)
  })
  
  labelledDocs <- reactive({
    validate(clusterResult$labelledDocs, message=FALSE)
    return(clusterResult$labelledDocs)
  })
  
  getNumTopics <- reactive({
    validate(need(topicModelResult$numTopics, message=FALSE))
    return(topicModelResult$numTopics)
  })
  
  classifyResult <- reactiveValues()
  
  observeEvent(input$classifyExample, {
    validate(input$enterExample, message=FALSE)
    
    print("event received")
    
    example <- input$enterExample
    
    print(example)
    
    dataSet <- inputDataSet()
    
    textSet <- textDataSet()
    
    model <- ldaModel()
    
    numTopics <- getNumTopics()
    
    print(example)
   
    results <- classifyNewExamples(c(example), 
                                   labelledDocs, 
                                   textSet, 
                                   model, 
                                   numTopics)
    
    classifyResult$ranked <- results$ranked
    classifyResult$allTopicsRanked <- results$allTopicsRanked
    })
  
  output$exampleOutput <- renderDataTable({
    validate(classifyResult$ranked, message=FALSE)
    classifyResult$ranked
  })
}