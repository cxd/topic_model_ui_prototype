

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
    validate(need(clusterResult$labelledDocs, message=FALSE))
    return(clusterResult$labelledDocs)
  })
  
  getNumTopics <- reactive({
    validate(need(topicModelResult$numTopics, message=FALSE))
    return(topicModelResult$numTopics)
  })
  
  classifyResult <- reactiveValues()
  
  observeEvent(input$classifyExample, {
    
    print(input$enterExample)
    
    validate(need(input$enterExample, message=FALSE))
    
    print("event received")
    
    example <- input$enterExample
    
    print(example)
    
    dataSet <- inputDataSet()
    
    textSet <- textDataSet()
    
    model <- ldaModel()
    
    numTopics <- getNumTopics()
    
    print(example)
   
    results <- classifyNewExamples(c(example), 
                                   labelledDocs(), 
                                   textSet, 
                                   model, 
                                   numTopics)
    
    topicLabels <- labelledTopics()
    
    ranked <- results$ranked
    ranked$topic <- as.numeric(ranked$topic)
    ranked <- inner_join(ranked, topicLabels, by="topic") %>%
      arrange(-probability)
    
    allTopicsRanked <- results$allTopicsRanked
    allTopicsRanked$topic <- as.numeric(allTopicsRanked$topic)
    allTopicsRanked <- inner_join(allTopicsRanked, topicLabels, by="topic") %>%
      arrange(-probability)
    
    classifyResult$ranked <- ranked
    classifyResult$allTopicsRanked <- allTopicsRanked
    })
  
  output$exampleOutput <- renderDataTable({
    validate(need(classifyResult$ranked, message=FALSE))
    classifyResult$allTopicsRanked
  })
  
  output$relatedDocs <- renderDataTable({
    validate(need(classifyResult$ranked, message=FALSE))
    data <- labelledDocs()
    r <- classifyResult$ranked$topic
    data <- data[data$topic == r,]
    data
  })
}