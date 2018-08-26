

cluster_docs_ui <- function(id) {
  ns <- NS(id)
  
  verticalLayout(
    inputPanel(
      uiOutput(ns("chooseDisplayTopic"))
    ),
    dataTableOutput(ns("labelledDocuments")),
    plotOutput(ns("clusterSizes"), width="800px", height="800px"),
    inputPanel(
      
      actionButton(ns("saveClusters"), "Save Clusters"),
      downloadButton(ns("exportData"), "Export Clusters")
    )
  )
  
}

cluster_docs_srv <- function(input, output, session, topicModelResult=list(), labelledResult=list()) {
  
  clusterResults <- reactiveValues()
  
  inputDataSet <- reactive({
    validate(need(topicModelResult$dataSet, message=FALSE))
    return(topicModelResult$dataSet)
  })
  
  ldaModel <- reactive({
    validate(need(topicModelResult$model, message=FALSE))
    return(topicModelResult$model)
  })
  
  termDocMat <- reactive({
    validate(need(topicModelResult$termMat, message=FALSE))
    return(topicModelResult$termMat)
  })

  getNumTopics <- reactive({
    validate(need(topicModelResult$numTopics, message=FALSE))
    return(topicModelResult$numTopics)
  })
  
  labelledTopics <- reactive({
    validate(need(labelledResult$labelledTopics, message=FALSE))
    return(labelledResult$labelledTopics)
  })
  
  clusteredDocs <- reactive({
    validate(need(topicModelResult$model, message=FALSE))
    dataSet <- inputDataSet()
    model <- ldaModel()
    docMat <- termDocMat()
    topics <- labelledTopics()
    clusterResults$labelledDocs <- clusterDocuments(dataSet, model, docMat, topics) 
    clusterResults$labelledDocs
  })
  
  docsForDisplay <- reactive({
    validate(need(topicModelResult$model, message=FALSE))
    validate(need(input$chooseDisplayTopic, message=FALSE))
    data <- clusteredDocs()
    if (input$chooseDisplayTopic == "ALL") {
      data 
    } else {
      t <- as.numeric(input$chooseDisplayTopic)
      subset <- data[data$topic == t,]
      subset
    }
    })
  
  output$chooseDisplayTopic <- renderUI({
    ns <- session$ns
    n <- getNumTopics()
    options <- c("ALL", as.character(seq(from=1,to=n)))
    selectInput(ns("chooseDisplayTopic"),
                "Filter documents by topic",
                options)
  })
  
  output$labelledDocuments <- renderDataTable({
    validate(need(topicModelResult$model, message=FALSE))
    docsForDisplay()
    },
    options=list(
      pageLength=10
    ))
  
  output$clusterSizes <- renderPlot({
    validate(need(clusterResults$labelledDocs, message=FALSE))
    boxPlotClusterSizes(clusterResults$labelledDocs)
  })
  
  # Downloadable csv of selected dataset ----
  output$exportData <- downloadHandler(
    filename = function() {
      topics <- labelledTopics()
      paste0("export_",nrow(topics),"_clusters.csv")
    },
    content = function(file) {
      validate(need(clusterResults$labelledDocs, message=FALSE))
      write.csv(clusterResults$labelledDocs, file, row.names = FALSE)
    }
  )
  
  
  return(clusterResults)
}
