require(DT)
## https://github.com/rstudio/DT/pull/480


label_topics_ui <- function(id) {
  ns <- NS(id)
  verticalLayout(
    p("Enter labels summarising each of the topics in the table below."),
    DTOutput(ns("topicTable")),
    
    inputPanel(
      actionButton(ns("labelBtn"), "Save Labels"),
      
      downloadButton(ns("exportBtn"), "Export Labels")
    ),
    
    inputPanel(
      
      
      fileInput(inputId=ns("sourceLabelFile"),
                label="Or Load Previous Labels",
                multiple=FALSE,
                accept=c(
                  "application/csv",
                  "csv")),
      
      actionButton(ns("loadLabelBtn"), "Load Selected File")
    )
  ) 
}

label_topics_srv <- function(input, output, session, topicModelResult=list()) {
  
  ns <- session$ns
  
  labelledResult <- reactiveValues()
  
  getNumTopics <- reactive({
    validate(need(topicModelResult$numTopics, message=FALSE))
    return(topicModelResult$numTopics)
  })
  
  getNumTerms <- reactive({
    validate(need(topicModelResult$numTerms, message=FALSE))
    return(topicModelResult$numTerms)
  })
  
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
  
  getClusteredDocs <- reactive({
    dataSet <- inputDataSet()
    model <- ldaModel()
    docMat <- termDocMat()
    numTopics <- getNumTopics()
    topics <- data.frame(topic=1:numTopics,
                     label=rep("unknown", numTopics),
                     stringsAsFactors = FALSE)
    labelledResult$clusteredDocs <- clusterDocuments(dataSet, model, docMat, topics) 
    labelledResult$clusteredDocs
    })
  
  getSuggestions <- reactive({
    numTopics <- getNumTopics()
    topics <- data.frame(topic=1:numTopics,
                     label=rep("unknown", numTopics),
                     stringsAsFactors = FALSE)
    suggestions <- suggestLabels(ldaModel(), getNumTerms(), getClusteredDocs(), topics)
    suggestions
  })
  
  output$topicTable <- renderDT({
    numTopics <- getNumTopics()
    df <- data.frame()
    if (!is.null(labelledResult$labelledTopics) && numTopics == nrow(labelledResult$labelledTopics)) {
      df <- labelledResult$labelledTopics
    } else {
      labelledResult$labelledTopics <- getSuggestions()
      df <- labelledResult$labelledTopics
    }
    df
  }, selection="none", 
  editable=TRUE,
  rownames=FALSE)
  
  observeEvent(input$labelBtn, {
    print(labelledResult$labelledTopics) 
  })
  
  proxy <- dataTableProxy(ns("topicTable"))
  
  observeEvent(input$topicTable_cell_edit, {
      
      info <- input$topicTable_cell_edit
      i <- info$row
      j <- info$col+1
      v <- info$value
      
      print(info)
      
      if (!is.null(labelledResult$labelledTopics)) {
        tmpDf <- labelledResult$labelledTopics
        tmpDf[i,j] <- v
        print(tmpDf)
        labelledResult$labelledTopics <- tmpDf
        replaceData(proxy, tmpDf, resetPaging=FALSE, rownames=FALSE)
      }
     
  })
  
  output$exportBtn <- downloadHandler(
    filename=function() {
      folder <- paste0("labels_export_",as.numeric(Sys.time()),".csv")
      folder
    },
    content=function(targetFile) {
      labels <- labelledResult$labelledTopics
      
      write.csv(labels,targetFile, row.names=FALSE)
    },
    contentType="application/csv")
  
  observeEvent(input$loadLabelBtn, {
    validate(need(input$sourceLabelFile, message=FALSE))
    temp <- read.csv(input$sourceLabelFile$datapath, header=TRUE)
    numTopics <- getNumTopics()
    if (nrow(temp) == numTopics) {
      labelledResult$labelledTopics <- temp
      replaceData(proxy, temp, resetPaging=FALSE, rownames=FALSE)
    }
  })
  
  return(labelledResult)
  
}