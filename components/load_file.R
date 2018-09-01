library(shiny)

source("components/term_topics_ui.R")

load_file_ui <- function(id) {
  ns <- NS(id)
  verticalLayout(
    inputPanel(
      fileInput(inputId=ns("sourceFile"),
                label="Input Document CSV",
                multiple=FALSE,
                accept=c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput(inputId=ns("textColSel"),
                  label="Text Column Name",
                  choices=c()),
      selectInput(inputId=ns("labelColSel"),
                  label="Tag Label Column Name",
                  choices=c()),
      checkboxInput(inputId=ns("hasLabels"),
                    label="CSV Has Class Labels Column"),
      selectInput(inputId=ns("docIdName"),
                  label="Doc Id Column Name",
                  choices=c()),
      checkboxInput(inputId=ns("hasDocId"),
                    label="Use Doc Id Column"),
      actionButton(ns("readInDataBtn"), "Update Data")  
    ),
    inputPanel(
      fileInput(inputId=ns("sourceZipFile"),
                label="Or Load Previous Model from zip file",
                multiple=FALSE,
                accept=c(
                  "application/zip",
                  "zip")),
      actionButton(ns("readZip"), "Load Zip File")  
    )
    
  )
}

load_file_srv <- function(input, output, session) {
  
  inputData <- reactive({
    
    validate(need(input$sourceFile, message=FALSE))
    
    data <- read.csv(input$sourceFile$datapath, header=TRUE)
    
    choiceLabels <- colnames(data)
    
    inputData <- list()
    inputData$data <- data
    inputData$choiceLabels <- choiceLabels
    
    return(inputData)
  })
  
  observe( { 
    
    choiceLabels <- inputData()$choiceLabels
  
    ns <- session$ns
    
    updateSelectInput(session, inputId="textColSel",
                choices=choiceLabels)
    updateSelectInput(session, inputId="labelColSel",
                label="Tag Label Column Name",
                choices=choiceLabels)
    updateSelectInput(session, inputId="docIdName",
                label="Doc Id Column Name",
                choices=choiceLabels)
  })
  
  loadFileResult <- reactiveValues(result=list())
  
  observeEvent(input$readInDataBtn, {
    validate(need(input$sourceFile, message="Cannot get file"))
    
    
    loadFileResult$result$textColName <- input$textColSel
    loadFileResult$result$labelColName <- input$labelColSel
    loadFileResult$result$hasClassLabels <- input$hasLabels
    loadFileResult$result$docIdName <- input$docIdName
    loadFileResult$result$hasDocIdCol <- input$hasDocId
    loadFileResult$result$srcFile <- input$sourceFile$datapath
    
    #print(loadFileResult$result)
    })
  
  observeEvent(input$readZip, {
    validate(need(input$sourceZipFile, message="Cannot get zip file"))
    
    zipSrc <- input$sourceZipFile$datapath
    
    print(zipSrc)
    
    if (file.exists(zipSrc)) {
      readResult <- importLDAModel(zipSrc)
      loadFileResult$result$ldaModel <- readResult$ldaModel
      loadFileResult$result$dataSet <- readResult$dataSet
      loadFileResult$result$metaData <- readResult$metaData
      loadFileResult$result$textData <- readResult$textData
      loadFileResult$result$termMat <- readResult$termMat
    }
    
  })
  
  return (loadFileResult)
}