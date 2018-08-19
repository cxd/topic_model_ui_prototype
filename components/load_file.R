library(shiny)

source("components/term_topics_ui.R")

load_file_ui <- function(id) {
  ns <- NS(id)
  verticalLayout(
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
    actionButton(ns("readInDataBtn"), "Update Data"),
    fluid=TRUE
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
  
  return (loadFileResult)
}