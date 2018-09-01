
term_ratios_ui <- function(id) {
  ns <- NS(id)
  
  verticalLayout(
    inputPanel(
      uiOutput(ns("chooseTopic1")),
      
      uiOutput(ns("chooseTopicRange"))
    ),
    plotOutput(ns("topicRatioPlot"), width="900px", height="1024px")
  )
  
}


term_ratios_srv <- function(input, output, session, topicModelResult=list()) {
  
  
  
  
  getNumTopics <- reactive({
    
    print(paste("numTopics", topicModelResult$numTopics))
    print(paste("numTerms", topicModelResult$numTerms))
    
    validate(need(topicModelResult$numTopics, message=FALSE))
    return(topicModelResult$numTopics)
  })
  
  getNumTerms <- reactive({
    validate(need(topicModelResult$numTerms, message=FALSE))
    return(topicModelResult$numTerms)
  })
  
  getModel <- reactive({
    validate(need(topicModelResult$model, message=FALSE))
    return(topicModelResult$model)
  })
  output$chooseTopic1 <- renderUI({
    numTopics <- getNumTopics()
    ns <- session$ns
    selectInput(inputId=ns("chooseTopic1"),
                label="Select Start Topic",
                choices=1:numTopics)
  })
  output$chooseTopicRange <- renderUI({
    numTopics <- getNumTopics()
    ns <- session$ns
    
    ranges <- c("1..10")
    if (numTopics > 10) {
      ranges <- sapply(seq(from=1,to=numTopics, by=10), 
                       function(i) {
                         
                         start=i
                         end=i+10-1
                         
                         paste0(start, "..", end)
                       })
    }
    
    selectInput(inputId=ns("chooseTopicRange"),
                label="Select Topic Range",
                choices=ranges)
  })
  
  rangeSelection <- reactive({
    validate(need(input$chooseTopicRange, message=FALSE))
    
    t <- input$chooseTopicRange
    
    range <- as.numeric(unlist(stringr::str_split(t, "\\.\\.")))
    return(range)
  })
  
  output$topicRatioPlot <- renderPlot({
    withProgress(message="Calculate Log Ratio",
                 {
                   incProgress()
                   
                   setProgress(1)
                 }
    )
    selectedRange <- rangeSelection()
    selectedRange <- seq(from=selectedRange[1], to=selectedRange[2])
    print(selectedRange)
    print(input$chooseTopic1)
    
    plotNTermsLogRatioInRange(getModel(), 
                              as.numeric(input$chooseTopic1), 
                              getNumTerms(), 
                              getNumTopics(), 
                              selectedRange)
  })
}