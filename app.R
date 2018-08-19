#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


source("components/load_file.R")
source("components/term_topics_ui.R")
source("components/term_ratios.R")
source("components/label_topics.R")
source("components/cluster_docs.R")


# allow large files
options(shiny.maxRequestSize=100*1024^2) 


ui <- fluidPage(
  titlePanel("Explore Topics"),
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Configure Data", load_file_ui("load")),
                tabPanel("Explore Topics", term_topics_ui("terms")),
                tabPanel("Compare Term Ratios", term_ratios_ui("termratios")),
                tabPanel("Label Topics", label_topics_ui("assignlabels")),
                tabPanel("Cluster Documents", cluster_docs_ui("cluster")),
                tabPanel("Interactive Cluster Classification"),
                tabPanel("Build Classifier"),
                tabPanel("Interactive Classification")
  )
),
theme="bootstrap.css")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  loadFileResult <- callModule(load_file_srv, "load")
  
  modelResult <- callModule(term_topics_observer, "terms", loadFileResult, "tabs", "Explore Topics")
  
  callModule(term_ratios_srv, "termratios", modelResult)
  
  labelledResult <- callModule(label_topics_srv,"assignlabels", modelResult)
  
  clusterResult <- callModule(cluster_docs_srv, "cluster", modelResult, labelledResult)
  
  reactive({
    print("test")
    print(loadFileResult)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

