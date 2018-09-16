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
source("components/eval_perplexity.R")
source("components/term_topics_ui.R")
source("components/term_ratios.R")
source("components/label_topics.R")
source("components/cluster_docs.R")
source("components/interactive_cluster.R")
source("components/train_dnn_model.R")

# allow large files
options(shiny.maxRequestSize=100*1024^2) 


ui <- fluidPage(
  titlePanel("Explore Topics"),
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Configure Data", load_file_ui("load")),
                tabPanel("Assess Number of Topics", eval_perplexity_ui("evalp")),
                tabPanel("Explore Topics", term_topics_ui("terms")),
                tabPanel("Compare Term Ratios", term_ratios_ui("termratios")),
                tabPanel("Label Topics", label_topics_ui("assignlabels")),
                tabPanel("Cluster Documents", cluster_docs_ui("cluster")),
                tabPanel("Interactive Cluster Classification",
                         example_cluster_docs_ui("examplecluster")),
                tabPanel("Build Classifier",
                         train_model_ui("trainDNN")),
                tabPanel("Interactive Classification")
  )
),
theme="bootstrap.css")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  loadFileResult <- callModule(load_file_srv, "load")
  
  callModule(eval_perplexity_srv, "evalp", loadFileResult)
  
  modelResult <- callModule(term_topics_observer, "terms", loadFileResult, "tabs", "Explore Topics")
  
  callModule(term_ratios_srv, "termratios", modelResult)
  
  labelledResult <- callModule(label_topics_srv,"assignlabels", modelResult)
  
  clusterResult <- callModule(cluster_docs_srv, "cluster", modelResult, labelledResult)
  
  callModule(example_cluster_docs_srv, "examplecluster", modelResult, labelledResult, clusterResult)
  
  trainResult <- callModule(train_model_srv, "trainDNN", loadFileResult, modelResult, labelledResult, clusterResult)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

