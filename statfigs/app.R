#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(TADA)
library(shinybusy)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    shiny::titlePanel("TADA Stat Figs"),
    shiny::fluidRow(column(4,
      shiny::fluidRow(shiny::selectInput("whatdata","What Data Would You Like To Use?", choices = c("Shepherdstown", "Tribal", "Random"))),
      shiny::fluidRow(shiny::actionButton("godata","Get Dataset"))),
      column(4,
        shiny::fluidRow(shiny::uiOutput("selcols")),
        shiny::fluidRow(shiny::uiOutput("lockit"))
      )),
    htmltools::br(),
    shiny::fluidRow(column(3, shiny::uiOutput("univals"))),
    shiny::fluidRow(column(6, plotly::plotlyOutput("boxplot")),
                    column(6, plotly::plotlyOutput("histogram")))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  reacts = shiny::reactiveValues()
  
  shiny::observeEvent(input$godata,{
    # a modal that pops up showing it's working on querying the portal
    shinybusy::show_modal_spinner(
      spin = "double-bounce",
      color = "#0071bc",
      text = "Grabbing your dataset...",
      session = shiny::getDefaultReactiveDomain()
    )
    if(input$whatdata=="Shepherdstown"){
      reacts$dat = TADA::Data_NCTCShepherdstown_HUC12
    }
    if(input$whatdata=="Tribal"){
      reacts$dat = TADA::Data_6Tribes_5y
    }
    if(input$whatdata=="Random"){
      load(system.file("extdata", "statecodes_df.Rdata", package = "TADA"))
      state = sample(statecodes_df$STUSAB,1)
      number_of_days = 90
      ten_yrs_ago = Sys.Date()-10*365
      random_start_date = ten_yrs_ago + sample(10*365,1)
      end_date = random_start_date + number_of_days
      
      dat = TADA_DataRetrieval(startDate = as.character(random_start_date), endDate = as.character(end_date), statecode = state, sampleMedia = "Water")
      
      if(dim(dat)[1]<1){
        dat = Data_NCTCShepherdstown_HUC12
      }
      reacts$dat = dat
    }
    reacts$names = names(reacts$dat)
    shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
  })
  
  output$selcols = shiny::renderUI({
    shiny::req(reacts$names)
    shiny::selectInput("selcols", "Select Grouping Columns",choices = reacts$names, selected = "TADA.ComparableDataIdentifier", multiple = TRUE)
  })
  
  output$lockit = shiny::renderUI({
    shiny::req(reacts$dat)
    shiny::actionButton("lockit","Lock groups in")
  })
  
  shiny::observeEvent(input$lockit,{
    reacts$dat = reacts$dat %>% unite("group", input$selcols, sep = "_", remove = FALSE)
    reacts$done = "Y"

  })
  
  output$univals = shiny::renderUI({
    shiny::req(reacts$done)
    choices = unique(reacts$dat$group)
    shiny::selectInput("univals", "Pick Group", choices = choices)
  })
  
  shiny::observeEvent(input$univals,{
    plotd = subset(reacts$dat, reacts$dat$group == input$univals)
    reacts$boxp = TADA_Boxplot(plotd, id_cols = input$selcols)
    reacts$hist = TADA_Histogram(plotd, id_cols = input$selcols)
  })
  
  output$boxplot = plotly::renderPlotly({
    req(reacts$boxp)
    reacts$boxp
  })
  
  output$histogram = plotly::renderPlotly({
    req(reacts$hist)
    reacts$hist %>%
      plotly::config(displayModeBar = FALSE)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
