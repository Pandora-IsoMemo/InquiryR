library(shiny)
library(shinysurveys)
library(jsonlite)
library(InquiryR)

tagList(
  navbarPage(
    title = paste("InquiryR", packageVersion("InquiryR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      title = "Create Inquiry",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          actionButton("load_example", "Load Example"),
          tags$br(), tags$br(),
          selectInput("select_template", "Select Template", choices = c("No inquiry available ..." = "")),
          downloadButton("download_template_execute", "Download"),
          actionButton("remove_template", "Remove"),
        ),
        mainPanel(
          inquiryTemplateUI("inquiry_template"),
        ),
      )
    ),
    tabPanel(
      title = "Respond to Inquiry",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput("render_template", "Load Template", choices = c("No inquiry available ..." = "")),
          actionButton("render_template_execute", "Load"),
          tags$br(), tags$br(),
          downloadButton("download_response", "Download Response")
        ),
        mainPanel(
          uiOutput("survey_ui")
        ),
      )
    )
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/InquiryR/"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyjs::useShinyjs()
)
