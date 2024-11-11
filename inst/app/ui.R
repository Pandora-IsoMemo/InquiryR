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
          actionButton("create_df", "Load Example"),
          tags$br(), tags$br(),
          downloadButton("downloadTemplate", "Download")
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
          actionButton("renderSurvey", "Load Template"),
          tags$br(), tags$br(),
          downloadButton("downloadData", "Download")
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
