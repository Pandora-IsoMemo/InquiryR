shinyServer(function(input, output, session) {
  # Tab: Create Inquiry ----
  inquiry_template <- empty_template()

  observeEvent(input$load_example, {
    # reset select_template input
    updateSelectInput(session, "load_template-select_template", selected = character(0))

    inquiry_template$title <- "Example Survey"
    inquiry_template$description <- "This survey was generated from an example DataFrame."

    inquiry_template$questions <-  shinyTools::shinyTryCatch(read.csv(file.path("data", "example_questions.csv")),
                                                             errorTitle = "Reading example file failed",
                                                             alertStyle = "shinyalert")

    # notify user that the data frame was created
    showNotification("An Inquiry Template has been loaded.", duration = 5)
  })

  submitted_templates <- inquiryTemplateServer("inquiry_template", init_template = inquiry_template)

  loaded_template <- loadInquiryServer("load_template", submitted_templates)

  observeEvent(loaded_template$title, {
    inquiry_template$title <- loaded_template$title
    inquiry_template$description <- loaded_template$description
    inquiry_template$questions <- loaded_template$questions
  })

  observe({
    # update download/render choices
    if (length(submitted_templates()) > 0) {
      choices <- names(submitted_templates())
    } else {
      choices <- c("No inquiry available ..." = "")
    }

    updateSelectInput(session, "select_template", choices = choices)
  })

  # download template as json
  observe({
    # enable/disable download button
    if (!is.null(input$select_template) &&
        input$select_template != "") {
      shinyjs::enable("download_template_execute", asis = TRUE)
      shinyjs::enable("remove_template", asis = TRUE)
    } else {
      shinyjs::disable("download_template_execute", asis = TRUE)
      shinyjs::disable("remove_template", asis = TRUE)
    }
  })

  downloadType <- reactive({
    if (isTRUE(is_encrypted(submitted_templates()[[input$select_template]]))) {
      "bin"
    } else {
      "json"
    }
  })

  output$download_template_execute <- downloadHandler(
    filename = function() {
      paste("inquiry_template_", Sys.Date(), ".", downloadType(), sep = "")
    },
    content = function(file) {
      selected_template <- submitted_templates()[[input$select_template]]

      if (is_encrypted(selected_template)) {
        # Write the encrypted template to a file
        zz <- file(file, "wb")
        writeBin(selected_template, zz)
        close(zz)
      } else {
        # Convert to JSON
        inquiry_template_json <- jsonlite::toJSON(
          sanitizeQuestionsForJson(submitted_templates()[[input$select_template]]),
          pretty = TRUE,
          auto_unbox = TRUE
        )

        # Download the JSON file
        write(inquiry_template_json, file)
      }
    }
  )

  observeEvent(input$remove_template, {
    new_templates <- submitted_templates()

    new_templates <- new_templates[!(input$select_template == names(new_templates))]
    submitted_templates(new_templates)
  })

  # Tab: Respond to Inquiry ----
  loaded_inquiry <- loadInquiryServer("load_inquiry", submitted_templates)

  # Initialize a reactive flag
  survey_ui_created <- reactiveVal(FALSE)
  output$survey_ui <- renderUI({
    shiny::validate(need(
      nrow(loaded_inquiry$questions) > 0,
      "Please load an inquiry first."
    ))

    survey_ui_created(TRUE)
    # Use a div container with a unique class to scope the survey styling
    div(
      class = "survey-container",
      # Survey output
      shinyTools::shinyTryCatch(
        shinysurveys::surveyOutput(
          df = sanitizeQuestions(loaded_inquiry$questions),
          survey_title = loaded_inquiry$title,
          survey_description = loaded_inquiry$description,
          #theme = NULL # <- BUG: dependencies do not work if theme is NULL
          theme = rgb(0, 0, 0, 0) # <- HACK: use transparent theme to avoid theme issues, because surveyOutput overwrites the style
        ),
        errorTitle = "Loading the Inquiry failed",
        alertStyle = "shinyalert"
      )
    )
  })

  observe({
    req(isTRUE(survey_ui_created()))
    shinyTools::shinyTryCatch(shinysurveys::renderSurvey(),
                              errorTitle = "Rendering the Inquiry failed",
                              alertStyle = "shinyalert")
  })

  # Handle the Submit button action
  survey_data <- reactiveVal()
  observeEvent(input$submit, {
    # Aggregate responses with getSurveyData()
    survey_data(shinysurveys::getSurveyData())

    # notify user that the results were saved
    showNotification("Your responses have been saved.", duration = 5)
  })

  # Add a download button to trigger the download handler
  observe({
    if (!is.null(survey_data())) {
      shinyjs::enable("download_response", asis = TRUE)
    } else {
      shinyjs::disable("download_response", asis = TRUE)
    }
  })

  output$download_response <- downloadHandler(
    filename = function() {
      paste("survey_results", Sys.Date(), ".json", sep = "")
    },
    content = function(file) {
      # Convert to JSON
      survey_json <- jsonlite::toJSON(survey_data(), pretty = TRUE, auto_unbox = TRUE)

      # Download the JSON file
      write(survey_json, file)
    }
  )
})
