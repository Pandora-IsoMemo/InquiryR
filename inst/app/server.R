shinyServer(function(input, output, session) {
  # Tab: Create Inquiry ----
  loaded_template <- empty_template()

  imported_template <- DataTools::importServer(
    "import_template",
    title = "",
    ckanFileTypes = "json",
    importType = "list",
    fileExtension = "json",
    options = DataTools::importOptions(rPackageName = "InquiryR")

  )

  observeEvent(imported_template(), {
    req(length(imported_template()) > 0)

    req(isTRUE(validateImport(imported_template()[[1]])))

    # missingEntries <- setdiff(c("title", "description", "questions"),
    #                           names(imported_template()[[1]]))
    # if (length(missingEntries) > 0) {
    #   showNotification(
    #     paste(
    #       "The imported template is missing the following entries: ",
    #       paste(missingEntries, collapse = ", ")
    #     ),
    #     duration = 5
    #   )
    # }

    loaded_template$title <- imported_template()[[1]]$title
    loaded_template$description <- imported_template()[[1]]$description
    loaded_template$questions <- imported_template()[[1]]$questions %>% sanitizeQuestions()
    # notify user that the data frame was created
    showNotification("An Inquiry Template has been loaded.", duration = 5)
  })

  observeEvent(input$load_example, {
    loaded_template$title <- "Example Survey"
    loaded_template$description <- "This survey was generated from an example DataFrame."

    loaded_template$questions <- file.path("data", "example_questions.csv") %>%
      read.csv() %>%
      shinyTools::shinyTryCatch(errorTitle = "Reading example file failed", alertStyle = "shinyalert")

    # notify user that the data frame was created
    showNotification("An Inquiry Template has been loaded.", duration = 5)
  })

  submitted_templates <- inquiryTemplateServer("inquiry_template", init_template = loaded_template)

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
        writeBin(selected_template, file)
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
  inquiry_template <- loadInquiryServer("load_template", submitted_templates)

  # Initialize a reactive flag
  survey_ui_created <- reactiveVal(FALSE)
  output$survey_ui <- renderUI({
    shiny::validate(need(
      nrow(inquiry_template$questions) > 0,
      "Please load an inquiry first."
    ))

    survey_ui_created(TRUE)
    # Use a div container with a unique class to scope the survey styling
    div(
      class = "survey-container",
      # Survey output
      shinysurveys::surveyOutput(
        df = inquiry_template$questions,
        survey_title = inquiry_template$title,
        survey_description = inquiry_template$description,
        #theme = NULL # <- BUG: dependencies do not work if theme is NULL
        theme = rgb(0, 0, 0, 0) # <- HACK: use transparent theme to avoid theme issues, because surveyOutput overwrites the style
      ) %>%
        shinyTools::shinyTryCatch(errorTitle = "Loading the Inquiry failed", alertStyle = "shinyalert")
    )
  })

  observe({
    req(isTRUE(survey_ui_created()))
    shinysurveys::renderSurvey() %>%
      shinyTools::shinyTryCatch(errorTitle = "Rendering the Inquiry failed", alertStyle = "shinyalert")
  })

  # Handle the Submit button action
  survey_data <- reactiveVal()
  observeEvent(input$submit, {
    # Collect the responses from input list
    survey_data(data.frame(
      input_id = inquiry_template$questions$input_id,
      response = sapply(inquiry_template$questions$input_id, function(x)
        input[[x]])
    ))

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
