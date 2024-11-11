shinyServer(function(input, output, session) {
  # Tab: Create Inquiry ----
  inquiry_template <- reactiveValues(
    title = "Survey",
    description = "Description of the survey.",
    questions = data.frame(
      question = character(),
      option = character(),
      input_type = character(),
      input_id = character(),
      dependence = character(),
      dependence_value = character(),
      required = logical()
    )
  )

  observe({
    if (nrow(inquiry_template$questions) > 0) {
      shinyjs::enable("downloadTemplate", asis = TRUE)
    } else {
      shinyjs::disable("downloadTemplate", asis = TRUE)
    }
  })

  observeEvent(input$create_df, {
    inquiry_template$title <- "Example Survey"
    inquiry_template$description <- "This survey was generated from an example DataFrame."
    inquiry_template$questions <- data.frame(
      question = c("What is your name?", "How do you feel today?"),
      option = c("", ""),
      input_type = c("text", "text"),
      input_id = c("name", "mood"),
      dependence = c(NA, NA),
      dependence_value = c(NA, NA),
      required = c(TRUE, FALSE)
    )
    # notify user that the data frame was created
    showNotification("An Inquiry Template has been created.", duration = 5)
  })

  # download template as json
  output$downloadTemplate <- downloadHandler(
    filename = function() { paste("inquiry_template", Sys.Date(), ".json", sep = "") },
    content = function(file) {
      # Convert to JSON
      inquiry_template_json <- jsonlite::toJSON(reactiveValuesToList(inquiry_template), pretty = TRUE, auto_unbox = TRUE)

      # Download the JSON file
      write(inquiry_template_json, file)
    }
  )

  inquiryTemplateServer("inquiry_template", inquiry_template = inquiry_template)

  # Tab: Respond to Inquiry ----
  output$survey_ui <- renderUI({
    shiny::validate(need(nrow(inquiry_template$questions) > 0, "Please create or load an Inquiry template first."))
    shiny::validate(need(input$renderSurvey, "Please load an Inquiry template first."))

    # Use a div container with a unique class to scope the survey styling
    div(class = "survey-container",
        # Survey output
        shinysurveys::surveyOutput(
          df = inquiry_template$questions,
          survey_title = inquiry_template$title,
          survey_description = inquiry_template$description,
          theme = NULL
        )
    )
  })


  # Handle the Submit button action
  survey_data <- reactiveVal()
  observeEvent(input$submit, {
    # Collect the responses from input list
    survey_data(data.frame(
      input_id = inquiry_template$questions$input_id,
      response = sapply(inquiry_template$questions$input_id, function(x) input[[x]])
    ))

    # notify user that the results were saved
    showNotification("Your responses have been saved.", duration = 5)
  })

  # Add a download button to trigger the download handler
  observe({
    if (!is.null(survey_data())) {
      shinyjs::enable("downloadData", asis = TRUE)
    } else {
      shinyjs::disable("downloadData", asis = TRUE)
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("survey_results", Sys.Date(), ".json", sep = "") },
    content = function(file) {
      # Convert to JSON
      survey_json <- jsonlite::toJSON(survey_data(), pretty = TRUE, auto_unbox = TRUE)

      # Download the JSON file
      write(survey_json, file)
    }
  )
})
