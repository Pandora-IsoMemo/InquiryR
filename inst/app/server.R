shinyServer(function(input, output, session) {
  # Tab: Create Inquiry ----
  # Placeholder for DataFrame creation
  df <- reactiveVal(NULL)

  observe({
    if (!is.null(df())) {
      shinyjs::enable("downloadTemplate", asis = TRUE)
    } else {
      shinyjs::disable("downloadTemplate", asis = TRUE)
    }
  })

  observeEvent(input$create_df, {
    df(data.frame(
      question = c("What is your name?", "How do you feel today?"),
      option = c("", ""),
      input_type = c("text", "text"),
      input_id = c("name", "mood"),
      dependence = c(NA, NA),
      dependence_value = c(NA, NA),
      required = c(TRUE, FALSE)
    ))
    # notify user that the data frame was created
    showNotification("An Inquiry Template has been created.", duration = 5)
  })

  # download template as json
  output$downloadTemplate <- downloadHandler(
    filename = function() { paste("inquiry_template", Sys.Date(), ".json", sep = "") },
    content = function(file) {
      # Convert to JSON
      df_json <- jsonlite::toJSON(df(), pretty = TRUE, auto_unbox = TRUE)

      # Download the JSON file
      write(df_json, file)
    }
  )

  # Display the DataFrame in the table output
  output$df_table <- renderTable({
    shiny::validate(need(df(), "Please create an Inquiry template first."))
    df()
  })

  # Tab: Respond to Inquiry ----
  # Placeholder for survey responses
  survey_data <- reactiveVal()

  observe({
    if (!is.null(survey_data())) {
      shinyjs::enable("downloadData", asis = TRUE)
    } else {
      shinyjs::disable("downloadData", asis = TRUE)
    }
  })

  # Handle the Submit button action
  observeEvent(input$submit, {
    # Collect the responses from input list
    survey_data(data.frame(
      input_id = df()$input_id,
      response = sapply(df()$input_id, function(x) input[[x]])
    ))

    # notify user that the results were saved
    showNotification("Your responses have been saved.", duration = 5)
  })

  # Add a download button to trigger the download handler
  output$survey_ui <- renderUI({
    shiny::validate(need(df(), "Please create or load an Inquiry template first."))
    shiny::validate(need(input$renderSurvey, "Please load an Inquiry template first."))
    fluidPage(
      # Survey output
      shinysurveys::surveyOutput(
        df = df(),
        survey_title = "Custom Survey",
        survey_description = "This survey was generated from a custom DataFrame."
      )
    )
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
