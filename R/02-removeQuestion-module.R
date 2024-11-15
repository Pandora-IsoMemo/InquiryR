removeQuestionUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      5,
      pickerInput(
        ns("remove_questions"),
        "Remove Question(s)",
        choices = c("Please add questions first ..." = ""),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} options selected"
        ),
        width = "100%"
      )
    ),
    column(
      5,
      pickerInput(
        ns("remove_options"),
        "Remove Option(s)",
        choices = c("Please select question(s) first ..." = ""),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} options selected"
        ),
        width = "100%"
      )
    ),
    column(2, style = "margin-top: 1.5em;", actionButton(ns("remove"), "Remove", width = "100%"))
  ))
}

removeQuestionServer <- function(id, questions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    new_questions <- reactiveVal(NULL)

    observe({
      logDebug("%s: Update 'input$remove_questions' choices.", id)
      # update input$remove_questions
      choices_df <- questions()[, c("input_id", "question")] %>%
        distinct()
      choices <- choices_df$input_id
      names(choices) <- choices_df$question
      updatePickerInput(session, "remove_questions", choices = choices)
    }) %>%
      bindEvent(questions(),
                ignoreInit = TRUE,
                ignoreNULL = FALSE)

    # enable/disable 'Remove' button
    observe({
      logDebug("%s: Enable/Disable 'Remove' button.", id)
      if (length(input$remove_questions) > 0) {
        choices <- questions()$option[questions()$input_id %in% input$remove_questions] %>%
          unique()
        updatePickerInput(session,
                          "remove_options",
                          choices = choices,
                          selected = choices)
        shinyjs::enable(ns("remove"), asis = TRUE)
      } else {
        updatePickerInput(
          session,
          "remove_options",
          choices = c("Please select question(s) first ..." = "")
        )
        shinyjs::disable(ns("remove"), asis = TRUE)
      }
    })

    # remove selected questions
    observe({
      logDebug("%s: Remove selected questions.", id)
      remove_index <- (questions()$input_id %in% input$remove_questions) &
        (questions()$option %in% input$remove_options)

      new_questions(questions()[!remove_index, ])
    }) %>%
      bindEvent(input$remove)

    return(new_questions)
  })
}
