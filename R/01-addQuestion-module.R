addQuestionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(
      10,
      textInput(
        ns("question"),
        "Question*",
        placeholder = "Enter question ...",
        width = "100%"
      ),
    ), column(
      2, textInput(ns("question_id"), "Input ID*", value = createRandomID())
    )),
    uiOutput(ns("help_question")),
    fluidRow(
      column(3, selectInput(
        ns("input_type"),
        "Input Type*",
        c(
          "Text" = "text",
          "Numeric" = "numeric",
          "Multiple Choice" = "mc",
          "Dropdown Select" = "select",
          "Yes/No question" = "y/n",
          "Matrix (horzizontal options)" = "matrix"
        )
      )),
      column(
        3,
        style = "margin-top: 1.5em;",
        checkboxInput(
          ns("required"),
          "Is the question required (not optional)?",
          width = "100%"
        )
      ),
      column(
        6,
        style = "margin-top: 1.5em;",
        checkboxInput(
          ns("dependence"),
          "Does this question depend on another question?",
          width = "100%"
        )
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.dependence",
      selectInput(
        ns("dependence_questions"),
        "Does this question depend on another question?",
        choices = c("Please add questions first ..." = ""),
        width = "100%"
      ),
      textInput(
        ns("dependence_value"),
        "Value that the dependence question must take for this question to be shown",
        width = "100%"
      )
    ),
    fluidRow(
      column(
        10,
        textInput(
          ns("option"),
          label = "Placeholder",
          placeholder = "Enter placeholder value ...",
          value = "Your Answer",
          width = "100%"
        ),
        uiOutput(ns("help_option"))
      ),
      column(
        2,
        style = "margin-top: 1.5em;",
        actionButton(ns("add"), "Add", width = "100%")
      )
    )
  )
}

addQuestionServer <- function(id, questions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    new_question <- reactiveVal(NULL)

    # enable/disable 'Add' button
    observe({
      logDebug("%s: Enable/Disable 'Add' button.", id)
      if (!is.null(input$question) && input$question != "" &&
          !is.null(input$question_id) && input$question_id != "") {
        shinyjs::enable(ns("add"), asis = TRUE)
      } else {
        shinyjs::disable(ns("add"), asis = TRUE)
      }
    })

    help_question <- reactiveVal(NULL)
    help_option <- reactiveVal(NULL)

    output$help_question <- renderUI({
      helpText(help_question())
    })
    output$help_option <- renderUI({
      helpText(help_option())
    })

    observe({
      label <- switch(input$input_type,
                        "mc" = "Possible answers*",
                        "select" = "Possible answers*",
                        "text" = "Placeholder",
                        "numeric" = "Default value",
                        "matrix" = "Possible answers*")
        placeholder <- switch(input$input_type,
                              "mc" = "Option 1, Option 2, ...",
                              "select" = "Option 1, Option 2, ...",
                              "text" = "Enter placeholder value ...",
                              "numeric" = "Enter default value ...",
                              "matrix" = "Option 1, Option 2, ...")
        value <- switch(input$input_type,
                        "mc" = "",
                        "select" = "",
                        "text" = "Your Answer",
                        "numeric" = "",
                        "matrix" = "")

        if (input$input_type == "y/n") {
          shinyjs::hide(ns("option"), asis = TRUE)
        } else {
          shinyjs::show(ns("option"), asis = TRUE)
          updateTextInput(session, "option", label = label, placeholder = placeholder, value = value)
        }
    }) %>%
      bindEvent(input$input_type)

    observe({
      # enable/disable options if input_id is reused
      if (input$input_type == "matrix" && input$question_id %in% questions()$input_id) {
        # get options of question
        options <- questions()$option[questions()$input_id == input$question_id] %>%
          unique() %>%
          paste(collapse = ", ")
        help_option("Provide a new 'Input ID' to add new options or add more 'Questions'.")
        updateTextInput(session, "option", value = options)
        shinyjs::disable(ns("option"), asis = TRUE)
      } else {
        help_option(NULL)
        shinyjs::enable(ns("option"), asis = TRUE)
      }
    })

    observe({
      # enable/disable question name if input_id is reused
      # this allows to add options to an existing question
      if (input$input_type %in% c("mc", "select") && input$question_id %in% questions()$input_id) {
        # get name of question
        question <- questions()$question[questions()$input_id == input$question_id] %>%
          unique()
        help_question("Provide a new 'Input ID' to add a new 'Question' or add more options.")
        updateTextInput(session, "question", value = question)
        shinyjs::disable(ns("question"), asis = TRUE)
      } else {
        help_question(NULL)
        shinyjs::enable(ns("question"), asis = TRUE)
      }
    })

    observe({
      logDebug("%s: Add new question.", id)
      if (input$question_id %in% questions()$input_id &&
          input$input_type %in% c("text", "numeric", "y/n")) {
        showNotification("Question ID already exists. Please enter a different ID.",
                         duration = 5)
        return(NULL)
      }

      new_rows <- data.frame(option = extractOptions(input$option, type = input$input_type)) %>%
        mutate(
          question = input$question,
          input_type = input$input_type,
          input_id = input$question_id,
          dependence = ifelse(input$dependence, input$dependence_questions, NA),
          dependence_value = ifelse(input$dependence, input$dependence_value, NA),
          required = input$required
        )

      new_question(new_rows)

      # clean inputs if text or numeric or y/n
      if (input$input_type %in% c("text", "numeric", "y/n")) {
        updateTextInput(session, "question", value = "")
        updateTextInput(session, "question_id", value = createRandomID())
        updateTextInput(session, "option", value = "")
      }

      # clean inputs if mc, select
      if (input$input_type %in% c("mc", "select")) {
        updateTextInput(session, "option", value = "")
      }

      # clean inputs if matrix
      if (input$input_type == "matrix") {
        updateTextInput(session, "question", value = "")
      }
    }) %>%
      bindEvent(input$add)

    return(new_question)
  })
}

createRandomID <- function(length = 10) {
  # Define the characters to sample from
  chars <- c(letters, LETTERS, 0:9)

  # Sample characters to form the ID
  random_id <- paste0(sample(chars, length, replace = TRUE), collapse = "")

  return(random_id)
}

extractOptions <- function(options, type) {
  if (is.null(options) || options == "") {
    return("")
  }

  if (type == "y/n") {
    return(c("Yes", "No"))
  }

  res <- strsplit(options, ",")[[1]]

  # remove leading/trailing whitespaces
  res <- trimws(res)

  return(res)
}
