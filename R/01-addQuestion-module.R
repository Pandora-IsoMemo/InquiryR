addQuestionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(
      10,
      textInput(
        ns("question"),
        "Add a Question*",
        placeholder = "Enter question ...",
        width = "100%"
      ),
    ), column(
      2, textInput(ns("question_id"), "Input ID*", value = createRandomID())
    )),
    fluidRow(
      column(3, selectInput(
        ns("input_type"),
        "Input Type*",
        c(
          "Text" = "text",
          "Numeric" = "numeric",
          "Multiple Choice" = "mc",
          "Dropdown Select" = "select",
          "Yes/No question" = "y/n"
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
          "Possible response",
          placeholder = "Enter possible response ...",
          width = "100%"
        )
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
    new_questions_df <- reactiveVal(NULL)

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

    observe({
      logDebug("%s: Add new question.", id)
      questions_df <- questions()

      if (input$question_id %in% questions_df$input_id) {
        showNotification("Question ID already exists. Please enter a different ID.",
                         duration = 5)
        return(NULL)
      }

      # add a new row to the questions data frame
      new_question <- data.frame(
        question = input$question,
        option = input$option,
        input_type = input$input_type,
        input_id = input$question_id,
        dependence = ifelse(input$dependence, input$dependence_questions, NA),
        dependence_value = ifelse(input$dependence, input$dependence_value, NA),
        required = input$required
      )

      questions_df <- rbind(questions_df, new_question)

      # update the questions data frame
      new_questions_df(questions_df)

      # clean inputs
      updateTextInput(session, "question", value = "")
      updateTextInput(session, "question_id", value = createRandomID())
    }) %>%
      bindEvent(input$add)

    return(new_questions_df)
  })
}

createRandomID <- function(length = 10) {
  # Define the characters to sample from
  chars <- c(letters, LETTERS, 0:9)

  # Sample characters to form the ID
  random_id <- paste0(sample(chars, length, replace = TRUE), collapse = "")

  return(random_id)
}
