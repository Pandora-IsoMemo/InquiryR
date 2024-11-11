#' Inquiry Template UI
#'
#' @rdname inquiryTemplateServer
#'
#' @export
inquiryTemplateUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Inquiry Template"),
    fluidRow(
      column(
        4,
        textInput(
          ns("title"),
          "Template Name",
          placeholder = "Enter template name ...",
          width = "100%"
        )
      ),
      column(8,
             textInput(
               ns("description"),
               "Template Description",
               placeholder = "Enter template description ...",
               width = "100%"
             )
             )
    ),
    tags$br(),
    fluidRow(column(
      10,
      textInput(
        ns("question"),
        "Add a Question",
        placeholder = "Enter question ...",
        width = "100%"
      ),
    ), column(
      2, textInput(ns("question_id"), "Input ID", value = createRandomID())
    )),
    fluidRow(
      column(3, selectInput(
        ns("input_type"),
        "Input Type",
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
        align = "right",
        style = "margin-top: 1.5em;",
        actionButton(ns("add"), "Add", width = "100%")
      )
    ),
    tags$br(),
    fluidRow(
      column(
        10,
        selectInput(
          ns("remove_questions"),
          "Remove question(s)",
          choices = c("Please add questions first ..." = ""),
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(
        2,
        align = "right",
        style = "margin-top: 1.5em;",
        actionButton(ns("remove"), "Remove", width = "100%")
      )
    ),
    tags$br(),
    tableOutput(ns("questions_table"))
  )
}

#' Inquiry Template Server
#'
#' @param id The module id
#' @param inquiry_template The inquiry template containing the questions data frame
#'
#' @export
inquiryTemplateServer <- function(id, inquiry_template) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe Inquiry name and description ----
    observe({
      logDebug("%s: Update 'input$title' value.", id)
      updateTextInput(session, "title", value = inquiry_template$title)
    }) %>% bindEvent(inquiry_template$title)

    observe({
      logDebug("%s: Update 'input$description' value.", id)
      updateTextInput(session, "description", value = inquiry_template$description)
    }) %>% bindEvent(inquiry_template$description)

    observe({
      logDebug("%s: Set 'inquiry_template$title' value.", id)
      inquiry_template$title <- input$title
    }) %>% bindEvent(input$title)

    observe({
      logDebug("%s: Set 'inquiry_template$description' value.", id)
      inquiry_template$description <- input$description
    }) %>% bindEvent(input$description)


    # observe questions ----
    output$questions_table <- renderTable({
      shiny::validate(need(
        nrow(inquiry_template$questions) > 0,
        "Please create an Inquiry template first."
      ))
      inquiry_template$questions
    })

    observe({
      logDebug("%s: Update 'input$remove_questions' choices.", id)
      # update input$remove_questions
      choices <- inquiry_template$questions$input_id
      names(choices) <- inquiry_template$questions$question
      updateSelectInput(session, "remove_questions", choices = choices)
    }) %>%
      bindEvent(inquiry_template$questions,
                ignoreInit = TRUE,
                ignoreNULL = FALSE)

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
      questions_df <- inquiry_template$questions

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
      inquiry_template$questions <- questions_df

      # clean inputs
      updateTextInput(session, "question", value = "")
      updateTextInput(session, "question_id", value = createRandomID())
    }) %>%
      bindEvent(input$add)

    # enable/disable 'Remove' button
    observe({
      logDebug("%s: Enable/Disable 'Remove' button.", id)
      if (length(input$remove_questions) > 0) {
        shinyjs::enable(ns("remove"), asis = TRUE)
      } else {
        shinyjs::disable(ns("remove"), asis = TRUE)
      }
    })

    # remove selected questions
    observe({
      logDebug("%s: Remove selected questions.", id)
      questions_df <- inquiry_template$questions
      questions_df <- questions_df[!questions_df$input_id %in% input$remove_questions, ]
      inquiry_template$questions <- questions_df
    }) %>%
      bindEvent(input$remove)

  })
}

createRandomID <- function(length = 10) {
  # Define the characters to sample from
  chars <- c(letters, LETTERS, 0:9)

  # Sample characters to form the ID
  random_id <- paste0(sample(chars, length, replace = TRUE), collapse = "")

  return(random_id)
}
