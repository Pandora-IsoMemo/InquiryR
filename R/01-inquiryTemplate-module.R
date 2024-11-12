#' Inquiry Template UI
#'
#' @rdname inquiryTemplateServer
#'
#' @export
inquiryTemplateUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             tags$h3("Inquiry Template")
             ),
      column(
        3,
        style = "margin-top: 1em;",
        textInput(
          ns("title"),
          label = NULL,
          placeholder = "Enter template name*",
          width = "100%"
        )
      ),
      column(6,
             style = "margin-top: 1em;",
             textInput(
               ns("description"),
               label = NULL,
               placeholder = "Enter template description",
               width = "100%"
             )
             )
    ),
    tags$br(),
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
    ),
    tags$br(),
    fluidRow(
      column(
        10,
        selectInput(
          ns("remove_questions"),
          "Remove Question(s)",
          choices = c("Please add questions first ..." = ""),
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(
        2,
        style = "margin-top: 1.5em;",
        actionButton(ns("remove"), "Remove", width = "100%")
      )
    ),
    tags$hr(),
    tags$br(),
    fluidRow(
      column(10,
             htmlOutput(ns("questions_header")),
             tableOutput(ns("questions_table"))),
      column(
        2,
        actionButton(ns("submit"), "Submit Template", width = "100%"),
        checkboxInput(ns("add_password"), "Add Password"),
        conditionalPanel(
          ns = ns,
          condition = "input.add_password",
          passwordInput(ns("password"), "Password"),
          checkboxInput(ns("show_password"), "Show Password", value = FALSE)
        )
      )
    )
  )
}

#' Inquiry Template Server
#'
#' @param id The module id
#' @param init_template An inquiry template containing the questions data frame
#'
#' @export
inquiryTemplateServer <- function(id, init_template) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    submitted_templates <- reactiveVal(list())

    # observe Inquiry name and description ----
    observe({
      logDebug("%s: Update 'input$title' value.", id)
      updateTextInput(session, "title", value = init_template$title)
    }) %>% bindEvent(init_template$title)

    observe({
      logDebug("%s: Update 'input$description' value.", id)
      updateTextInput(session, "description", value = init_template$description)
    }) %>% bindEvent(init_template$description)

    observe({
      logDebug("%s: Set 'init_template$title' value.", id)
      init_template$title <- input$title
    }) %>% bindEvent(input$title)

    observe({
      logDebug("%s: Set 'init_template$description' value.", id)
      init_template$description <- input$description
    }) %>% bindEvent(input$description)


    # observe title ----
    output$questions_header <- renderUI({
      shiny::validate(need(
        !is.null(init_template$title) && init_template$title != "",
        "Please set a title ..."
      ))

      HTML(paste0(
        "<h3>", init_template$title, "</h3>",
        "<h4>", init_template$description, "</h4>",
        "<br>",
        "<p>Questions dataframe:</p>"
      ))
    })

    # observe questions ----
    output$questions_table <- renderTable({
      shiny::validate(need(
        nrow(init_template$questions) > 0,
        "Please add questions first ..."
      ))
      init_template$questions
    }, width = "100%")

    observe({
      logDebug("%s: Update 'input$remove_questions' choices.", id)
      # update input$remove_questions
      choices <- init_template$questions$input_id
      names(choices) <- init_template$questions$question
      updateSelectInput(session, "remove_questions", choices = choices)
    }) %>%
      bindEvent(init_template$questions,
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
      questions_df <- init_template$questions

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
      init_template$questions <- questions_df

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
      questions_df <- init_template$questions
      questions_df <- questions_df[!questions_df$input_id %in% input$remove_questions, ]
      init_template$questions <- questions_df
    }) %>%
      bindEvent(input$remove)

    # show/hide password
    observeShowPassword(input, id_password = ns("password"))

    # enable/disable 'Submit' button
    observe({
      logDebug("%s: Enable/Disable 'Submit' button.", id)
      if (!is.null(init_template$title) && init_template$title != "" && nrow(init_template$questions) > 0) {
        shinyjs::enable(ns("submit"), asis = TRUE)
      } else {
        shinyjs::disable(ns("submit"), asis = TRUE)
      }
    })

    # submit the inquiry template
    save_template <- reactiveVal(FALSE)

    observe({
      logDebug("%s: Submit Inquiry Template.", id)

      # check if name already exists
      if (init_template$title %in% names(submitted_templates())) {
        # ask the user if they want to overwrite the template
        showModal(modalDialog(
          title = "Overwrite Template?",
          "The template already exists. Do you want to overwrite it?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_overwrite"), label = "Overwrite")
          )
        ))

        observe({
          logDebug("%s: Confirm Overwrite.", id)
          # Save new value and close the modal
          save_template(TRUE)
          removeModal()
        }) %>%
          bindEvent(input$confirm_overwrite)
      } else {
        # else save the template
        save_template(TRUE)
      }
    }) %>%
      bindEvent(input$submit)

    observe({
      req(isTRUE(save_template()))
      logDebug("%s: Save Inquiry Template.", id)
      new_templates <- submitted_templates()

      if (isFALSE(input$add_password)) {
        new_template <- reactiveValuesToList(init_template)
      }

      if (isTRUE(input$add_password)) {
        if (is.null(input$password) || input$password == "") {
          showNotification("Please enter a password to encrypt the template.", duration = 5)
          new_template <- NULL
        } else{
          # encrypt template using cyphr
          key <- key_sodium(hash(charToRaw(input$password)))
          message(input$password)
          new_template <- encrypt_object(reactiveValuesToList(init_template), key)
        }
      }

      # reset trigger
      save_template(FALSE)

      req(new_template)
      new_templates[[init_template$title]] <- new_template
      submitted_templates(new_templates)
      # notify user that the template was submitted
      showNotification("An Inquiry Template has been saved", duration = 5)
      # clean password
      updateCheckboxInput(session, "show_password", value = FALSE)
      updateTextInput(session, "password", value = "")
    }) %>%
      bindEvent(save_template())

    return(submitted_templates)
  })
}

# Observe Show/Hide Password
#
# @param input The input object
# @param id_password The password input id
# @param input_show The checkbox input id
observeShowPassword <- function(input, id_password, input_show = "show_password") {
  observe({
    logDebug("Show/Hide password.")
    if (isTRUE(input[[input_show]])) {
      shinyjs::runjs(sprintf("$('#%s').attr('type', 'text');", id_password))
    } else {
      shinyjs::runjs(sprintf("$('#%s').attr('type', 'password');", id_password))
    }
  })
}

#' Empty Template
#'
#' @export
empty_template <- function() {
  reactiveValues(
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
}

createRandomID <- function(length = 10) {
  # Define the characters to sample from
  chars <- c(letters, LETTERS, 0:9)

  # Sample characters to form the ID
  random_id <- paste0(sample(chars, length, replace = TRUE), collapse = "")

  return(random_id)
}
