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
    addQuestionUI(ns("new_question")),
    tags$br(),
    fluidRow(
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
      column(
        2,
        style = "margin-top: 1.5em;",
        actionButton(ns("remove"), "Remove", width = "100%")
      )
    ),
    # add option to remove option
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
      choices_df <- init_template$questions[, c("input_id", "question")] %>%
        distinct()
      choices <- choices_df$input_id
      names(choices) <- choices_df$question
      updatePickerInput(session, "remove_questions", choices = choices)
    }) %>%
      bindEvent(init_template$questions,
                ignoreInit = TRUE,
                ignoreNULL = FALSE)

    new_question <- addQuestionServer("new_question", reactive(init_template$questions))

    observe({
      logDebug("%s: Add new question.", id)
      init_template$questions <- rbind(init_template$questions, new_question()) %>%
        distinct()
    }) %>% bindEvent(new_question())

    # enable/disable 'Remove' button
    observe({
      logDebug("%s: Enable/Disable 'Remove' button.", id)
      if (length(input$remove_questions) > 0) {
        choices <- init_template$questions$option[init_template$questions$input_id %in% input$remove_questions] %>%
          unique()
        updatePickerInput(session, "remove_options", choices = choices, selected = choices)
        shinyjs::enable(ns("remove"), asis = TRUE)
      } else {
        updatePickerInput(session, "remove_options", choices = c("Please select question(s) first ..." = ""))
        shinyjs::disable(ns("remove"), asis = TRUE)
      }
    })

    # remove selected questions
    observe({
      logDebug("%s: Remove selected questions.", id)
      questions_df <- init_template$questions
      remove_index <- (questions_df$input_id %in% input$remove_questions) &
        (questions_df$option %in% input$remove_options)

      questions_df <- questions_df[!remove_index, ]
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
