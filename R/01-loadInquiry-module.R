#' Load Inquiry UI
#'
#' @rdname loadInquiryServer
#'
#' @export
loadInquiryUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("select_template"),
      "Select Template",
      choices = c("No inquiry available ..." = "")
    ),
    passwordInput(ns("password"), "Password"),
    checkboxInput(ns("show_password"), "Show Password", value = FALSE),
    actionButton(ns("load_execute"), "Load")
  )
}

#' Load Inquiry Server
#'
#' @param id The module id
#' @param submitted_templates Reactive value of submitted templates
#'
#' @export
loadInquiryServer <- function(id, submitted_templates) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    inquiry_template <- empty_template()

    observe({
      logDebug("%s: Update select choices", id)
      if (length(submitted_templates()) > 0) {
        choices <- names(submitted_templates())
      } else {
        choices <- c("No inquiry available ..." = "")
      }

      updateSelectInput(session, "select_template", choices = choices)
    })

    # show/hide password
    observeShowPassword(input, id_password = ns("password"))

    observe({
      logDebug("%s: Enable/disable load button", id)
      if (!is.null(input$select_template) &&
          input$select_template != "") {
        shinyjs::enable(ns("load_execute"), asis = TRUE)
        if (is_encrypted(submitted_templates()[[input$select_template]])) {
          shinyjs::show(ns("password"), asis = TRUE)
          shinyjs::show(ns("show_password"), asis = TRUE)
        } else {
          shinyjs::hide(ns("password"), asis = TRUE)
          shinyjs::hide(ns("show_password"), asis = TRUE)
        }
      } else {
        shinyjs::disable(ns("load_execute"), asis = TRUE)
        shinyjs::hide(ns("password"), asis = TRUE)
        shinyjs::hide(ns("show_password"), asis = TRUE)
      }
    })

    observeEvent(input$load_execute, {
      logDebug("%s: Load template", id)

      selected_template <- submitted_templates()[[input$select_template]]

      if (is_encrypted(selected_template)) {
        logDebug("%s: Try decrypting template", id)

        # try to encrypt the object
        key <- key_sodium(hash(charToRaw(input$password)))
        selected_template <- try({
          decrypt_object(selected_template, key)
        }, silent = TRUE)
      }

      if (inherits(selected_template, "try-error")) {
        logWarn("%s: Decryption failed", id)
        showNotification("Decryption failed. Please check your password.",
                         duration = 5)
        selected_template <- NULL
      }

      req(selected_template)
      entries <- names(selected_template)

      for (entry in entries) {
        inquiry_template[[entry]] <- selected_template[[entry]]
      }
    })

    return(inquiry_template)
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

#' Check if object is encrypted
#'
#' @param obj The object to check
#'
#' @export
is_encrypted <- function(obj) {
  inherits(obj, "raw")  # Checks if the object is of class "raw"
}
