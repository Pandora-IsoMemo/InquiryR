#' Load Inquiry UI
#'
#' @rdname loadInquiryServer
#'
#' @export
loadInquiryUI <- function(id) {
  ns <- NS(id)
  tagList(
    DataTools::importUI(ns("import_template"), label = "Import"),
    selectInput(
      ns("select_template"),
      "Load Inquiry",
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

      import_tmp <- imported_template()[[1]]

      if (!is_encrypted(import_tmp)) {
        new_template <- setNames(list(import_tmp), import_tmp$title)
      } else {
        new_template <- setNames(list(import_tmp), "encrypted inquiry")
      }

      # concatenate the new template with the old ones
      old_template_list <- submitted_templates()
      new_template_list <- updateListNamesIfDuplicate(new_template, old_template_list)

      submitted_templates(c(old_template_list, new_template_list))
      # notify user that the data frame was created
      showNotification("An Inquiry Template has been loaded.", duration = 5)
    })

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

        # try to decrypt the object
        key <- key_sodium(hash(charToRaw(input$password)))
        selected_template <-
          decrypt_object(selected_template, key) %>%
          shinyTools::shinyTryCatch(errorTitle = "Decryption failed. Please check your password.", alertStyle = "shinyalert")
      }

      if (inherits(selected_template, "try-error")) {
        logWarn("%s: Decryption failed", id)
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
    title = "",
    description = "",
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

#' Validate Import
#'
#' @param imported_template The imported template
#'
#' @export
validateImport <- function(imported_template) {
  missingEntries <- setdiff(c("title", "description", "questions"),
                            names(imported_template))
  if (length(missingEntries) > 0) {
    showNotification(paste(
      "The imported template is missing the following entries: ",
      paste(missingEntries, collapse = ", ")
    ),
    duration = 5)

    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check if object is encrypted
#'
#' @param obj The object to check
#'
#' @export
is_encrypted <- function(obj) {
  inherits(obj, "raw")  # Checks if the object is of class "raw"
}
