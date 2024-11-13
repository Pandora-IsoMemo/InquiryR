#' Sanitize questions for JSON
#'
#' Replace NA in list$questions with list(NULL) to avoid empty columns. This prevents toJSON from dropping empty columns.
#'
#' @param list A list with a data frame named questions
#'
#' @return A list with NA replaced by list(NULL) in list$questions
#'
#' @export
sanitizeQuestionsForJson <- function(list) {
  # replace NA in list$questions with list(NULL)
  # otherwise toJSON will drop empty columns
  df <- list$questions
  df[is.na(df)] <- ""
  list$questions <- df
  return(list)
}

#' Sanitize Questions
#'
#' Replace 'dependence' and 'dependence_value' values equal to "" with NA.
#'
#' @param questions The questions to sanitize
#'
#' @export
sanitizeQuestions <- function(questions) {
  # correct empty dependence values
  questions$dependence[questions$dependence == ""] <- NA
  questions$dependence_value[questions$dependence_value == ""] <- NA

  return(questions)
}
