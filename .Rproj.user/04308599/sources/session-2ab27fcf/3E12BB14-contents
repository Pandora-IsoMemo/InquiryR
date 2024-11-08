#' Start Application
#'
#' @param port port
#' @param launch.browser If true, the system's default web browser will be launched
#'
#' @export
startApplication <- function(port = getOption("shiny.port"),
                             launch.browser = getOption("shiny.launch.browser", interactive())) {
  shiny::runApp(
    system.file("app", package = "InquiryR"),
    port = port,
    host = "0.0.0.0",
    launch.browser = launch.browser
  )
}
