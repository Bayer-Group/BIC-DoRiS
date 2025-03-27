#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp runApp

run_doris <- function(host=NULL, port=NULL, ...) {
  doris_app <- shiny::shinyApp(ui = app_ui, server = app_server)
  shiny::runApp(doris_app, host=host, port=port)
}
