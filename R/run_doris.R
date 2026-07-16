#' Run the Shiny Application
#'
#' @param host host name (see [shiny::runApp()])
#' @param port port number (see [shiny::runApp()])
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp runApp

run_doris <- function(host = NULL, port = NULL, ...) {
  doris_app <- shiny::shinyApp(ui = app_ui, server = app_server)

  # Wenn lokal: runApp starten
  if (interactive()) {
    options(shiny.port = port)
    options(shiny.host = host)
    shiny::runApp(doris_app)
  } else {
    # Für Shiny Server: App-Objekt zurückgeben
    doris_app
  }
}
