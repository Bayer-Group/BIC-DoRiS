#' helper function for helptext in doris app
#'
#' @param id identifier name
#' @param text help text

helpTextDropdownButton <- function(id, text, right) {
  shinyWidgets::dropdownButton(
    inputId = id,
    text,
    status = "doris_custom",
    circle = TRUE,
    size = "xs",
    icon = icon("question"),
    width = "600px",
    right = right
  )
}
