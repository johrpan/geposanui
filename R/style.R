#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(HTML(includeText(
      system.file("misc", "style.css", package = "geposanui")
    )))
  )
}
