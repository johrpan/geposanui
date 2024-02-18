# This is needed to make data.table's and shiny's symbols available within the
# package.
#' @import data.table
#' @import shiny
NULL

#' Add a help popover to a control.
#'
#' @param title Title of the help popover.
#' @param help Content of the help popover.
#' @param child The control to add the popover to.
#'
#' @noRd
popover <- function(title, help, child) {
  div(
    style = "display: flex;",
    child,
    div(
      style = "margin-left: 5px;",
      bslib::popover(
        bsicons::bs_icon("question-circle"),
        title = title,
        help,
        options = list(offset = c(0, 10))
      )
    )
  )
}