#' Run the application server.
#'
#' @param reference_gene_sets A list of predefined gene sets to be used as
#'   reference genes. This should be a named list containing vectors of gene IDs
#'   for each set. You have to provide *at least one gene set* which will be
#'   selected as the initial reference gene set.
#' @param species_sets A list of predefined species sets. This should be a named
#'   list containing vectors of species IDs for each set. The names will be used
#'   to present the species set throughout the user interface.
#' @param methods A list of [`geposan::method`] objects to be used for all
#'   presets. By default, all available methods will be used.
#' @param comparison_gene_sets A named list of predefined gene sets to be used
#'   as comparison genes.
#' @param locked Whether the application should be locked and prohibit
#'   performing custom analyses. If this is set to `TRUE`, only the predefined
#'   gene and species sets are available for customizing the analysis. This may
#'   be useful to limit resource usage on a publicly available instance.
#' @param title Set the title of the application.
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(reference_gene_sets,
                    species_sets = NULL,
                    methods = geposan::all_methods(),
                    comparison_gene_sets = NULL,
                    locked = FALSE,
                    title = "Gene Position Analysis",
                    port = 3464) {
  stopifnot(!is.null(reference_gene_sets) & !is.null(reference_gene_sets[[1]]))

  # These function calls make the required java scripts available.
  shinyjs::useShinyjs()
  rclipboard::rclipboardSetup()

  # Bundle of global options to redue broilerplate.
  options <- list(
    reference_gene_sets = reference_gene_sets,
    species_sets = species_sets,
    methods = methods,
    comparison_gene_sets = comparison_gene_sets,
    locked = locked,
    title = title
  )

  # Actually run the app.
  shiny::runApp(
    shiny::shinyApp(ui(options), server(options)),
    port = port
  )
}

#' Generate the main UI for the application.
#'
#' @param options Global options for the application.
#'
#' @noRd
ui <- function(options) {
  div(
    custom_css(),
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    navbarPage(
      id = "main_page",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "united",
        primary = "#1964bf"
      ),
      title = options$title,
      selected = "Results",
      tabPanel(
        "Input data",
        input_page_ui("input_page", options)
      ),
      tabPanel(
        "Results",
        results_ui("results", options)
      ),
      tabPanel(
        "Help",
        div(
          class = "container",
          htmltools::includeMarkdown(
            system.file("content", "manual.md", package = "geposanui")
          )
        )
      )
    ),
    div(
      class = "footer",
      HTML(glue::glue(
        "<code>geposanui</code> version {packageVersion(\"geposanui\")}<br>",
        "GitHub: <a href=\"https://github.com/johrpan/geposanui/\" ",
        "target=\"blank\">johrpan/geposan</a><br>",
        "Citation: <a href=\"https://doi.org/10.1093/nargab/lqae037\" ",
        "target=\"blank\">10.1093/nargab/lqae037</a>"
      ))
    )
  )
}

#' Create a server function for the application.
#'
#' @param options Global application options.
#' @noRd
server <- function(options) {
  function(input, output, session) {
    preset <- input_page_server("input_page", options)

    observe({
      updateNavbarPage(
        session,
        "main_page",
        selected = "Results"
      )
    }) |> bindEvent(preset(), ignoreInit = TRUE)

    # Compute the results according to the preset.
    analysis <- reactive({
      withProgress(
        message = "Analyzing genes",
        value = 0.0,
        { # nolint
          geposan::analyze(
            preset(),
            progress = function(progress) {
              setProgress(progress)
            },
            include_results = FALSE
          )
        }
      )
    }) |> bindCache(preset())

    results_server("results", options, analysis)
  }
}
