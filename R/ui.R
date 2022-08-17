#' Generate the main UI for the application.
#'
#' @param options Global options for the application.
#'
#' @noRd
ui <- function(options) {
  ranking_choices <- purrr::lmap(geposan::all_methods(), function(method) {
    l <- list()
    l[[method[[1]]$name]] <- method[[1]]$id
    l
  })

  ranking_choices <- c(ranking_choices, "Combined" = "combined")

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
        sidebarLayout(
          sidebarPanel(
            width = 3,
            comparison_editor_ui("comparison_editor", options),
            methods_ui("methods"),
            filters_ui("filters")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              type = "pills",
              tabPanel(
                title = "Overview",
                div(
                  style = "margin-top: 16px",
                  plotly::plotlyOutput(
                    "rank_plot",
                    width = "100%",
                    height = "600px"
                  )
                )
              ),
              tabPanel(
                title = "Method comparison",
                div(
                  style = "margin-top: 16px",
                  plotly::plotlyOutput(
                    "rankings_plot",
                    width = "100%",
                    height = "600px"
                  )
                )
              ),
              tabPanel(
                title = "Method correlation",
                div(
                  class = "flow-layout",
                  style = "margin-top: 16px",
                  selectInput(
                    "ranking_y",
                    label = NULL,
                    choices = ranking_choices
                  ),
                  span(
                    style = paste0(
                      "display: inline-block;",
                      "margin-right: 12px;",
                      "padding: 0.375rem 0.75rem;"
                    ),
                    "~"
                  ),
                  selectInput(
                    "ranking_x",
                    label = NULL,
                    choices = ranking_choices,
                    selected = "combined"
                  ),
                  div(
                    style = paste0(
                      "display: inline-block;",
                      "padding: 0.375rem 0.75rem;"
                    ),
                    checkboxInput(
                      "use_ranks",
                      "Use ranks instead of scores",
                      value = TRUE
                    )
                  ),
                  div(
                    style = paste0(
                      "display: inline-block;",
                      "padding: 0.375rem 0.75rem;"
                    ),
                    checkboxInput(
                      "use_sample",
                      "Take random sample of genes",
                      value = TRUE
                    )
                  )
                ),
                plotly::plotlyOutput(
                  "ranking_correlation_plot",
                  width = "100%",
                  height = "600px"
                )
              ),
              tabPanel(
                title = "Comparison",
                div(
                  style = "margin-top: 16px",
                  htmlOutput("comparison_text"),
                  plotly::plotlyOutput(
                    "boxplot",
                    width = "100%",
                    height = "600px"
                  )
                )
              ),
              tabPanel(
                title = "Ortholog locations",
                div(
                  style = "margin-top: 16px",
                  plotly::plotlyOutput(
                    "gene_locations_plot",
                    width = "100%",
                    height = "1200px"
                  )
                )
              ),
              tabPanel(
                title = "Scores by position",
                div(
                  class = "flow-layout",
                  style = "margin-top: 16px",
                  selectInput(
                    "positions_plot_chromosome_name",
                    label = NULL,
                    choices = c(
                      list("All chromosomes" = "all"),
                      chromosome_choices()
                    )
                  ),
                  plotly::plotlyOutput(
                    "positions_plot",
                    width = "100%",
                    height = "600px"
                  )
                )
              ),
              tabPanel(
                title = "Detailed results",
                details_ui("results")
              ),
              tabPanel(
                title = "g:Profiler",
                div(
                  style = "margin-top: 16px",
                  plotly::plotlyOutput("gost_plot"),
                ),
                div(
                  style = "margin-top: 16px",
                  DT::DTOutput("gost_details")
                )
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Publication"
      )
    )
  )
}

#' Generate a named list for choosing chromosomes.
#' @noRd
chromosome_choices <- function() {
  choices <- purrr::lmap(
    unique(geposan::genes$chromosome),
    function(name) {
      choice <- list(name)

      names(choice) <- paste0(
        "Chromosome ",
        name
      )

      choice
    }
  )

  choices[order(suppressWarnings(sapply(choices, as.integer)))]
}
