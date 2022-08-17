#' Create the UI for the results page.
#'
#' @param id ID for namespacing.
#' @param options Global options for the application.
#'
#' @return The UI elements.
#'
#' @noRd
results_ui <- function(id, options) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      comparison_editor_ui(NS(id, "comparison_editor"), options),
      methods_ui(NS(id, "methods")),
      filters_ui(NS(id, "filters"))
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
              NS(id, "rank_plot"),
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
              NS(id, "rankings_plot"),
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
              NS(id, "ranking_y"),
              label = NULL,
              choices = list(
                "Combined" = "combined"
              )
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
              NS(id, "ranking_x"),
              label = NULL,
              choices = list(
                "Combined" = "combined"
              )
            ),
            div(
              style = paste0(
                "display: inline-block;",
                "padding: 0.375rem 0.75rem;"
              ),
              checkboxInput(
                NS(id, "use_ranks"),
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
                NS(id, "use_sample"),
                "Take random sample of genes",
                value = TRUE
              )
            )
          ),
          plotly::plotlyOutput(
            NS(id, "ranking_correlation_plot"),
            width = "100%",
            height = "600px"
          )
        ),
        tabPanel(
          title = "Comparison",
          div(
            style = "margin-top: 16px",
            htmlOutput(NS(id, "comparison_text")),
            plotly::plotlyOutput(
              NS(id, "boxplot"),
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
              NS(id, "gene_locations_plot"),
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
              NS(id, "positions_plot_chromosome_name"),
              label = NULL,
              choices = c(
                list("All chromosomes" = "all"),
                chromosome_choices()
              )
            ),
            plotly::plotlyOutput(
              NS(id, "positions_plot"),
              width = "100%",
              height = "600px"
            )
          )
        ),
        tabPanel(
          title = "Detailed results",
          details_ui(NS(id, "results"))
        ),
        tabPanel(
          title = "g:Profiler",
          div(
            style = "margin-top: 16px",
            plotly::plotlyOutput("gost_plot"),
          ),
          div(
            style = "margin-top: 16px",
            DT::DTOutput(NS(id, "gost_details"))
          )
        )
      )
    )
  )
}

#' Application logic for the results page.
#'
#' @param id ID for namespacing.
#' @param options Global application options.
#' @param analysis A reactive containing the analysis that gets visualized.
#'
#' @noRd
results_server <- function(id, options, analysis) {
  preset <- reactive(analysis()$preset)

  moduleServer(id, function(input, output, session) {
    comparison_gene_ids <- comparison_editor_server(
      "comparison_editor",
      preset,
      options
    )

    # Rank the results.
    ranking <- methods_server("methods", analysis, comparison_gene_ids)

    genes_with_distances <- merge(
      geposan::genes,
      geposan::distances[species == "hsapiens"],
      by.x = "id",
      by.y = "gene"
    )

    # Add gene information to the results.
    results <- reactive({
      merge(
        ranking(),
        genes_with_distances,
        by.x = "gene",
        by.y = "id",
        sort = FALSE
      )
    })

    # Apply the filters.
    results_filtered <- filters_server("filters", results)

    # Server for the detailed results panel.
    details_server("results", preset, results_filtered)

    output$rank_plot <- plotly::renderPlotly({
      preset <- preset()
      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      geposan::plot_scores(ranking(), gene_sets = gene_sets)
    })

    output$rankings_plot <- plotly::renderPlotly({
      preset <- preset()

      rankings <- list()
      methods <- preset$methods
      all <- ranking()

      for (method in methods) {
        weights <- list()
        weights[[method$id]] <- 1.0
        rankings[[method$name]] <- geposan::ranking(all, weights)
      }

      rankings[["Combined"]] <- all

      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      geposan::plot_rankings(rankings, gene_sets)
    })

    observe({
      ranking_choices <- c(
        purrr::lmap(preset()$methods, function(method) {
          l <- list()
          l[[method[[1]]$name]] <- method[[1]]$id
          l
        }),
        "Combined" = "combined"
      )

      updateSelectInput(
        session,
        "ranking_y",
         choices = ranking_choices
      )

      updateSelectInput(
        session,
        "ranking_x",
        choices = ranking_choices,
        selected = "combined"
      )
    }) |> bindEvent(preset())

    output$ranking_correlation_plot <- plotly::renderPlotly({
      preset <- preset()
      ranking <- ranking()

      ranking_x <- if (input$ranking_x == "combined") {
        ranking
      } else {
        weights <- list()
        weights[[input$ranking_x]] <- 1.0
        geposan::ranking(ranking, weights)
      }

      ranking_y <- if (input$ranking_y == "combined") {
        ranking
      } else {
        weights <- list()
        weights[[input$ranking_y]] <- 1.0
        geposan::ranking(ranking, weights)
      }

      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      method_names <- preset$methods |> purrr::lmap(function(method) {
        l <- list()
        l[[method[[1]]$id]] <- method[[1]]$name
        l
      })

      method_names[["combined"]] <- "Combined"

      geposan::plot_rankings_correlation(
        ranking_x,
        ranking_y,
        method_names[[input$ranking_x]],
        method_names[[input$ranking_y]],
        gene_sets = gene_sets,
        use_ranks = input$use_ranks,
        use_sample = input$use_sample
      )
    })

    output$comparison_text <- renderUI({
      reference <- geposan::compare(
        ranking(),
        preset()$reference_gene_ids
      )

      comparison <- if (!is.null(comparison_gene_ids())) {
        geposan::compare(ranking(), comparison_gene_ids())
      }

      num <- function(x, digits) {
        format(
          round(x, digits = digits),
          nsmall = digits,
          scientific = FALSE
        )
      }

      comparison_text <- function(name, comparison) {
        glue::glue(
          "The {name} have a mean score of ",
          "<b>{num(comparison$mean_score, 4)}</b> ",
          "resulting in a mean rank of ",
          "<b>{num(comparison$mean_rank, 1)}</b>. ",
          "This corresponds to a percent rank of ",
          "<b>{num(100 * comparison$mean_percentile, 2)}%</b>. ",
          "A Wilcoxon rank sum test gives an estimated score difference ",
          "between <b>{num(comparison$test_result$conf.int[1], 3)}</b> and ",
          "<b>{num(comparison$test_result$conf.int[2], 3)}</b> with a 95% ",
          "confidence. This corresponds to a p-value of ",
          "<b>{num(comparison$test_result$p.value, 4)}</b>."
        )
      }

      reference_div <- div(HTML(
        comparison_text("reference genes", reference)
      ))

      if (!is.null(comparison)) {
        div(
          reference_div,
          div(HTML(comparison_text("comparison genes", comparison)))
        )
      } else {
        reference_div
      }
    })

    output$boxplot <- plotly::renderPlotly({
      preset <- preset()
      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      geposan::plot_boxplot(ranking(), gene_sets)
    })

    output$gene_locations_plot <- plotly::renderPlotly({
      preset <- preset()
      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      geposan::plot_positions(
        preset$species_ids,
        gene_sets,
        reference_gene_ids = preset$reference_gene_ids
      )
    })

    output$positions_plot <- plotly::renderPlotly({
      preset <- preset()
      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      chromosome <- if (input$positions_plot_chromosome_name == "all") {
        NULL
      } else {
        input$positions_plot_chromosome_name
      }

      geposan::plot_scores_by_position(
        ranking(),
        chromosome_name = chromosome,
        gene_sets = gene_sets
      )
    })

    gost <- reactive({
      withProgress(
        message = "Querying g:Profiler",
        value = 0.0,
        { # nolint
          setProgress(0.2)
          gprofiler2::gost(
            results_filtered()[, gene],
            custom_bg = preset()$gene_ids,
            domain_scope = "custom_annotated"
          )
        }
      )
    }) |> bindCache(results_filtered(), preset())

    output$gost_plot <- plotly::renderPlotly({
      gprofiler2::gostplot(
        gost(),
        capped = FALSE,
        interactive = TRUE
      )
    })

    output$gost_details <- DT::renderDT({
      data <- data.table(gost()$result)
      setorder(data, p_value)

      data[, total_ratio := term_size / effective_domain_size]
      data[, query_ratio := intersection_size / query_size]
      data[, increase := (query_ratio - total_ratio) / total_ratio]

      data <- data[, .(
        source,
        term_name,
        total_ratio,
        query_ratio,
        increase,
        p_value
      )]

      dt <- DT::datatable(
        data,
        rownames = FALSE,
        colnames = c(
          "Source",
          "Term",
          "Total ratio",
          "Query ratio",
          "Increase",
          "p-value"
        ),
        options = list(
          pageLength = 25
        )
      ) |>
        DT::formatRound("p_value", digits = 4) |>
        DT::formatPercentage(
          c("total_ratio", "query_ratio", "increase"),
          digits = 2
        )
    })
  })
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
