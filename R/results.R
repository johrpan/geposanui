#' Create the UI for the results page.
#'
#' @param id ID for namespacing.
#' @param options Global options for the application.
#'
#' @return The UI elements.
#'
#' @noRd
results_ui <- function(id, options) {
  ranking_choices <- purrr::lmap(options$methods, function(method) {
    l <- list()
    l[[method[[1]]$name]] <- method[[1]]$id
    l
  })

  ranking_choices <- c(ranking_choices, "Combined" = "combined")

  sidebarLayout(
    sidebarPanel(
      width = 3,
      comparison_editor_ui(NS(id, "comparison_editor"), options),
      methods_ui(NS(id, "methods"), options)
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
          title = "Gene sets",
          div(
            style = "margin: 1rem",
            htmlOutput(NS(id, "comparison_text"))
          ),
          div(
            style = "margin-top: 16px;",
            plotly::plotlyOutput(
              NS(id, "boxplot"),
              width = "100%",
              height = "600px"
            )
          )
        ),
        tabPanel(
          title = "Methods",
          info(paste0(
            "This plot compares the results of the individual methods with ",
            "the combined ranking. It shows a condensed version of the ",
            "overview plot for each method. The thickness of each graph ",
            "represents the distribution of scores for each ranking (violin ",
            "plot)."
          )),
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
          info(paste0(
            "This plot visualizes the correlation between different methods. ",
            "You can use the controls below to select two methods to ",
            "compare. By default, a random sample of genes is used to make ",
            "the visualization easier to interpret. This behavior can be ",
            "disabled by clicking the checkbox."
          )),
          div(
            class = "flow-layout",
            style = "margin: 1rem",
            selectInput(
              NS(id, "ranking_y"),
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
              NS(id, "ranking_x"),
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
          div(
            style = "margin: 1rem",
            htmlOutput(NS(id, "method_correlation"))
          ),
          plotly::plotlyOutput(
            NS(id, "ranking_correlation_plot"),
            width = "100%",
            height = "600px"
          )
        ),
        tabPanel(
          title = "Scores by position",
          info(paste0(
            "This page combines different visualizations of the distribution ",
            "of scores by chromosomal position. Use the menu below to switch ",
            "from the overview to plots for individual human chromosomes."
          )),
          div(
            class = "flow-layout",
            style = "margin: 1rem",
            selectInput(
              NS(id, "positions_plot_chromosome_name"),
              label = NULL,
              choices = c(
                list(
                  "Chromosome overview" = "overview",
                  "All chromosomes" = "all"
                ),
                chromosome_choices()
              )
            )
          ),
          htmlOutput(
            NS(id, "positions_plot"),
            container = \(...) div(style = "width: 100%; height: 600px", ...)
          )
        ),
        tabPanel(
          title = "Ortholog locations",
          info(paste0(
            "This plot shows the locations of the selected genes for each ",
            "species. The blue line visualizes the largest possible ",
            "distance in this species (across all chromosomes)."
          )),
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
          title = "Detailed results",
          details_ui(NS(id, "results"))
        ),
        tabPanel(
          title = "g:Profiler",
          gsea_ui(NS(id, "gsea"))
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
    ranking <- methods_server("methods", options, analysis, comparison_gene_ids)

    genes_with_distances <- merge(
      geposan::genes,
      geposan::distances[species == 9606, .(gene, distance)],
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

    # Server for the detailed results panel.
    details_server("results", options, results)

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

    ranking_x <- reactive({
      if (input$ranking_x == "combined") {
        ranking()
      } else {
        weights <- list()
        weights[[input$ranking_x]] <- 1.0
        geposan::ranking(ranking(), weights)
      }
    })

    ranking_y <- reactive({
      if (input$ranking_y == "combined") {
        ranking()
      } else {
        weights <- list()
        weights[[input$ranking_y]] <- 1.0
        geposan::ranking(ranking(), weights)
      }
    })

    output$method_correlation <- renderText({
      data <- merge(
        ranking_x()[, c("gene", "score")],
        ranking_y()[, c("gene", "score")],
        by = "gene"
      )

      c <- stats::cor(
        data$score.x,
        data$score.y,
        method = "spearman"
      ) |>
        round(digits = 4) |>
        format(nsmall = 4)

      p <- stats::cor.test(
        data$score.x,
        data$score.y,
        method = "spearman"
      )$p.value |>
        round(digits = 4) |>
        format(nsmall = 4)

      HTML(glue::glue(
        "Spearman's rank correlation coefficient: ",
        "<b>{c}</b>, p = <b>{p}</b>"
      ))
    })

    output$ranking_correlation_plot <- plotly::renderPlotly({
      preset <- preset()

      gene_sets <- list("Reference genes" = preset$reference_gene_ids)
      comparison_gene_ids <- comparison_gene_ids()

      if (length(comparison_gene_ids) >= 1) {
        gene_sets <- c(
          gene_sets,
          list("Comparison genes" = comparison_gene_ids)
        )
      }

      method_names <- options$methods |> purrr::lmap(function(method) {
        l <- list()
        l[[method[[1]]$id]] <- method[[1]]$name
        l
      })

      method_names[["combined"]] <- "Combined"

      geposan::plot_rankings_correlation(
        ranking_x(),
        ranking_y(),
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
          div(
            style = "margin-top: 16px;",
            HTML(comparison_text("comparison genes", comparison))
          )
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

    output$positions_plot <- renderUI({
      preset <- preset()

      if (input$positions_plot_chromosome_name == "overview") {
        geposan::plot_chromosomes(ranking())
      } else {
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
      }
    })

    gsea_server("gsea", results)
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
