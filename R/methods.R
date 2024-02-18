# Construct UI for the methods editor.
methods_ui <- function(id, options) {
  verticalLayout(
    h5("Methods"),
    popover(
      title = "Optimization target",
      help = paste0(
        "These genes will be used as the target for optimization. This means ",
        "that the method weights will be automatically adjusted to maximize ",
        "the scores of this gene set. Select \"Reference genes\", if you want ",
        "to compare your genes of interest with the reference gene set."
      ),
      div(class = "label", "Genes to optimize for")
    ),
    selectInput(
      NS(id, "optimization_genes"),
      label = NULL,
      choices = list(
        "Reference genes" = "reference",
        "Comparison genes" = "comparison"
      )
    ),
    popover(
      title = "Optimization target",
      help = paste0(
        "This determines how the genes, that were selected for optimization, ",
        "are combined. For example, \"Mean rank\" optimizes the method ",
        "weights based on the highest possible mean score across all selected ",
        "genes and \"First rank\" would always focus on the best performing ",
        "gene."
      ),
      div(class = "label", "Optimization target")
    ),
    selectInput(
      NS(id, "optimization_target"),
      label = NULL,
      choices = list(
        "Mean rank" = "mean",
        "Median rank" = "median",
        "First rank" = "min",
        "Last rank" = "max",
        "Customize weights" = "custom"
      )
    ),
    lapply(options$methods, function(method) {
      verticalLayout(
        popover(
          title = method$name,
          help = method$help,
          checkboxInput(
            NS(id, method$id),
            span(
              method$description,
              class = "control-label"
            ),
            value = TRUE
          )
        ),
        sliderInput(
          NS(id, sprintf("%s_weight", method$id)),
          NULL,
          min = -1.0,
          max = 1.0,
          step = 0.01,
          value = 1.0
        )
      )
    })
  )
}

#' Construct server for the methods editor.
#'
#' @param options Global options for the application.
#' @param analysis The reactive containing the results to be weighted.
#' @param comparison_gene_ids The comparison gene IDs.
#'
#' @return A reactive containing the weighted results.
#' @noRd
methods_server <- function(id, options, analysis, comparison_gene_ids) {
  moduleServer(id, function(input, output, session) {
    # Observe each method's enable button and synchronise the slider state.
    lapply(options$methods, function(method) {
      observeEvent(input[[method$id]], {
        shinyjs::toggleState(
          sprintf("%s_weight", method$id),
          condition = input[[method$id]]
        )
      })

      shinyjs::onclick(sprintf("%s_weight", method$id), {
        updateSelectInput(
          session,
          "optimization_target",
          selected = "custom"
        )
      })
    })

    # This reactive will always contain the currently selected optimization
    # gene IDs in a normalized form.
    optimization_gene_ids <- reactive({
      gene_ids <- if (input$optimization_genes == "comparison") {
        comparison_gene_ids()
      } else {
        analysis()$preset$reference_gene_ids
      }

      sort(unique(gene_ids))
    })

    # This reactive will always contain the optimal weights according to
    # the selected parameters.
    optimal_weights <- reactive({
      withProgress(message = "Optimizing weights", {
        setProgress(0.2)

        included_methods <- NULL

        for (method in options$methods) {
          if (input[[method$id]]) {
            included_methods <- c(included_methods, method$id)
          }
        }

        geposan::optimal_weights(
          analysis(),
          included_methods,
          optimization_gene_ids(),
          target = input$optimization_target
        )
      })
    }) |> bindCache(
      analysis(),
      optimization_gene_ids(),
      sapply(options$methods, function(method) input[[method$id]]),
      input$optimization_target
    )

    reactive({
      weights <- NULL

      if (length(optimization_gene_ids()) < 1 |
        input$optimization_target == "custom") {
        for (method in options$methods) {
          if (input[[method$id]]) {
            weight <- input[[sprintf("%s_weight", method$id)]]
            weights[[method$id]] <- weight
          }
        }
      } else {
        weights <- optimal_weights()

        for (method_id in names(weights)) {
          updateSliderInput(
            session,
            sprintf("%s_weight", method_id),
            value = weights[[method_id]]
          )
        }
      }

      geposan::ranking(analysis(), weights)
    })
  })
}
