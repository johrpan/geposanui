# Construct UI for the methods editor.
methods_ui <- function(id) {
  verticalLayout(
    h3("Methods"),
    selectInput(
      NS(id, "optimization_genes"),
      "Genes to optimize for",
      choices = list(
        "Reference genes" = "reference",
        "Comparison genes" = "comparison"
      )
    ),
    selectInput(
      NS(id, "optimization_target"),
      "Optimization target",
      choices = list(
        "Mean squared error" = "mse",
        "Mean percentile" = "mean",
        "Median percentile" = "median",
        "Worst percentile" = "worst",
        "Best percentile" = "best",
        "Customize weights" = "custom"
      )
    ),
    uiOutput(NS(id, "method_sliders"))
  )
}

# Construct server for the methods editor.
#
# @param analysis The reactive containing the results to be weighted.
#
# @return A reactive containing the weighted results.
methods_server <- function(id, analysis, comparison_gene_ids) {
  moduleServer(id, function(input, output, session) {
    output$method_sliders <- renderUI({
      lapply(analysis()$preset$methods, function(method) {
        verticalLayout(
          checkboxInput(
            session$ns(method$id),
            span(
              method$description,
              class = "control-label"
            ),
            value = TRUE
          ),
          sliderInput(
            session$ns(sprintf("%s_weight", method$id)),
            NULL,
            min = -1.0,
            max = 1.0,
            step = 0.01,
            value = 1.0
          )
        )
      })
    })

    method_observers <- list()
    method_listeners <- list()

    observe({
      for (method_observer in method_observers) {
        destroy(method_observer)
      }

      for (method_listener in method_listeners) {
        shinyjs::removeEvent(method_listener)
      }

      method_observers <- lapply(analysis()$preset$methods, function(method) {
        observeEvent(input[[method$id]], {
          shinyjs::toggleState(
            sprintf("%s_weight", method$id),
            condition = input[[method$id]]
          )
        })
      })

      method_listeners <- lapply(analysis()$preset$methods, function(method) {
        shinyjs::onclick(sprintf("%s_weight", method$id), {
          updateSelectInput(
            session,
            "optimization_target",
            selected = "custom"
          )
        })
      })

      for (method in analysis()$preset$methods) {
        method_observer <-
          method_observers <- c(method_observers, method_observer)

        method_listener <- shinyjs::onclick(sprintf("%s_weight", method$id), {
          updateSelectInput(
            session,
            "optimization_target",
            selected = "custom"
          )
        })

        method_listeners <- c(method_listeners, method_listener)
      }
    }) |> bindEvent(analysis())

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

        for (method in analysis()$preset$methods) {
          if (!is.null(input[[method$id]])) {
            if (input[[method$id]]) {
              included_methods <- c(included_methods, method$id)
            }
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
      sapply(analysis()$preset$methods, function(method) input[[method$id]]),
      input$optimization_target
    )

    reactive({
      weights <- NULL

      if (length(optimization_gene_ids()) < 1 |
        input$optimization_target == "custom") {
        for (method in analysis()$preset$methods) {
          if (!is.null(input[[method$id]])) {
            if (input[[method$id]]) {
              weight <- input[[sprintf("%s_weight", method$id)]]
              weights[[method$id]] <- weight
            }
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
