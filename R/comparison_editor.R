#' Create a comparison editor.
#'
#' @param options Global application options
#' @noRd
comparison_editor_ui <- function(id, options) {
  verticalLayout(
    h5("Comparison"),
    popover(
      title = "Comparison genes",
      help = paste0(
        "Select your genes of interest to compare their scores with the ",
        "reference genes. This will not influence the computation of scores, ",
        "but it will update the visualizations and summary statistics. Select ",
        "\"Your genes\" and use the other controls below for selecting or ",
        "pasting the genes. You can also use predefined gene sets for ",
        "comparison."
      ),
      div(class = "label", "Comparison genes")
    ),
    selectInput(
      NS(id, "comparison_genes"),
      label = NULL,
      choices = c(
        "Your genes",
        "Random genes",
        names(options$comparison_gene_sets)
      )
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Your genes'",
        NS(id, "comparison_genes")
      ),
      gene_selector_ui(NS(id, "custom_genes"))
    ),
    tabsetPanel(
      id = NS(id, "warning_panel"),
      type = "hidden",
      tabPanelBody(value = "hide"),
      tabPanelBody(
        value = "show",
        div(
          style = "color: orange; margin-bottom: 16px;",
          htmlOutput(NS(id, "warnings"))
        )
      )
    )
  )
}

#' Create a server for the comparison editor.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @param preset A reactive containing the current preset.
#' @param options Global application options
#'
#' @return A reactive containing the comparison gene IDs.
#'
#' @noRd
comparison_editor_server <- function(id, preset, options) {
  moduleServer(id, function(input, output, session) {
    custom_gene_ids <- gene_selector_server("custom_genes")

    comparison_warnings <- reactiveVal(character())
    output$warnings <- renderUI({
      HTML(paste(comparison_warnings(), collapse = "<br>"))
    })

    observe({
      updateTabsetPanel(
        session,
        "warning_panel",
        selected = if (is.null(comparison_warnings())) "hide" else "show"
      )
    })

    reactive({
      new_warnings <- character()

      preset <- preset()
      gene_pool <- preset$gene_ids
      reference_gene_ids <- preset$reference_gene_ids
      gene_pool <- gene_pool[!gene_pool %chin% reference_gene_ids]

      gene_ids <- if (input$comparison_genes == "Random genes") {
        gene_pool[sample(length(gene_pool), length(reference_gene_ids))]
      } else if (input$comparison_genes == "Your genes") {
        custom_gene_ids()
      } else {
        options$comparison_gene_sets[[input$comparison_genes]]
      }

      excluded_reference_gene_ids <-
        gene_ids[gene_ids %chin% reference_gene_ids]

      if (length(excluded_reference_gene_ids) > 0) {
        excluded_reference_genes <-
          geposan::genes[id %chin% excluded_reference_gene_ids]
        excluded_reference_genes[is.na(name), name := id]

        new_warnings <- c(new_warnings, paste0(
          "The following genes have been excluded because they are already ",
          "part of the reference genes: ",
          paste(
            excluded_reference_genes$name,
            collapse = ", "
          )
        ))
      }

      excluded_gene_ids <- gene_ids[!gene_ids %chin% gene_pool]

      if (length(excluded_gene_ids) > 0) {
        excluded_genes <-
          geposan::genes[id %chin% excluded_gene_ids]
        excluded_genes[is.na(name), name := id]

        new_warnings <- c(new_warnings, paste0(
          "The following genes are not present in the results: ",
          paste(
            excluded_genes$name,
            collapse = ", "
          )
        ))
      }

      comparison_warnings(new_warnings)

      gene_ids[!gene_ids %chin% reference_gene_ids & gene_ids %chin% gene_pool]
    })
  })
}
