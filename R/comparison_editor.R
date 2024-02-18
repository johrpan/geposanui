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

    reactive({
      if (input$comparison_genes == "Random genes") {
        preset <- preset()
        gene_pool <- preset$gene_ids
        reference_gene_ids <- preset$reference_gene_ids
        gene_pool <- gene_pool[!gene_pool %chin% reference_gene_ids]
        gene_pool[sample(length(gene_pool), length(reference_gene_ids))]
      } else if (input$comparison_genes == "Your genes") {
        custom_gene_ids()
      } else {
        options$comparison_gene_sets[[input$comparison_genes]]
      }
    })
  })
}
