#' Create the UI for a gene selector.
#'
#' @param id ID for namespacing.
#' @param default_gene_ids Gene IDs of initially selected genes.
#'
#' @return The user interface
#'
#' @noRd
gene_selector_ui <- function(id, default_gene_ids = NULL) {
  named_genes <- geposan::genes[name != ""]
  named_genes <- unique(named_genes, by = "name")
  gene_choices <- named_genes$id
  names(gene_choices) <- named_genes$name

  verticalLayout(
    selectInput(
      NS(id, "identifier_type"),
      "Gene identifiers",
      choices = list(
        "Select from list" = "list",
        "HGNC symbols" = "hgnc",
        "Ensembl gene IDs" = "ensembl"
      )
    ),
    tabsetPanel(
      id = NS(id, "custom_input"),
      type = "hidden",
      tabPanelBody(
        "list",
        shinyWidgets::virtualSelectInput(
          NS(id, "selected_genes"),
          label = NULL,
          choices = gene_choices,
          multiple = TRUE,
          search = TRUE,
          selectAllOnlyVisible = TRUE
        ),
      ),
      tabPanelBody(
        "hgnc",
        textAreaInput(
          NS(id, "hgnc_names_raw"),
          label = NULL,
          value = paste(
            geposan::genes[
              id %chin% default_gene_ids & name != "",
              name
            ],
            collapse = "\n"
          ),
          height = "250px"
        )
      ),
      tabPanelBody(
        "ensembl",
        textAreaInput(
          NS(id, "gene_ids_raw"),
          label = NULL,
          value = paste(default_gene_ids, collapse = "\n"),
          height = "250px"
        )
      )
    )
  )
}

#' Application logic for the gene selector.
#'
#' @param id ID for namespacing.
#'
#' @return A reactive containing the selected gene IDs.
#'
#' @noRd
gene_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateTabsetPanel(
        session,
        "custom_input",
        selected = input$identifier_type
      )
    })

    reactive({
      gene_ids <- if (input$identifier_type == "list") {
        input$selected_genes
      } else if (input$identifier_type == "hgnc") {
        inputs <- unique(strsplit(input$hgnc_names_raw, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        geposan::genes[name %chin% inputs, id]
      } else {
        inputs <- unique(strsplit(input$gene_ids_raw, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        geposan::genes[id %chin% inputs, id]
      }

      if (length(gene_ids) > 100) {
        gene_ids[seq_len(100)]
      } else {
        gene_ids
      }
    })
  })
}
