#' Create the UI for a preset editor.
#'
#' @param id ID for namespacing.
#' @param options Global options for the application.
#'
#' @return The UI elements.
#'
#' @noRd
preset_editor_ui <- function(id, options) {
  species_choices <- c("All species", names(options$species_sets))
  gene_choices <- names(options$reference_gene_sets)

  if (!options$locked) {
    species_choices <- c(species_choices, "Customize")
    gene_choices <- c(gene_choices, "Customize")
  }

  verticalLayout(
    h3("Inputs"),
    selectInput(
      NS(id, "species"),
      "Species to include",
      choices = species_choices
    ),
    if (!options$locked) {
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'Customize'",
          NS(id, "species")
        ),
        selectizeInput(
          inputId = NS(id, "custom_species"),
          label = "Select input species",
          choices = NULL,
          multiple = TRUE
        ),
      )
    },
    selectInput(
      NS(id, "reference_genes"),
      "Reference genes",
      choices = gene_choices
    ),
    if (!options$locked) {
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'Customize'",
          NS(id, "reference_genes")
        ),
        gene_selector_ui(NS(id, "custom_genes"))
      )
    },
    if (options$locked) {
      HTML(paste0(
        "This instance prohibits performing custom analyses ",
        "to reduce resource usage. Normally, it is possible ",
        "to use this web application for analyzing any set of ",
        "reference genes to find patterns in their ",
        "chromosomal positions. If you would like to apply ",
        "this method for your own research, see ",
        "<a href=\"https://code.johrpan.de/johrpan/geposanui/src/",
        "branch/main/README.md\" target=\"_blank\">this page</a> for ",
        "more information."
      ))
    }
  )
}

#' Application logic for the preset editor.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @param options Global application options.
#'
#' @return A reactive containing the preset or `NULL`, if the input data doesn't
#'   result in a valid one.
#'
#' @noRd
preset_editor_server <- function(id, options) {
  moduleServer(id, function(input, output, session) {
    custom_gene_ids <- if (!options$locked) {
      species_choices <- geposan::species$id
      names(species_choices) <- geposan::species$name

      updateSelectizeInput(
        session,
        "custom_species",
        choices = species_choices,
        server = TRUE
      )

      gene_selector_server("custom_genes")
    } else {
      NULL
    }

    reactive({
      reference_gene_ids <- if (input$reference_genes == "Customize") {
        custom_gene_ids()
      } else {
        options$reference_gene_sets[[input$reference_genes]]
      }

      species_ids <- if (input$species == "All species") {
        geposan::species$id
      } else if (input$species == "Customize") {
        input$custom_species
      } else {
        options$species_sets[[input$species]]
      }

      tryCatch(
        geposan::preset(
          reference_gene_ids,
          species_ids = species_ids
        ),
        error = function(err) NULL
      )
    })
  })
}
