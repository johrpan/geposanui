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
    h5("Inputs"),
    popover(
      title = "Species to include",
      help = paste0(
        "This can be used to limit the input dataset to a predefined set of ",
        "species. Normally, it is reasonable to use all species. So, do not ",
        "change this unless you have specific reasons to do so. The ",
        "algorithms will automatically optimize the input dataset by ",
        "excluding species that do not share enough genes."
      ),
      div(class = "label", "Species to include")
    ),
    selectInput(
      NS(id, "species"),
      label = NULL,
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
    popover(
      title = "Reference genes",
      help = paste0(
        "The reference genes are the main input to the computation. They are ",
        "used for computing some of the scores and for optimizing the weights ",
        "of the different methods."
      ),
      div(class = "label", "Reference genes")
    ),
    selectInput(
      NS(id, "reference_genes"),
      label = NULL,
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
    tabsetPanel(
      id = NS(id, "error_panel"),
      type = "hidden",
      tabPanelBody(value = "hide"),
      tabPanelBody(
        value = "show",
        div(
          style = "color: red;",
          htmlOutput(NS(id, "errors"))
        )
      )
    ),
    tabsetPanel(
      id = NS(id, "warning_panel"),
      type = "hidden",
      tabPanelBody(value = "hide"),
      tabPanelBody(
        value = "show",
        div(
          style = "color: orange;",
          htmlOutput(NS(id, "warnings"))
        )
      )
    ),
    if (options$locked) {
      HTML(paste0(
        "This instance prohibits performing custom analyses ",
        "to reduce resource usage. Normally, it is possible ",
        "to use this web application for analyzing any set of ",
        "reference genes to find patterns in their ",
        "chromosomal positions. If you would like to apply ",
        "this method for your own research, see ",
        "<a href=\"https://github.com/johrpan/geposanui/blob/main/README.md\" ",
        "target=\"_blank\">this page</a> for ",
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
    preset_errors <- reactiveVal(character())
    preset_warnings <- reactiveVal(character())

    output$errors <- renderUI({
      HTML(paste(preset_errors(), collapse = "<br>"))
    })

    output$warnings <- renderUI({
      HTML(paste(preset_warnings(), collapse = "<br>"))
    })

    observe({
      updateTabsetPanel(
        session,
        "error_panel",
        selected = if (is.null(preset_errors())) "hide" else "show"
      )
    })

    observe({
      updateTabsetPanel(
        session,
        "warning_panel",
        selected = if (is.null(preset_warnings())) "hide" else "show"
      )
    })

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

      new_errors <- character()
      new_warnings <- character()

      preset <- withCallingHandlers(
        tryCatch(
          geposan::preset(
            reference_gene_ids,
            species_ids = species_ids,
            methods = options$methods
          ),
          error = function(e) {
            new_errors <<- c(new_errors, e$message)
            NULL
          }
        ),
        warning = function(w) {
          new_warnings <<- c(new_warnings, w$message)
        }
      )

      preset_errors(new_errors)
      preset_warnings(new_warnings)

      if (length(new_errors) >= 1) {
        NULL
      } else {
        preset
      }
    })
  })
}
