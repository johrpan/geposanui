#' Create the UI for the input page.
#'
#' @param options Global options for the application.
#'
#' @noRd
input_page_ui <- function(id, options) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      preset_editor_ui(NS(id, "preset_editor"), options),
      tabsetPanel(
        id = NS(id, "apply_panel"),
        type = "hidden",
        tabPanelBody(value = "hide"),
        tabPanelBody(
          value = "show",
          actionButton(
            NS(id, "apply_button"),
            "Perform analysis",
            class = "btn-primary",
            style = "margin-top: 16px; margin-bottom: 16px"
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      plotly::plotlyOutput(
        NS(id, "positions_plot"),
        width = "100%",
        height = "1200px"
      )
    )
  )
}

#' Application logic for the input page.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @param options Global options for the application.
#'
#' @return A reactive containing the `preset` for the analysis.
#'
#' @noRd
input_page_server <- function(id, options) {
  moduleServer(id, function(input, output, session) {
    current_preset <- reactiveVal(
      geposan::preset(options$reference_gene_sets[[1]])
    )

    potential_preset <- preset_editor_server("preset_editor", options)

    output$positions_plot <- plotly::renderPlotly({
      preset <- potential_preset()

      if (is.null(preset)) {
        NULL
      } else {
        geposan::plot_positions(
          preset$species_ids,
          list("Reference genes" = preset$reference_gene_ids),
          reference_gene_ids = preset$reference_gene_ids
        )
      }
    })

    observe({
      if (is.null(potential_preset()) |
        rlang::hash(potential_preset()) ==
          rlang::hash(current_preset())) {
        updateTabsetPanel(
          session,
          "apply_panel",
          selected = "hide"
        )
      } else {
        updateTabsetPanel(
          session,
          "apply_panel",
          selected = "show"
        )
      }
    })

    observe({
      current_preset(potential_preset())
    }) |> bindEvent(input$apply_button)

    current_preset
  })
}
