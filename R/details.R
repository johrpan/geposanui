#' Construct UI for the detailed results panel.
#' @noRd
details_ui <- function(id) {
  verticalLayout(
    div(
      style = "margin-top: 16px",
      splitLayout(
        cellWidths = "auto",
        uiOutput(NS(id, "copy")),
        downloadButton(NS(id, "download"), "Download CSV")
      )
    ),
    div(
      style = "margin-top: 16px",
      DT::DTOutput(NS(id, "genes"))
    )
  )
}

#' Server for the detailed results panel.
#'
#' @param preset A reactive containing the preset that has been used.
#' @param filtered_results A reactive containing the prefiltered results to be
#'   displayed.
#'
#' @noRd
details_server <- function(id, preset, filtered_results) {
  moduleServer(id, function(input, output, session) {
    output$copy <- renderUI({
      results <- filtered_results()

      gene_ids <- results[, gene]
      names <- results[name != "", name]

      genes_text <- paste(gene_ids, collapse = "\n")
      names_text <- paste(names, collapse = "\n")

      splitLayout(
        cellWidths = "auto",
        rclipboard::rclipButton(
          "copy_ids_button",
          "Copy gene IDs",
          genes_text,
          icon = icon("clipboard")
        ),
        rclipboard::rclipButton(
          "copy_names_button",
          "Copy gene names",
          names_text,
          icon = icon("clipboard")
        )
      )
    })

    columns <- reactive({
      methods <- preset()$methods
      method_ids <- sapply(methods, function(method) method$id)
      method_names <- sapply(methods, function(method) method$name)

      column_ids <- c(
        "rank",
        "gene",
        "name",
        "chromosome",
        "distance",
        method_ids,
        "score",
        "percentile"
      )

      column_names <- c(
        "",
        "Gene",
        "",
        "Chromosome",
        "Distance",
        method_names,
        "Score",
        "Percentile"
      )

      list(
        method_ids = method_ids,
        column_ids = column_ids,
        column_names = column_names
      )
    })

    output_data <- reactive({
      column_ids <- columns()$column_ids
      filtered_results()[, ..column_ids]
    })

    output$download <- downloadHandler(
      filename = "geposan_filtered_results.csv",
      content = \(file) fwrite(output_data(), file = file),
      contentType = "text/csv"
    )

    output$genes <- DT::renderDT({
      columns <- columns()

      data <- copy(output_data())
      data[, distance := glue::glue(
        "{format(round(distance / 1000000, digits = 2), nsmall = 2)} Mbp"
      )]

      DT::datatable(
        data,
        rownames = FALSE,
        colnames = columns$column_names,
        selection = "none",
        options = list(
          rowCallback = js_link(),
          columnDefs = list(list(visible = FALSE, targets = 2)),
          pageLength = 25
        )
      ) |>
        DT::formatRound(c(columns$method_ids, "score"), digits = 4) |>
        DT::formatPercentage("percentile", digits = 2)
    })
  })
}

#' Generate a JavaScript function to replace gene IDs with Ensembl gene links.
#' @noRd
js_link <- function() {
  DT::JS("function(row, data) {
    let id = data[1];
    var name = data[2];
    if (!name) name = 'Unknown';
    let url = `https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=${id}`;
    $('td:eq(1)', row).html(`<a href=\"${url}\" target=\"_blank\">${name}</a>`);
  }")
}
