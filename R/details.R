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
#' @param filtered_results A reactive containing the prefiltered results to be
#'   displayed.
#'
#' @noRd
details_server <- function(id, filtered_results) {
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

    methods <- geposan::all_methods()
    method_ids <- sapply(methods, function(method) method$id)
    method_names <- sapply(methods, function(method) method$name)

    columns <- c(
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

    output_data <- reactive({
      filtered_results()[, ..columns][
        ,
        distance := paste0(
          format(
            round(distance / 1000000, digits = 2),
            nsmall = 2,
          ),
          " Mbp"
        )
      ]
    })

    output$download <- downloadHandler(
      filename = "geposan_filtered_results.csv",
      content = function(file) {
        fwrite(output_data(), file = file)
      },
      contentType = "text/csv"
    )

    output$genes <- DT::renderDT({
      dt <- DT::datatable(
        output_data(),
        rownames = FALSE,
        colnames = column_names,
        options = list(
          rowCallback = js_link(),
          columnDefs = list(list(visible = FALSE, targets = 2)),
          pageLength = 25
        )
      )

      DT::formatPercentage(
        dt,
        c(method_ids, "score", "percentile"),
        digits = 2
      )
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
