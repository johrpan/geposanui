% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{run_app}
\alias{run_app}
\title{Run the application server.}
\usage{
run_app(
  reference_gene_sets,
  species_sets = NULL,
  methods = geposan::all_methods(),
  comparison_gene_sets = NULL,
  locked = FALSE,
  title = "Gene Position Analysis",
  port = 3464
)
}
\arguments{
\item{reference_gene_sets}{A list of predefined gene sets to be used as
reference genes. This should be a named list containing vectors of gene IDs
for each set. You have to provide \emph{at least one gene set} which will be
selected as the initial reference gene set.}

\item{species_sets}{A list of predefined species sets. This should be a named
list containing vectors of species IDs for each set. The names will be used
to present the species set throughout the user interface.}

\item{methods}{A list of \code{\link[geposan:method]{geposan::method}} objects to be used for all
presets. By default, all available methods will be used.}

\item{comparison_gene_sets}{A named list of predefined gene sets to be used
as comparison genes.}

\item{locked}{Whether the application should be locked and prohibit
performing custom analyses. If this is set to \code{TRUE}, only the predefined
gene and species sets are available for customizing the analysis. This may
be useful to limit resource usage on a publicly available instance.}

\item{title}{Set the title of the application.}

\item{port}{The port to serve the application on.}
}
\description{
Run the application server.
}
