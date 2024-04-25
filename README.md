# geposanui

This R package provides a graphical user interface for analyzing genes based
on position data across species. It is a frontend for the R package
[`geposan`](https://github.com/johrpan/geposan). You can visit an example
instance of the application [here](https://tpe-old.uni-rostock.de) where the
method is used to find new
[TPE-OLD](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4233240/) candidate
genes based on the currently known ones.

## Installation

This package depends on [`geposan`](https://github.com/johrpan/geposan'),
which should be installed first.

You can install the development version of `geposanui` using:

```r
# install.packages("remotes")
# remotes::install_github("johrpan/geposan")
remotes::install_github("johrpan/geposanui")
```

See [this page](https://remotes.r-lib.org/reference/install_git.html) for more
information on this command.

## Usage

The main entry point is the `run_app()` function that is provided by the
package. It will setup the initial analysis and provide the interactive user
interface. You have to provide at least one gene set for the application to
work. The following minimal example shows how to do that:

```r
# Ten random genes from Ensembl.
my_interesting_genes <- c(
    "ENSG00000142347",
    "ENSG00000186174",
    "ENSG00000143553",
    "ENSG00000240972",
    "ENSG00000105357",
    "ENSG00000159251",
    "ENSG00000258643",
    "ENSG00000147873",
    "ENSG00000158270",
    "ENSG00000197616"
)

# Choose a name for your gene set.
geposanui::run_app(list("Interesting genes" = my_interesting_genes))
```

This will run the application which you can reach using your favorite browser.
For more information on the options provided by the function, take a look at the
built-in documentation (`?geposanui::run_app`).

## Publication

This method and its implementation have been peer-reviewed and published in
NAR Genomics and Bioinformatics. If you use the package in your research or
would like to refer to our methodology, please cite the following paper:

Elias F Projahn, Georg Fuellen, Michael Walter, Steffen Möller, Proposing
candidate genes under telomeric control based on cross-species position data,
NAR Genomics and Bioinformatics, Volume 6, Issue 2, June 2024, lqae037,
https://doi.org/10.1093/nargab/lqae037

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the
[GNU Affero General Public License](https://www.gnu.org/licenses/agpl-3.0.html)
for more details.
