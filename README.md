
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UtilsReportRMR

<!-- badges: start -->
<!-- badges: end -->

The goal of UtilsReportRMR is to provide utility functions that
facilitate easier, faster and more reliable communication of data
pre-processing and analysis.

## Installation

You can install the development version of UtilsReportRMR like so:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("MiguelRodo/UtilsReportRMR")
```

## Uses

``` r
library(UtilsReportRMR)
```

### Display outputs by section with headings

The function `loop_and display` produces the following result, when
using the `knitr` chunk option `results = "asis"`:

``` r
example_tbl <- data.frame(
  x = rep(c("Female", "Male"), each = 2),
  y = rep(c("Child", "Adult"), 2),
  path_plot = paste0(here::here(), "/man/fig/p_", 1:4, ".png"),
  stringsAsFactors = FALSE
)
loop_and_display(
  .tbl = example_tbl,
  .vars = c("x", "y"), 
  .f = function(df) {
    #print(df)
    cat(paste0("![](", df$path_plot, ")"), "\n")
    #knitr::include_graphics(df$path_plot)
  }
)
#> Loading required namespace: pander
```

## Female

### Adult

![](C:/Users/migue/Work/Packages/UtilsReportRMR/man/fig/p_2.png)

### Child

![](C:/Users/migue/Work/Packages/UtilsReportRMR/man/fig/p_1.png)

## Male

![](C:/Users/migue/Work/Packages/UtilsReportRMR/man/fig/p_4.png)

### Child

![](C:/Users/migue/Work/Packages/UtilsReportRMR/man/fig/p_3.png)

The function copy\_file makes it easier to copy files by working out the
final directory programmatically. Amongst other options, one can ensure
that the relative path from a particular directory is kept in the final
copied-to directory:

``` r
dir_test <- file.path(tempdir(), "copy_file")
from <- file.path(dir_test, "folder1", "folder2", "folder3", "silly.txt")
if (!dir.exists(dirname(from))) dir.create(dirname(from), recursive = TRUE)
file.create(from)
#> [1] TRUE

copy_file(
  from = from,
  to_dir = file.path(dir_test, "testToDir"),
  keep_relative_path_from = file.path(dir_test, "folder1")
)
file.exists(
  file.path(dir_test, "testToDir", "folder2", "folder3", "silly.txt")
)
#> [1] TRUE
unlink(dir_test, recursive = TRUE)
```
