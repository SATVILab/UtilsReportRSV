
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reportutils

<!-- badges: start -->
<!-- badges: end -->

The goal of reportutils is to provide utility functions that facilitate
easier, faster and more reliable communication of data pre-processing
and analysis.

## Installation

You can install the development version of reportutils like so:

``` r
devtools::install_github("MiguelRodo/reportutils")
```

## Uses

``` r
library(reportutils)
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
```

## Female

### Adult

![](c:/Users/migue/Work/Packages/reportutils/man/fig/p_2.png)

### Child

![](c:/Users/migue/Work/Packages/reportutils/man/fig/p_1.png)

## Male

### Adult

![](c:/Users/migue/Work/Packages/reportutils/man/fig/p_4.png)

### Child

![](c:/Users/migue/Work/Packages/reportutils/man/fig/p_3.png)
