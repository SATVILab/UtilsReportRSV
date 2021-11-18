#' @title Cat path to a plot for Markdown doc
#'
#' @description Useful so you don't need to
#' remember the syntax.
#'
#' @param x character.
#' Path to a plot.
#'
#' @examples
#' display_plot_md(here::here("test.png"))
display_plot_md <- function(x) cat(paste0("![](", x, ")"), "\n")
