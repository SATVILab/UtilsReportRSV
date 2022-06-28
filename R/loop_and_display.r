#' @title Print output
#'
#' @description Loop over combinations of elements
#' from column in a dataframe, printing the
#' elements as Markdown headings and outputting
#' some sort of Markdown-output (such as a plot
#' or a table).
#'
#' The key advantage over something like using
#' \code{dplyr::group_by} with \code{dplyr::do}
#' is that the headings are printed out without repeats.
#'
#' The advantage over writing out loops by hand
#' is that it's less typing and much easier to make
#' changes to.
#'
#' @param .tbl dataframe.
#' Contains columns with names in \code{.vars}.
#' @param .vars character vector.
#' Names of columns in \code{.tbl} for which
#' combinations of unique elements within each define a set of
#' row(s) within \code{.tbl}.
#' Each such set of row(s) is passed to the function \code{.f}.
#' @param .f function.
#' Takes a dataframe as input, and prints or cat's output.
#' Default is \code{pander::pandoc.table}.
#' @param sort logical.
#' Whether to sort the unique elements with each looping
#' variable. For example, if we loop on the column \code{"V1"}
#' in the dataframe \code{data.frame(V1 = c(x2, x1, x2, x1), V2 = ...)},
#' if we do not sort the first header related to V1 will be
#' the element x2 and the second x1 (i.e. the order
#' in which they appear in the column). If we sort,
#' this will be reversed and first x1 would appear.
#' Similarly, if V1 were a factor,
#' then sorting would force the elements to be displayed
#' in the order they are specified in the levels of the factor.
#' Default is \code{FALSE}.
#' @param skip_if_nothing logical.
#' If \code{TRUE}, then no headings or output from \code{.f}
#' are printed for combinations of the unique elements in \code{.vars}
#' for which there are no corresponding entries in \code{.tbl}.
#' @param orig_to_display named list.
#' Used to rename headers as specified by unique levels
#' of the variables in \code{.tbl} selected by \code{.vars},
#' e.g. renaming \code{"M"} to \code{"Male"}.
#' Names correspond to columns in \code{.tbl} selected by \code{.vars}.
#' Element is either a named character vector or a function.
#' If the element is a named character vector,
#' then the header is used to select the element within the vector.
#' Note that numeric elements are coerced to character.
#' If the element is a function, then the original header
#' is passed as the first (non-default) argument to the function and
#' the output is then taken to be the header.
#' Default is \code{list()}.
#' @param header_lvl_top integer.
#' The level of the header used for the
#' highest level looping variable.
#' Corresponds to the number of
#' hashes before a Markdown heading.
#' Default is \code{2}.
#' @param header_lvl_max integer.
#' Maximum header level to print.
#' Default is \code{6}.
#' @param header_lvl_max_repeat logical.
#' If \code{TRUE}, then the smallest
#' header level is used for all looping variables
#' corresponding to header level six or higher.
#' if \code{FALSE}, then the variables corresponding
#' to (what would be) header level seven or higher
#' are plotted in bold with their level number in
#' brackets next to them.
#' Default is \code{FALSE}.
#'
#' @examples
#'
#' test_tbl <- tibble::tibble(
#'   x = rep(letters[1:2], each = 3),
#'   y = purrr::map(1:2, function(i) {
#'     letters[(3:5) - as.numeric(i)]
#'   }) %>%
#'     unlist()
#' )
#' loop_and_display(
#'   test_tbl,
#'   .vars = c("x", "y")
#' )
#' loop_and_display(
#'   test_tbl,
#'   .vars = c("x", "y"),
#'   orig_to_display = list(
#'     "x" = c(
#'       "a" = "Letter A",
#'       "b" = "Letter B"
#'     )
#'   )
#' )
#' @return \code{invisible(TRUE)}, if successful.
#'
#' @export
#'
#' @importFrom magrittr %>%
loop_and_display <- function(.tbl,
                             .vars,
                             .f = pander::pandoc.table,
                             sort = FALSE,
                             skip_if_nothing = TRUE,
                             orig_to_display = list(),
                             header_lvl_top = 2,
                             header_lvl_max = 6,
                             header_lvl_max_repeat = FALSE) {
  if (!requireNamespace("pander")) {
    install.packages("pander")
  }
  vars_list <- lapply(.vars, function(x) {
    out <- unique(.tbl[[x]])
    if (sort) out <- sort(out)
    if (is.factor(out)) out <- as.character(out)
    out
  }) %>%
    setNames(.vars)

  # replace with tidyr::cross
  # function or whatever it is
  vars_tbl <- cross_df_safe(vars_list) %>%
    dplyr::arrange(dplyr::across(.vars)) %>%
    dplyr::mutate_if(is.factor, as.character)

  # comparison list for headers
  # stores elements that have not been plotted
  # for a given loop.
  vars_list_hd <- vars_list

  # above resets when last element
  # of last var is considered
  n_var <- length(vars_list)
  # last_elem_list <- purrr::map(vars_list, function(x) x[length(x)])
  for (i in seq_len(nrow(vars_tbl))) {
    vars_row <- vars_tbl[i, ]
    j <- 1
    reset_ind <- j
    if (i > 1) {
      mismatch_vec_ind <- purrr::map_lgl(seq_along(vars_row), function(j) {
        vars_row[[j]] != vars_tbl[i - 1, ][[j]]
      }) %>%
        which()
      mismatch_vec_ind <- switch(as.character(length(mismatch_vec_ind)),
        "0" = , # nolint
        "1" = integer(0),
        mismatch_vec_ind[-1]
      )
    } else {
      mismatch_vec_ind <- integer(0)
    }

    ind_vec <- rep(TRUE, nrow(.tbl))
    for (j in seq_len(ncol(vars_row))) {
      cn <- colnames(vars_row)[j]
      ind_vec <- ind_vec & .tbl[[cn]] %in% vars_row[[cn]]
    }
    if (skip_if_nothing && sum(ind_vec) == 0) {
      for (j in seq_along(mismatch_vec_ind)) {
        vars_list_hd[[mismatch_vec_ind[j]]] <- vars_list[[mismatch_vec_ind[j]]]
      }
      next
    }
    for (j in seq_len(ncol(vars_row))) {
      cn <- colnames(vars_row)[j]
      hd <- vars_row[[j]]
      if (hd %in% vars_list_hd[[cn]]) {
        if (cn %in% names(orig_to_display)) {
          hd <- switch(typeof(orig_to_display[[cn]]),
            "closure" = orig_to_display[[cn]](hd),
            "character" = {
              if (is.null(names(orig_to_display[[cn]]))) {
                stop("labelling vectors in orig_to_display must be named")
              }
              orig_to_display[[cn]][[as.character(hd)]]
            },
            stop("orig_to_display elements must be character vectors or functions") # nolint
          )
        }
        header_lvl_unrestricted <- header_lvl_top - 1 + j
        if (header_lvl_max_repeat || header_lvl_unrestricted <= header_lvl_max) {
          header_lvl <- min(header_lvl_unrestricted, header_lvl_max)
          pander::pandoc.header(
            hd,
            level = header_lvl
          )
        } else {
          lvl_past <- header_lvl_unrestricted - header_lvl_max
          hd <- paste0(hd, " (l", lvl_past, ")")
          cat(paste0(hd, "\n"))
        }
      }
      vars_list_hd[[cn]] <- setdiff(vars_list_hd[[cn]], vars_row[[cn]])
    }
    .f(.tbl[ind_vec, ])
    for (j in seq_along(mismatch_vec_ind)) {
      vars_list_hd[[mismatch_vec_ind[j]]] <- vars_list[[mismatch_vec_ind[j]]]
    }
  }
  invisible(TRUE)
}
