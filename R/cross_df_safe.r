#' @title Safely cross lists
#'
#' @description Returns the cartesian of all its elements in a list
#' as a dataframe, with one combination per row.

#'
#' @param .l A list of lists or atomic vectors (or, a dataframe).
#' @param .filter function.
#' A predicate function that takes the same number of arguments
#' as the number of variables to be combined.
#'
#' @details
#' Elements in the list to be crossed
#' such as \code{list(1, 2:3)} keep
#' their multi-length elements in one combination
#' (i.e. one dataframe row).
#' That is the sense in which is "safe",
#' as it behaves as expected.
#' The function `purrr::cross` returns an error
#' in this case, whilst `purrr::cross` piped
#' `purrr::map_df(tibble::as_tibble)` w
#' creates two combinations (two dataframe rows)
#' from the second list element above.
#' One could wrap the second element in a list, but
#' that adds an extra layer when accessing the element
#' in the combination.
#'
#' See examples for a demonstration.
#'
#' @examples
#' to_cross_list <- list(
#'  v1 = c("a", "b"),
#'  v2 = list(3, 4:5)
#' )
#' # returns an error
#' try(purrr::cross_df(to_cross_list))
#' # can avoid this by wrapping each to-cross
#' # element's elements in lists,
#' # but then the crossed dataframe
#' # has second column's elements
#' # each be lists
#'  to_cross_list_list <- list(
#'   v1 = c("a", "b"),
#'   v2 = list(list(3), list(4:5))
#' )
#' purrr::cross_df(to_cross_list_list)
#' # returns a four-row dataframe, where
#' # second column has elements of lengths 1, 1, 2 and 2
#' # that are not lists
#' cross_df_safe(to_cross_list)
#' # the above is more natural to work with
#' # no element in a combination
#' # is needlessly wrapped inside a list of length 1
#'
#' @return A dataframe with the same number of rows as
cross_df_safe <- function(.l, .filter = NULL) {
  cols_to_protect_vec <- get_cols_to_protect(.l)

  cross_list <- purrr::cross(.l, .filter)

  cross_list %>%
    purrr::map_df(.convert_to_tibble_row_one,
                  protect = cols_to_protect_vec)
}

get_cols_to_protect <- function(.l) {
  purrr::map_lgl(
    seq_along(.l),
    function(ind) {
      list_vec_lgl <- purrr::map_lgl(.l[[ind]], function(x) {
        is.list(x)
      })
      if (any(list_vec_lgl)) {
        return(TRUE)
      }
      elem_vec_lgl <- purrr::map_lgl(.l[[ind]], function(x) {
        is.numeric(x) || is.logical(x) || is.character(x)
      })
      if(!all(elem_vec_lgl)) return(FALSE)


      purrr::map_lgl(.l[[ind]], function(x) {
        length(x) > 1
      }) %>%
        any()
    }
  ) %>%
    which()
}

.convert_to_tibble_row_one <- function(x, protect = NULL) {
  for (i in seq_along(protect)) {
    x[[protect[i]]] <- list(x[[protect[i]]])
  }
  tibble::as_tibble(x)
}
