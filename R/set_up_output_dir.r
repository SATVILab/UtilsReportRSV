
#' @title Set up output directories
#'
#' @description Set up \code{bookdown} manuscript
#' and non-manuscript figure and table output directories.
#' Creates directories, and saves objects specifying these directories
#' into global environment. See \code{prefix} for details
#' of objects saved.
#'
#' @param dir_base character. Path to base directory where objects
#' are to be saved, relative to \code{here::here()},
#' i.e. the base directory is effectively \code{here::here(dir_base)}.
#' Default is \code{"_book"}.
#' @param prefix character. Prefix for naming
#' objects specifying paths to particular directories.
#' Default is \code{.dir_bd}. This results in objects
#' \code{.dir_bd_manu}, \code{.dir_bd_manu_n},
#' \code{.dir_bd_manu_fig}, \code{dir_bd_manu_tbl},
#' \code{.dir_bd_manu_n_fig} and \code{dir_bd_manu_n_tbl},
#' which provide the absolute paths.
#' Corresponding objects ending in "_rel" (e.g.
#' \code{.dir_bd_manu_rel}) specify the
#' relative paths (relative to \code{here::here()}).
#'
#' @return \code{invisible(TRUE)}. Side effects are
#' the creation of directories and saving
#' of objects specifying those directories
#' to global environment.
#'
#' @export
#'
#' @aliases set_up_output_dir
setup_output_dir <- function(dir_base = "_book",
                             prefix = ".dir_bd_") {

  # auto-install here package if require
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here", quiet = TRUE)
  }

  # directories
  # -------------
  for (x in c("manu", "manu_n")) {
    for (y in c("fig", "tbl")) {
      dir_curr <- file.path(here::here(dir_base), x, y)
      if (!dir.exists(dir_curr)) dir.create(dir_curr, recursive = TRUE)
      nm <- paste0(prefix, x, "_", y)
      assign(
        x =  nm, value = dir_curr, envir = .GlobalEnv
      )

      dir_curr_rel <- gsub(here::here(), "", dir_curr)
      nm <- paste0(prefix, x, "_", y, "_rel")
      assign(
        x = nm, value = dir_curr_rel, envir = .GlobalEnv
      )
    }
  }

  invisible(TRUE)
}

#' @rdname setup_output_dir
#' @export
set_up_output_dir <- setup_output_dir