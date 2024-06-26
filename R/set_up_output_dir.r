
#' @title Set up bookdown output directories
#'
#' @description Create objects that
#' give paths relative to the bookdown output directory.
#' In addition, it creates those directories if they
#' do not already exist.
#'
#' Useful to save results to such folders
#' so that the resultant bookdown output directory
#' is portable, i.e. can be moved around and the
#' path references to the figures and tables
#' still work.
#'
#' If the output directory is unspecified (i.e. there is
#' no entry \code{output_dir} in \code{_bookdown.yml}),
#' then the output directory is taken to be the default \code{_book}.
#'
#' @param sub named character vector.
#' The names are the names of the objects to be created, and
#' the values are paths relative to the report output directory.
#'
#' @return \code{invisible(TRUE)}. Side effects are
#' the creation of directories and saving
#' of objects specifying those directories
#' to global environment.
#'
#' @export
#'
#' @examples
#' wd_temp <- file.path(tempdir(), "test_setup_output_dir")
#' if (!dir.exists(wd_temp)) {
#'   dir.create(wd_temp, recursive = TRUE)
#' }
#' file.create("_bookdown.yml")
#' setup_bookdown_output_dir()
#' dir_m_fig
#'
#' @aliases set_up_bookdown_output_dir
setup_bookdown_output_dir <- function(sub = c(
                                        dir_m_fig = "manu/fig",
                                        dir_m_tbl = "manu/tbl",
                                        dir_mn_fig = "manu_n/fig",
                                        dir_mn_tbl = "manu_n/tbl"
                                      )) {
  if (!file.exists("_bookdown.yml")) {
    stop("_bookdown.yml does not exist, so this seems not to be a bookdown directory.") # nolint
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    utils::install.packages("yaml", quiet = TRUE)
  }
  bd_settings <- yaml::read_yaml("_bookdown.yml")
  dir_bd <- ifelse(
    !is.null(bd_settings$output_dir),
    bd_settings$output_dir,
    "_book"
  )

  if (is.null(names(sub)) && length(sub) > 0) stop("sub must be named")
  if (!is.character(sub) && !is.null(sub)) {
    stop("sub must be a character vector")
  }

  for (i in seq_along(sub)) {
    dir_bd_sub <- file.path(dir_bd, sub[[i]])
    if (!dir.exists(dir_bd_sub)) {
      dir.create(dir_bd_sub, recursive = TRUE)
    }
    assign(
      x = names(sub)[i], value = file.path(dir_bd, sub[[i]]), envir = .GlobalEnv
    )
    sub[i] <- dir_bd_sub
  }

  invisible(sub)
}

#' @rdname setup_output_dir
#' @export
set_up_bookdown_output_dir <- setup_bookdown_output_dir
