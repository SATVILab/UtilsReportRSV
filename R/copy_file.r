#' @title Copy individual file(s)
#'
#' @description Copy individual files across
#' to destinations whose exact value may be calculated
#' based on certain parameters.
#' Note that unlike \code{base::file.copy},
#' this is only for individual files and not whole directories.
#' To copy directories, use \code{copy_dir}.
#'
#' @param from character vector.
#' Paths to files that need to be copied.
#' @param to_fn character vector.
#' Paths that files need to be copied to.
#' Must be supplied if \code{to_dir} is missing.
#' If supplied, then \code{to_dir},
#' \code{keep_structure_relative_to_dir} and
#' \code{make_final_adjustment} are ignored.
#' @param to_dir character.
#' Directory to copy files in \code{from} to.
#' Must be supplied if \code{to_fn} is missing.
#' @param keep_relative_path_from
#' character.
#' If specified, then the relative path to the file
#' from \code{to_dir} will be the same
#' as the relative path to the file
#' from \code{keep_structure_relative_to_dir}.
#' For example, if the \code{from} is
#' \code{\home\usr\folder1\folder2\folder3\plot.png} and
#' \code{to_dir} is \code{\home\usr\newfolder1}, then
#' without specifying
#' \code{keep_relative_path_from}
#' the final path coped to will be
#' \code{\home\usr\newfolder1\plot.png}.
#' However, if \code{keep_relative_path_from}
#' is \code{\home\usr\folder1},
#' then the file will be copied to will be
#' \code{\home\usr\newfolder1\folder2\folder3\plot.png}.
#' A frequently-useful option is \code{here::here()} (or \code{getwd()}).
#' Default is \code{NULL}.
#' @param make_final_adjustment function.
#' Function applied to path immediately before copying
#' (if \code{to_fn} was not specified).
#' Must accept a character vector of length 1 as input,
#' and return a character vector of length 1 as output.
#' Default is \code{NULL}.
#' @param overwrite logical.
#' If \code{FALSE}, then any existing file at the final
#' destination will be overwritten.
#' Default is \code{FALSE}.
#' @param return_relative_path_from character.
#' Functions similarly to \code{keep_relative_path_from},
#' except that it modifies the path returned by \code{copy_file}.
#' By default \code{copy_file} returns the absolute path the
#' object was copied to.
#' By specifying \code{return_relative_path_from}, one
#' may obtain a relative path instead.
#' Again, frequently-useful option is
#' \code{here::here()} (or \code{getwd()}) (especially for
#' passing relative paths to \code{knitr::include_graphics}
#' when using \code{bookdown}).
#' Default is \code{NULL}.
#' @param ... Passed to `file.copy`.
#'
#' @return Invisibly returns the final path copied to.
#' Useful for then displaying the file contents.
#'
#' @examples
#'
#' # setup
#'  dir_test <- file.path(tempdir(), "copy_file")
#'  from <- file.path(dir_test, "folder1", "folder2", "folder3", "silly.txt")
#'  if (!dir.exists(dirname(from))) dir.create(dirname(from), recursive = TRUE)
#'  file.create(from)
#'
#'  # show straightforward to_fn
#'  # --------------------
#'  copy_file(
#'    from = from,
#'    to_fn = file.path(dir_test, "testToFn", "sillier.txt")
#'  )
#'  file.exists(file.path(dir_test, "testToFn", "sillier.txt"))
#'
#'  # show straightforward to_dit
#'  # --------------------
#'  copy_file(
#'    from = from,
#'    to_dir = file.path(dir_test, "testToDir")
#'  )
#'  file.exists(file.path(dir_test, "testToDir", "silly.txt"))
#'
#'  # show keep_relative_path_from
#'  # --------------------
#'  copy_file(
#'    from = from,
#'    to_dir = file.path(dir_test, "testToDir"),
#'    keep_relative_path_from = file.path(dir_test, "folder1")
#'  )
#'  file.exists(
#'    file.path(dir_test, "testToDir", "folder2", "folder3", "silly.txt")
#'  )
#'
#'  # show make_final_adjustment
#'  # --------------------
#'  copy_file(
#'    from = from,
#'    to_dir = file.path(dir_test, "testToDir"),
#'    keep_relative_path_from = file.path(dir_test, "folder1"),
#'    make_final_adjustment = function(x) {
#'      gsub("folder3", "YABADOO", x)
#'    }
#'  )
#'
#'  file.exists(
#'    file.path(dir_test, "testToDir", "folder2", "YABADOO", "silly.txt")
#'  )
#'
#'  # show return_relative_path_from
#'  # --------------------
#'  path_abs <- copy_file(
#'    from = from,
#'    to_dir = file.path(dir_test, "testToDir"),
#'    keep_relative_path_from = file.path(dir_test, "folder1"),
#'    make_final_adjustment = function(x) {
#'      gsub("folder3", "YABADOO", x)
#'    }
#'  )
#'  path_abs
#'
#'  path_rel <- copy_file(
#'    from = from,
#'    to_dir = file.path(dir_test, "testToDir"),
#'    keep_relative_path_from = file.path(dir_test, "folder1"),
#'    make_final_adjustment = function(x) {
#'      gsub("folder3", "YABADOO", x)
#'    },
#'    return_relative_path_from = file.path(dir_test, "testToDir")
#'  )
#'  path_rel
#' @export
copy_file <- function(
  from,
  to_fn = NULL,
  to_dir = NULL,
  keep_relative_path_from = NULL,
  make_final_adjustment = NULL,
  return_relative_path_from = NULL,
  overwrite = FALSE,
  ...
) {
  if (missing(from)) {
    stop("from must be supplied")
  }
  from <- normalizePath(
    from,
    winslash = "/"
  )
  if (!file.exists(from)) stop("from must exist")
  if (!missing(to_fn)) {
    # if to path is specified
    to_fn <- normalizePath(
      to_fn,
      winslash = "/",
      mustWork = FALSE
    )
    if (!dir.exists(dirname(to_fn))) {
      dir.create(dirname(to_fn), recursive = TRUE)
    }
    invisible(file.copy(
      from = from,
      to = to_fn,
      overwrite = overwrite,
      ...
    ))
  } else if (missing(to_dir)) {
    stop("At least one of to_fn and to_dir must be retained")
  } else {
    # if to path is just to a directory
    to_dir <- normalizePath(
      to_dir, winslash = "/", mustWork = FALSE
    )

    # if relative structure to be kept
    if (!is.null(keep_relative_path_from)) {
      if (!dir.exists(keep_relative_path_from)) {
        warning(
          "keep_relative_path_from does not exist, making it unlikely to work"
        )
      }
      dir_rel_from <- normalizePath(
        keep_relative_path_from,
        winslash = "/",
        mustWork = FALSE
      )
      nchar_dir_rel_from <- nchar(dir_rel_from)
      dir_append <- substr(
        from,
        start = nchar(dir_rel_from) + 2,
        stop = nchar(dirname(from))
      )
      to_dir <- file.path(to_dir, dir_append)
    }

    # near-final path to be copied to
    to_fn <- file.path(to_dir, basename(from))

    # make any final adjustments
    if (!is.null(make_final_adjustment)) {
      to_fn <- make_final_adjustment(to_fn)
    }

    # create directory to copy to
    if (!dir.exists(dirname(to_fn))) {
      dir.create(dirname(to_fn), recursive = TRUE)
    }

    # copy
    invisible(file.copy(
      from = from,
      to = to_fn,
      overwrite = overwrite,
      ...
    ))
  }

  # return copied to path

  # if just the absolute path
  if (is.null(return_relative_path_from)) {
    return(invisible(to_fn))
  }

  # return path relative to some directory

  if (FALSE) {
    if (!dir.exists(return_relative_path_from)) {
      warning(
        "return_relative_path_from does not exist, making it unlikely to work"
      )
    }
    dir_rel_from <- normalizePath(
      return_relative_path_from,
      winslash = "/",
      mustWork = FALSE
    )
    nchar_dir_rel_from <- nchar(dir_rel_from)
    path_rel <- substr(
      to_fn,
      start = nchar_dir_rel_from + 2,
      stop = nchar(to_fn)
    )
  }

  path_rel <- get_relative_path(
    path = to_fn,
    path_base = return_relative_path_from
  )

  invisible(path_rel)
}

#' @title Get relative path
#'
#' @description Get the path to a file/directory
#' relative to some sub-path.
#'
#' @param path character.
#' Path to object for which we require a relative path.
#' @param path_base character.
#' Path against which the relative path should be calculated.
#'
#' @export
get_relative_path <- function(
  path,
  path_base
) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  path_base <- normalizePath(path_base, winslash = "/", mustWork = FALSE)

    # return path relative to some directory
  if (!dir.exists(path_base)) {
    warning(
      "path_base does not exist, making it unlikely to work"
    )
  }
  dir_rel_from <- normalizePath(
    path_base,
    winslash = "/",
    mustWork = FALSE
  )

  nchar_dir_rel_from <- nchar(dir_rel_from)

  substr(
    path,
    start = nchar_dir_rel_from + 2,
    stop = nchar(path)
  )
}