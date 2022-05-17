test_that("setup_output_dir works", {
  wd <- getwd()
  on.exit(setwd(wd))
  wd_temp <- file.path(tempdir(), "test_setup_output_dir")
  if (!dir.exists(wd_temp)) {
    dir.create(wd_temp, recursive = TRUE)
  }
  on.exit(try(unlink(wd_temp, recursive = TRUE)))
  setwd(wd_temp)
  invisible(file.create(".here"))
  expect_error(setup_bookdown_output_dir())
  invisible(file.create("_bookdown.yml"))

  setup_bookdown_output_dir()
  expect_identical(
    list.dirs(),
    c(
      ".", "./_book", "./_book/manu", "./_book/manu/fig", "./_book/manu/tbl",
      "./_book/manu_n", "./_book/manu_n/fig", "./_book/manu_n/tbl"
    )
  )
  expect_identical(
    setdiff(
      c(
        ".", "./_book", "./_book/manu", "./_book/manu/fig", "./_book/manu/tbl",
        "./_book/manu_n", "./_book/manu_n/fig", "./_book/manu_n/tbl"
      ),
      list.dirs(recursive = TRUE)
    ),
    character(0)
  )
  env <- environment()
  expect_true(exists("dir_m_fig", envir = .GlobalEnv))
  expect_true(exists("dir_mn_fig", envir = .GlobalEnv))
  expect_true(exists("dir_m_tbl", envir = .GlobalEnv))
  expect_true(exists("dir_mn_tbl", envir = .GlobalEnv))
  rm(dir_m_fig, envir = .GlobalEnv)
  rm(dir_mn_fig, envir = .GlobalEnv)
  rm(dir_m_tbl, envir = .GlobalEnv)
  rm(dir_mn_tbl, envir = .GlobalEnv)

  unlink("_book", recursive = TRUE)

  expect_error(
    setup_bookdown_output_dir(sub = "fig")
  )
  expect_error(
    setup_bookdown_output_dir(sub = 1)
  )

  setup_bookdown_output_dir(sub = c("dir_fig" = "fig"))

  expect_identical(
    setdiff(
      c(".", "./_book", "./_book/fig"),
      list.dirs(recursive = TRUE)
    ),
    character(0)
  )
  expect_true(exists("dir_fig", envir = .GlobalEnv))
  expect_false(exists("dir_m_fig", envir = .GlobalEnv))
  unlink("_book", recursive = TRUE)
  bd_settings <- list("output_dir" = "test")
  yaml::write_yaml(bd_settings, "_bookdown.yml")
  setup_bookdown_output_dir(sub = c("dir_fig" = "fig"))
  expect_identical(
    dir_fig,
    "test/fig"
  )
  rm(dir_fig, envir = .GlobalEnv)
  unlink(".here")
  unlink("test", recursive = TRUE)
  unlink("_bookdown.yml")
})
