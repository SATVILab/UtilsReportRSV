test_that("setup_output_dir works", {
  wd <- getwd()
  on.exit(setwd(wd))
  wd_temp <- file.path(tempdir(), "test_setup_output_dir")
  dir.create(wd_temp, recursive = TRUE)
  setwd(wd_temp)
  file.create(".here")
  setup_output_dir()
  expect_identical(
    list.files(),
    "_book"
  )
  expect_identical(
    list.files("_book"),
    c("manu", "manu_n")
  )
  expect_identical(
    list.files("_book/manu"),
    c("fig", "tbl")
  )
  expect_identical(
    list.files("_book/manu_n"),
    c("fig", "tbl")
  )
  expect_true(
    all(c("ma"))
  )
})
