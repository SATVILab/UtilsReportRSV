test_that("loop_and_display works", {

  library(UtilsReportRSV)
  test_tbl <- tibble::tibble(
    x = rep(letters[1:2], each = 2),
    y = purrr::map(1:2, function(i) {
      letters[(3:4) - as.numeric(i)]
    }) %>%
      unlist()
  )
  test_tbl <- tibble::tibble(
    x = rep(letters[1:2], each = 2),
    y = rep(letters[3:4], 2),
  )
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    skip_if_nothing = FALSE
  ))
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x"), skip_if_nothing = TRUE
  ))
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    skip_if_nothing = TRUE
  ))
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    .f = function(x) cat("\n", "abc", "\n"),
    skip_if_nothing = FALSE
  ))
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    header_lvl_max_repeat = TRUE,
    .f = function(x) cat("\n", "abc", "\n"),
    skip_if_nothing = FALSE
  ))
  expect_true(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    .f = function(x) cat("\n", "abc", "\n"),
    orig_to_display = list(
      "x" = c("a" = "1", "b" = "2"),
      "y" = function(x) ifelse(x == "c", "3", "4")
    ),
    skip_if_nothing = FALSE
  ))

  expect_error(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    .f = function(x) cat("\n", "abc", "\n"),
    orig_to_display = list(
      "x" = 1,
      "y" = function(x) ifelse(x == "c", "3", "4")
    ),
    skip_if_nothing = FALSE
  ))
  expect_error(loop_and_display(
    test_tbl,
    .vars = c("x", "y"),
    .f = function(x) cat("\n", "abc", "\n"),
    orig_to_display = list(
      "y" = c("3", "4")
    ),
    skip_if_nothing = FALSE
  ))
})
