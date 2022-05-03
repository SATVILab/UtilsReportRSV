test_that("copy_file works", {
  library(UtilsReportRSV)

  dir_test <- file.path(tempdir(), "copy_file")
  from <- file.path(dir_test, "folder1", "folder2", "folder3", "silly.txt")
  if (!dir.exists(dirname(from))) dir.create(dirname(from), recursive = TRUE)
  file.create(from)

  # test straight to_fn
  # --------------------
  copy_file(
    from = from,
    to_fn = file.path(dir_test, "testToFn", "sillier.txt")
  )
  expect_true(
    file.exists(file.path(dir_test, "testToFn", "sillier.txt"))
  )
  unlink(file.path(dir_test, "testToFn"), recursive = TRUE)

  # test straight to_dir
  # --------------------
  copy_file(
    from = from,
    to_dir = file.path(dir_test, "testToDir")
  )
  expect_true(
    file.exists(file.path(dir_test, "testToDir", "silly.txt"))
  )
  unlink(file.path(dir_test, "testToDir"), recursive = TRUE)

  # test keep_relative_path_from
  # --------------------
  copy_file(
    from = from,
    to_dir = file.path(dir_test, "testToDir"),
    keep_relative_path_from = file.path(dir_test, "folder1")
  )
  expect_true(
    file.exists(
      file.path(dir_test, "testToDir", "folder2", "folder3", "silly.txt")
    )
  )
  unlink(
    file.path(dir_test, "testToDir", "folder2", "folder3", "silly.txt"),
    recursive = TRUE
  )

  # test modification
  # --------------------
  copy_file(
    from = from,
    to_dir = file.path(dir_test, "testToDir"),
    keep_relative_path_from = file.path(dir_test, "folder1"),
    make_final_adjustment = function(x) {
      gsub("folder3", "YABADOO", x)
    }
  )
  expect_true(
    file.exists(
      file.path(dir_test, "testToDir", "folder2", "YABADOO", "silly.txt")
    )
  )
  unlink(
    file.path(dir_test, "testToDir", "folder2", "YABADOO", "silly.txt"),
    recursive = TRUE
  )

  # test return_relative_path_from
  # --------------------
  path_abs <- copy_file(
    from = from,
    to_dir = file.path(dir_test, "testToDir"),
    keep_relative_path_from = file.path(dir_test, "folder1"),
    make_final_adjustment = function(x) {
      gsub("folder3", "YABADOO", x)
    }
  )
  expect_true(
    file.exists(path_abs) &&
      !identical(normalizePath(from, winslash = "/"), path_abs)
  )
  path_rel <- copy_file(
    from = from,
    to_dir = file.path(dir_test, "testToDir"),
    keep_relative_path_from = file.path(dir_test, "folder1"),
    make_final_adjustment = function(x) {
      gsub("folder3", "YABADOO", x)
    },
    return_relative_path_from = file.path(dir_test, "testToDir")
  )
  expect_identical("folder2/YABADOO/silly.txt", path_rel)
  unlink(dir_test, recursive = TRUE)
})
