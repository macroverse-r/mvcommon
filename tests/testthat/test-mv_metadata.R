test_that("adds and retrieves named metadata", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, source = "test", version = "1.0")
  expect_equal(mv_get_metadata(df, "source"), "test")
  expect_equal(mv_get_metadata(df, "version"), "1.0")
})

test_that("get all metadata returns a named list", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, a = 1, b = 2)
  meta <- mv_get_metadata(df)
  expect_type(meta, "list")
  expect_true(all(c("a", "b", "last_updated") %in% names(meta)))
})

test_that("mv_has_metadata checks presence", {
  df <- data.frame(x = 1:3)
  expect_false(mv_has_metadata(df))
  df <- mv_add_metadata(df, k = "v")
  expect_true(mv_has_metadata(df))
  expect_true(mv_has_metadata(df, "k"))
  expect_false(mv_has_metadata(df, "missing"))
})

test_that("remove_metadata drops specified keys", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, a = 1, b = 2)
  df <- suppressMessages(mv_remove_metadata(df, "a"))
  expect_false(mv_has_metadata(df, "a"))
  expect_true(mv_has_metadata(df, "b"))
})

test_that("remove_metadata with no keys drops all", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, a = 1, b = 2)
  df <- suppressMessages(mv_remove_metadata(df))
  expect_false(mv_has_metadata(df))
})

test_that(".replace = FALSE preserves existing values", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, key = "first")
  df <- suppressWarnings(mv_add_metadata(df, key = "second", .replace = FALSE))
  expect_equal(mv_get_metadata(df, "key"), "first")
})

test_that(".replace = TRUE (default) overwrites", {
  df <- data.frame(x = 1:3)
  df <- mv_add_metadata(df, key = "first")
  df <- mv_add_metadata(df, key = "second")
  expect_equal(mv_get_metadata(df, "key"), "second")
})

test_that("add_metadata with no values warns and returns data unchanged", {
  df <- data.frame(x = 1:3)
  result <- suppressWarnings(mv_add_metadata(df))
  expect_identical(result, df)
})
