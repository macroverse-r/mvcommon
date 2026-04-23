restore_config <- function() {
  suppressMessages(mv_set_config(.reset = TRUE))
}

test_that("sets and retrieves an option", {
  withr::defer(restore_config())
  suppressMessages(mv_set_config(debug = TRUE))
  expect_true(mv_get_config("debug"))
})

test_that("returns all options when no argument is passed", {
  opts <- mv_get_config()
  expect_type(opts, "list")
  expect_true("verbose" %in% names(opts))
  expect_true("encoding" %in% names(opts))
})

test_that("reset restores defaults", {
  withr::defer(restore_config())
  suppressMessages(mv_set_config(debug = TRUE))
  suppressMessages(mv_set_config(.reset = TRUE))
  expect_false(mv_get_config("debug"))
  expect_true(mv_get_config("verbose"))
})

test_that("rejects unknown option names", {
  expect_error(suppressMessages(mv_set_config(nonexistent_option = 1)))
})

test_that("returns default for unset option", {
  expect_null(mv_get_config("nonexistent_option"))
  expect_equal(mv_get_config("nonexistent_option", default = 42), 42)
})

test_that("set_config with no args warns", {
  expect_warning(suppressMessages(mv_set_config()))
})
