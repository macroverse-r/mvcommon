test_that("reports memory usage for a data.frame", {
  df <- data.frame(x = 1:100, y = rnorm(100))
  info <- suppressMessages(mv_memory_usage(df))
  expect_type(info, "list")
  expect_true(info$size_bytes > 0)
  expect_true(nchar(info$size_formatted) > 0)
  expect_match(info$dimensions, "100 rows")
})

test_that("reports memory usage for a vector", {
  v <- 1:1000
  info <- suppressMessages(mv_memory_usage(v))
  expect_type(info, "list")
  expect_true(info$size_bytes > 0)
})

test_that("reports memory usage for a list", {
  lst <- list(a = 1:10, b = 1:20)
  info <- suppressMessages(mv_memory_usage(lst))
  expect_type(info, "list")
  expect_true(info$size_bytes > 0)
  expect_match(info$dimensions, "List with 2 elements")
})

# The operation = argument uses memory.limit(), which is Windows-only.
# The helper .estimate_operation_memory is not tested here; see TODO.md.
