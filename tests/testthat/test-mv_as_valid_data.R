test_that("passes a data.frame through", {
  df <- data.frame(x = 1:3)
  result <- mv_as_valid_data(df, verbose = FALSE)
  expect_s3_class(result, "data.frame")
})

test_that("converts a matrix", {
  mat <- matrix(1:12, nrow = 3)
  result <- mv_as_valid_data(mat, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
})

test_that("converts a list of equal-length elements", {
  lst <- list(x = 1:3, y = letters[1:3])
  result <- mv_as_valid_data(lst, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("errors on list with unequal lengths", {
  lst <- list(x = 1:3, y = letters[1:2])
  expect_error(mv_as_valid_data(lst, verbose = FALSE))
})

test_that("converts a vector to a one-column data.frame", {
  v <- 1:5
  result <- mv_as_valid_data(v, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 1)
})
