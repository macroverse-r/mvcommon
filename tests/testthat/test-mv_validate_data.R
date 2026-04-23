test_that("mv_validate_data rejects non-data-frames", {
  expect_error(mv_validate_data(1:10))
  expect_error(mv_validate_data("not a frame"))
})

test_that("mv_validate_data passes a clean data.frame through", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  result <- mv_validate_data(df, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("mv_validate_data fixes duplicate column names", {
  df <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")
  result <- mv_validate_data(df, verbose = FALSE)
  expect_false(any(duplicated(names(result))))
})

test_that("mv_validate_data unlists length-1 list columns", {
  df <- data.frame(x = 1:3)
  df$lst <- as.list(1:3)
  result <- mv_validate_data(df, verbose = FALSE)
  expect_false(is.list(result$lst) && !is.atomic(result$lst))
})

test_that("mv_validate_data adds validation metadata", {
  df <- data.frame(x = 1:3)
  result <- mv_validate_data(df, verbose = FALSE)
  expect_true(mv_has_metadata(result, "validated"))
  expect_true(mv_get_metadata(result, "validated"))
  expect_true(mv_has_metadata(result, "validation_date"))
})

test_that("mv_validate_data with fix = FALSE does not modify columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")
  result <- mv_validate_data(df, fix = FALSE, verbose = FALSE)
  expect_true(any(duplicated(names(result))))
})
