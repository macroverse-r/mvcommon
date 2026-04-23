test_that("validates well-formed years", {
  years <- c("2020", "2021", "2022")
  expect_equal(mv_validate_date_format(years, format = "year"), years)
})

test_that("rejects malformed years", {
  expect_error(mv_validate_date_format(c("20", "abcd"), format = "year"))
})

test_that("warns on years outside 1800-2100", {
  expect_warning(mv_validate_date_format(c("1700", "2020"), format = "year"))
})

test_that("validates well-formed quarters", {
  q <- c("2020Q1", "2020Q4", "1999Q2")
  expect_equal(mv_validate_date_format(q, format = "quarter"), q)
})

test_that("rejects malformed quarters", {
  expect_error(mv_validate_date_format(c("2020Q5", "2020"), format = "quarter"))
})

test_that("auto-detects year format", {
  expect_equal(mv_validate_date_format(c("2020", "2021")), c("2020", "2021"))
})

test_that("auto-detects quarter format", {
  q <- c("2020Q1", "2020Q2")
  expect_equal(mv_validate_date_format(q), q)
})

test_that("converts years to Date", {
  d <- mv_validate_date_format(c("2020", "2021"), format = "year", convert = TRUE)
  expect_s3_class(d, "Date")
  expect_equal(d, as.Date(c("2020-01-01", "2021-01-01")))
})

test_that("converts quarters to Date (first day of quarter)", {
  d <- mv_validate_date_format(c("2020Q1", "2020Q2", "2020Q3", "2020Q4"),
                               format = "quarter", convert = TRUE)
  expect_s3_class(d, "Date")
  expect_equal(d, as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01")))
})

test_that("handles NULL and empty input", {
  expect_null(mv_validate_date_format(NULL))
  expect_equal(mv_validate_date_format(character(0)), character(0))
})

test_that("rejects unknown format argument", {
  expect_error(mv_validate_date_format("2020", format = "weekly"))
})
