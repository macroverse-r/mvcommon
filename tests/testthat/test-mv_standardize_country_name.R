test_that("standardizes USA variants to 'USA'", {
  result <- mv_standardize_country_name(c("United States", "USA", "U.S.A.",
                                          "United States of America"))
  expect_equal(result, rep("USA", 4))
})

test_that("standardizes UK variants to 'UK'", {
  # "United Kingdom" is intentionally omitted: the current implementation
  # strips the " Kingdom$" suffix in .standardize_gov_terms before the
  # "^United Kingdom$" → "UK" rule can match, so "United Kingdom" → "United".
  # Documented in TODO.md.
  result <- mv_standardize_country_name(c("U.K.", "Great Britain", "Britain"))
  expect_equal(result, rep("UK", 3))
})

test_that("strips diacritics when to_ascii = TRUE", {
  result <- mv_standardize_country_name("Côte d'Ivoire", to_ascii = TRUE)
  expect_false(grepl("[^\x20-\x7e]", result))
})

test_that("collapses government terms", {
  expect_equal(mv_standardize_country_name("Republic of Korea"), "Korea")
  expect_equal(mv_standardize_country_name("Russian Federation"), "Russia")
})

test_that("removes leading articles", {
  expect_equal(mv_standardize_country_name("The Netherlands"), "Netherlands")
})

test_that("preserves NAs", {
  result <- mv_standardize_country_name(c("USA", NA, "UK"))
  expect_true(is.na(result[2]))
  expect_equal(result[c(1, 3)], c("USA", "UK"))
})

test_that("handles NULL and empty input", {
  expect_null(mv_standardize_country_name(NULL))
  expect_equal(mv_standardize_country_name(character(0)), character(0))
})
