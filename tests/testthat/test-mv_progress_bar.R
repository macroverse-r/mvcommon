test_that("creates a progress bar with expected fields", {
  pb <- mv_progress_bar(5)
  expect_s3_class(pb, "mv_progress_bar")
  expect_equal(pb$total, 5)
  expect_equal(pb$current, 0)
})

test_that("print method shows progress", {
  pb <- mv_progress_bar(10)
  out <- capture.output(print(pb))
  expect_true(any(grepl("macroverse progress bar", out)))
  expect_true(any(grepl("0 / 10", out)))
})

# Known bug: tick(), update(), and close() assume `self` is bound in
# the enclosing environment, but it never is. Calling tick() therefore
# errors (either "object 'self' not found" inside the body, or a cli
# progress lookup error because the bar was scoped to mv_progress_bar()
# and already released). Documented in TODO.md.
test_that("tick errors due to broken self-binding (documented bug)", {
  pb <- mv_progress_bar(5)
  expect_error(pb$tick())
})
