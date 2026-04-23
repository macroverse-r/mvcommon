test_that("mv_stop throws an error", {
  expect_error(mv_stop("boom"), "boom")
})

test_that("mv_warn emits a warning", {
  expect_warning(mv_warn("careful"), "careful")
})

test_that("mv_inform emits a message", {
  expect_message(mv_inform("hi"), "hi")
})

test_that("mv_success emits a message", {
  expect_message(mv_success("done"), "done")
})

test_that("mv_alert dispatches on type", {
  expect_message(mv_alert("info-msg", type = "info"), "info-msg")
  expect_warning(mv_alert("warn-msg", type = "warning"), "warn-msg")
  expect_error(mv_alert("err-msg", type = "error"), "err-msg")
})

test_that("mv_alert rejects unknown type", {
  expect_error(mv_alert("generic", type = "nonsense"),
               "should be one of")
})

test_that("mv_alert rejects non-scalar / non-character / NA message", {
  expect_error(mv_alert(c("a", "b")), "length 1")
  expect_error(mv_alert(42),          "character vector")
  expect_error(mv_alert(NA_character_), "not be NA")
})

test_that("mv_debug stays silent when debug = FALSE", {
  expect_silent(mv_debug("hidden", debug = FALSE))
})

test_that("mv_debug emits when debug = TRUE", {
  expect_message(mv_debug("visible", debug = TRUE), "visible")
})

# Regression test for the v0.1.0 bug: the old mv_debug used a
# `{message}` glue template resolved in .envir. When the caller's
# frame had no local `message` variable, glue fell through to base
# R message() and cli failed with "cannot coerce type 'closure'".
test_that("mv_debug works from a frame with no local `message`", {
  f <- function() mv_debug("from f", debug = TRUE)
  expect_message(f(), "from f")
})
