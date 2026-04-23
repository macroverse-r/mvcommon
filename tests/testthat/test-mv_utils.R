test_that("mv_check_package returns TRUE for an installed package", {
  expect_true(mv_check_package("cli"))
})

test_that("mv_check_package returns FALSE for a missing package", {
  expect_false(mv_check_package("definitely_not_a_real_package_xyz"))
})

test_that("mv_check_package enforces min_version", {
  expect_false(suppressWarnings(mv_check_package("cli", min_version = "99.0.0")))
})

test_that("mv_system_info returns expected fields", {
  info <- mv_system_info()
  expect_s3_class(info, "mv_system_info")
  expected <- c("os", "r_version", "timezone", "working_directory",
                "macroverse_version", "loaded_packages")
  expect_true(all(expected %in% names(info)))
})

test_that("print.mv_system_info produces output", {
  # cli sends its output via rlang::inform (message stream), so
  # capture.output() won't see it; capture the message stream too.
  info <- mv_system_info()
  out <- capture.output(print(info), type = "message")
  expect_true(any(grepl("System Information", out)))
})

test_that("mv_temp_dir creates a directory", {
  path <- mv_temp_dir("test_mvcommon_")
  withr::defer(unlink(path, recursive = TRUE))
  expect_true(dir.exists(path))
})

test_that("mv_check_internet respects the check_internet config flag", {
  prev <- mv_get_config("check_internet")
  withr::defer(suppressMessages(mv_set_config(check_internet = prev)))
  suppressMessages(mv_set_config(check_internet = FALSE))
  expect_true(mv_check_internet())
})
