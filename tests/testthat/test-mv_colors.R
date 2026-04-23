test_that("returns default palette as character vector", {
  cols <- mv_get_colors("default")
  expect_type(cols, "character")
  expect_equal(length(cols), 8)
})

test_that("returns all palettes when no argument", {
  all_pals <- mv_get_colors()
  expect_type(all_pals, "list")
  expect_true(all(c("default", "series", "bar", "scatter") %in% names(all_pals)))
})

test_that("returns all palettes when 'all'", {
  all_pals <- mv_get_colors("all")
  expect_type(all_pals, "list")
})

test_that("rejects unknown palette name", {
  expect_error(mv_get_colors("nonexistent"))
})

test_that("every palette contains valid hex colours", {
  for (pal_name in c("default", "series", "bar", "scatter", "macroverse_colors")) {
    pal <- mv_get_colors(pal_name)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)),
                info = paste("palette:", pal_name))
  }
})
