#' Convert and Validate Data in One Step
#'
#' @description
#' Convenience function that converts various data formats to a validated
#' data frame suitable for use in the macroverse ecosystem. Combines
#' conversion and validation in a single step.
#'
#' @param x Object to convert (matrix, list, vector, etc.)
#' @param ... Additional arguments passed to mv_validate_data
#' @param .name_repair How to handle column names (uses vctrs::vec_as_names)
#'
#' @return A validated data frame
#' @export
#' @examples
#' # Convert matrix
#' mat <- matrix(1:12, nrow = 3)
#' df <- mv_as_valid_data(mat)
#' 
#' # Convert list
#' lst <- list(x = 1:5, y = letters[1:5])
#' df <- mv_as_valid_data(lst)
#' 
#' # Convert and validate existing data frame
#' df <- mv_as_valid_data(iris)
mv_as_valid_data <- function(x, ..., .name_repair = "unique") {
  # Convert to data frame based on input type
  if (is.data.frame(x)) {
    df <- x
  } else if (is.matrix(x)) {
    df <- as.data.frame(x)
  } else if (is.list(x)) {
    # Check if all elements have same length
    lengths <- lengths(x)
    if (length(unique(lengths)) == 1) {
      df <- as.data.frame(x)
    } else {
      contextual::cx_stop("Cannot convert list to data frame",
              "x" = "List elements have different lengths",
              "i" = "Lengths: {.val {unique(lengths)}}")
    }
  } else if (is.vector(x)) {
    df <- data.frame(value = x)
  } else {
    # Try generic conversion
    df <- tryCatch(
      as.data.frame(x),
      error = function(e) {
        contextual::cx_stop("Cannot convert to data frame",
                "x" = "Object of class {.cls {class(x)}} cannot be converted",
                "i" = "Error: {e$message}")
      }
    )
  }
  
  # Clean names if janitor is available
  if (requireNamespace("janitor", quietly = TRUE)) {
    df <- janitor::clean_names(df, case = "snake")
  }
  
  # Validate and return
  mv_validate_data(df, ...)
}