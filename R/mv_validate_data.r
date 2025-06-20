#' Validate Data for macroverse Ecosystem
#' 
#' @description
#' Validates and fixes common data issues to ensure compatibility
#' across the macroverse ecosystem. Checks encoding, structure, and data types.
#' 
#' @param data A data frame, tibble, or data.table
#' @param check_encoding Logical, check and fix encoding issues
#' @param check_structure Logical, validate data structure
#' @param fix Logical, attempt to fix issues automatically
#' @param verbose Logical, print information about fixes
#' 
#' @return The validated (and possibly fixed) data
#' @export
#' @examples
#' # Basic validation
#' data <- mv_validate_data(my_data)
#' 
#' # Validation without automatic fixes
#' data <- mv_validate_data(my_data, fix = FALSE)
mv_validate_data <- function(data, 
                            check_encoding = TRUE,
                            check_structure = TRUE, 
                            fix = TRUE,
                            verbose = TRUE) {
  
  # Check basic structure
  if (!inherits(data, c("data.frame", "tbl", "data.table"))) {
    contextual::cx_stop("Invalid data type", 
            "x" = "Expected a data.frame, tibble, or data.table",
            "i" = "Got {.cls {class(data)}}")
  }
  
  # Track fixes
  fixes_applied <- character()
  
  # Check and fix encoding
  if (check_encoding) {
    encoding_result <- .check_encoding(data)
    if (!encoding_result$valid && fix) {
      data <- .fix_encoding(data, encoding_result$issues)
      fixes_applied <- c(fixes_applied, "Fixed encoding issues")
    }
  }
  
  # Check structure
  if (check_structure) {
    structure_result <- .validate_structure(data)
    if (!structure_result$valid && fix) {
      data <- .fix_structure(data, structure_result$issues)
      fixes_applied <- c(fixes_applied, "Fixed structure issues")
    }
  }
  
  # Report fixes
  if (verbose && length(fixes_applied) > 0) {
    contextual::cx_success("Data validation complete",
              "v" = "Applied {length(fixes_applied)} fixes:",
              "{fixes_applied}")
  }
  
  # Add validation timestamp
  data <- mv_add_metadata(data, 
                         validated = TRUE,
                         validation_date = Sys.time(),
                         validation_version = utils::packageVersion("mvcommon"))
  
  return(data)
}

# Internal helper functions
.check_encoding <- function(data) {
  char_cols <- names(data)[sapply(data, is.character)]
  issues <- list()
  
  for (col in char_cols) {
    encodings <- unique(Encoding(data[[col]]))
    if (any(encodings != "UTF-8" & encodings != "unknown")) {
      issues[[col]] <- encodings[encodings != "UTF-8" & encodings != "unknown"]
    }
  }
  
  list(valid = length(issues) == 0, issues = issues)
}

.fix_encoding <- function(data, issues) {
  for (col in names(issues)) {
    data[[col]] <- enc2utf8(data[[col]])
  }
  data
}

.validate_structure <- function(data) {
  issues <- list()
  
  # Check for list columns
  list_cols <- names(data)[sapply(data, is.list)]
  if (length(list_cols) > 0) {
    issues$list_columns <- list_cols
  }
  
  # Check for duplicate column names
  if (any(duplicated(names(data)))) {
    issues$duplicate_names <- names(data)[duplicated(names(data))]
  }
  
  list(valid = length(issues) == 0, issues = issues)
}

.fix_structure <- function(data, issues) {
  # Handle list columns
  if (!is.null(issues$list_columns)) {
    for (col in issues$list_columns) {
      # Attempt to unlist if possible
      if (all(lengths(data[[col]]) == 1)) {
        data[[col]] <- unlist(data[[col]])
      }
    }
  }
  
  # Handle duplicate names
  if (!is.null(issues$duplicate_names)) {
    names(data) <- make.unique(names(data))
  }
  
  data
}