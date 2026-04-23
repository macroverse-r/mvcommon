#' Configuration Management for macroverse Ecosystem
#'
#' @description
#' Functions to set and get configuration options for the macroverse ecosystem.
#' These settings affect behavior across all macroverse packages.
#'
#' @name mv_config
#' @examples
#' # Set configuration options
#' mv_set_config(verbose = TRUE, encoding = "UTF-8")
#' 
#' # Get specific option
#' mv_get_config("verbose")
#' 
#' # Get all options
#' mv_get_config()
#' 
#' # Reset to defaults
#' mv_set_config(.reset = TRUE)
NULL

# Internal environment for storing options
.macroverse_options <- new.env(parent = emptyenv())

# Default options
.default_options <- list(
  verbose = TRUE,
  debug = FALSE,
  encoding = "UTF-8",
  max_print = 10,
  progress_bar = TRUE,
  check_internet = TRUE,
  cache_dir = NULL,
  date_format = "%Y-%m-%d",
  decimal_mark = ".",
  thousands_sep = ",",
  scientific = FALSE,
  timezone = "UTC"
)

# Initialize with defaults
.onLoad <- function(libname, pkgname) {
  # Set defaults if not already set
  for (opt in names(.default_options)) {
    if (is.null(.macroverse_options[[opt]])) {
      .macroverse_options[[opt]] <- .default_options[[opt]]
    }
  }
}

#' Set macroverse Configuration Options
#'
#' @param ... Named arguments for options to set
#' @param .reset Logical, reset all options to defaults
#'
#' @return Invisibly returns the previous values of changed options
#' @export
mv_set_config <- function(..., .reset = FALSE) {
  if (.reset) {
    old <- as.list(.macroverse_options)
    rm(list = ls(.macroverse_options), envir = .macroverse_options)
    for (opt in names(.default_options)) {
      .macroverse_options[[opt]] <- .default_options[[opt]]
    }
    mv_success("Configuration reset to defaults")
    return(invisible(old))
  }
  
  dots <- list(...)
  if (length(dots) == 0) {
    mv_warn("No options provided",
            "i" = "Use mv_get_config() to see current options")
    return(invisible(NULL))
  }
  
  # Validate option names
  valid_options <- names(.default_options)
  invalid <- setdiff(names(dots), valid_options)
  if (length(invalid) > 0) {
    mv_stop("Invalid configuration options",
            "x" = "Unknown options: {.val {invalid}}",
            "i" = "Valid options: {.val {valid_options}}")
  }
  
  # Store old values
  old <- list()
  for (opt in names(dots)) {
    old[[opt]] <- .macroverse_options[[opt]]
    .macroverse_options[[opt]] <- dots[[opt]]
  }
  
  # Confirm changes
  if (mv_get_config("verbose")) {
    mv_success("Configuration updated",
               "v" = "Changed {length(dots)} option{?s}")
  }
  
  invisible(old)
}

#' Get macroverse Configuration Options
#'
#' @param option Character, specific option to retrieve. If NULL, returns all options.
#' @param default Default value if option is not set
#'
#' @return The option value, or a list of all options if option is NULL
#' @export
mv_get_config <- function(option = NULL, default = NULL) {
  if (is.null(option)) {
    # Return all options
    opts <- as.list(.macroverse_options)
    # Add any missing defaults
    for (opt in names(.default_options)) {
      if (!opt %in% names(opts)) {
        opts[[opt]] <- .default_options[[opt]]
      }
    }
    return(opts[order(names(opts))])
  }
  
  # Get specific option
  value <- .macroverse_options[[option]]
  if (is.null(value)) {
    # Check defaults
    value <- .default_options[[option]]
  }
  
  # Return default if still NULL
  if (is.null(value)) {
    value <- default
  }
  
  value
}