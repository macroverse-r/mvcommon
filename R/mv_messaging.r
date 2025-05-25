#' Messaging Functions for macroverse Ecosystem
#'
#' @description
#' Consistent messaging functions using the cli package for better user experience.
#' These functions provide standardized error, warning, and information messages
#' across all macroverse packages.
#'
#' @name mv_messaging
#' @examples
#' \dontrun{
#' # Error with details
#' mv_stop("Invalid input", 
#'         "x" = "Expected numeric vector",
#'         "i" = "Got character vector instead")
#' 
#' # Warning
#' mv_warn("Missing values detected",
#'         "!" = "Found {sum(is.na(x))} missing values",
#'         "i" = "These will be removed")
#' 
#' # Success message
#' mv_success("Data loaded successfully",
#'            "v" = "Loaded {nrow(data)} rows",
#'            "v" = "From {n_countries} countries")
#' }
NULL

#' Stop with formatted error message
#'
#' @param message Main error message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_stop <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(message = message, ..., .envir = .envir)
}

#' Warn with formatted warning message
#'
#' @param message Main warning message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_warn <- function(message, ..., .envir = parent.frame()) {
  cli::cli_warn(message = message, ..., .envir = .envir)
}

#' Show success message
#'
#' @param message Success message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_success <- function(message, ..., .envir = parent.frame()) {
  # First show the main success message
  cli::cli_alert_success(message, .envir = .envir)
  
  # Then show any additional details
  details <- list(...)
  if (length(details) > 0) {
    cli::cli_bullets(details, .envir = .envir)
  }
}

#' Show information message
#'
#' @param message Information message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_inform <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(message = message, ..., .envir = .envir)
}

#' Show alert message
#'
#' @param message Alert message
#' @param type Type of alert: "info", "success", "warning", or "danger"
#' @param .envir Environment for string interpolation
#'
#' @export
mv_alert <- function(message, type = "info", .envir = parent.frame()) {
  switch(type,
    info = cli::cli_alert_info(message, .envir = .envir),
    success = cli::cli_alert_success(message, .envir = .envir),
    warning = cli::cli_alert_warning(message, .envir = .envir),
    danger = cli::cli_alert_danger(message, .envir = .envir),
    cli::cli_alert(message, .envir = .envir)
  )
}

#' Show debug message
#'
#' @param message Debug message
#' @param ... Additional message components
#' @param debug Logical, whether to show debug messages (default: from config)
#' @param .envir Environment for string interpolation
#'
#' @export
mv_debug <- function(message, ..., debug = mv_get_config("debug"), .envir = parent.frame()) {
  if (isTRUE(debug)) {
    cli::cli_div(theme = list(.debug = list(color = "grey60", "font-style" = "italic")))
    cli::cli_text("{.debug [DEBUG] {message}}", .envir = .envir)
    
    # Show additional details if provided
    details <- list(...)
    if (length(details) > 0) {
      cli::cli_bullets(details, .envir = .envir)
    }
    cli::cli_end()
  }
}