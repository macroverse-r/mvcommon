#' Messaging Functions for macroverse Ecosystem
#'
#' @description
#' Consistent messaging functions using the cli package for better user experience.
#' These functions provide standardized error, warning, and information messages
#' across all macroverse packages.
#'
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
#' @name mv_messaging
NULL

#' Stop with formatted error message
#'
#' @param message Main error message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_stop <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(message, ..., .envir = .envir)
}

#' Warning with formatted message
#'
#' @param message Main warning message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_warn <- function(message, ..., .envir = parent.frame()) {
  cli::cli_warn(message, ..., .envir = .envir)
}

#' Information message
#'
#' @param message Main information message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_inform <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(message, ..., .envir = .envir)
}

#' Success message
#'
#' @param message Main success message
#' @param ... Additional message components using cli syntax
#' @param .envir Environment for string interpolation
#'
#' @export
mv_success <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(c("v" = message, ...), .envir = .envir)
}

#' Alert message
#'
#' @param message Main alert message
#' @param type Type of alert ("info", "warning", "error")
#' @param .envir Environment for string interpolation
#'
#' @export
mv_alert <- function(message, type = "info", .envir = parent.frame()) {
  switch(type,
    "info" = cli::cli_inform(c("i" = message), .envir = .envir),
    "warning" = cli::cli_warn(c("!" = message), .envir = .envir),
    "error" = cli::cli_abort(c("x" = message), .envir = .envir),
    cli::cli_inform(c("i" = message), .envir = .envir)
  )
}

#' Debug message
#'
#' @param message Debug message
#' @param ... Additional message components
#' @param debug Whether to show debug messages
#' @param .envir Environment for string interpolation
#'
#' @export
mv_debug <- function(message, ..., debug = mv_get_config("debug"), .envir = parent.frame()) {
  if (isTRUE(debug)) {
    cli::cli_inform(c("*" = paste0("[DEBUG] ", message), ...), .envir = .envir)
  }
}