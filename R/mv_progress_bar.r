#' Create Progress Bar for Long Operations
#'
#' @description
#' Creates a progress bar using the cli package for tracking long-running operations.
#' Provides a consistent interface across all macroverse packages.
#'
#' @param total Total number of items to process
#' @param format Progress bar format string (uses cli syntax)
#' @param clear Whether to clear the progress bar when done
#' @param ... Additional arguments passed to cli::cli_progress_bar
#'
#' @return A progress bar object with methods tick(), update(), and close()
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' pb <- mv_progress_bar(100)
#' for (i in 1:100) {
#'   Sys.sleep(0.01)
#'   pb$tick()
#' }
#' pb$close()
#' 
#' # Custom format
#' pb <- mv_progress_bar(
#'   length(countries),
#'   format = "Processing {cli::pb_current}/{cli::pb_total} countries [{cli::pb_bar}] {cli::pb_percent}"
#' )
#' 
#' for (country in countries) {
#'   # Process country
#'   pb$tick()
#' }
#' }
mv_progress_bar <- function(total, 
                           format = NULL, 
                           clear = TRUE,
                           ...) {
  
  # Default format if not provided
  if (is.null(format)) {
    format <- "Processing {cli::pb_current}/{cli::pb_total} [{cli::pb_bar}] {cli::pb_percent}"
  }
  
  # Create the progress bar
  id <- cli::cli_progress_bar(
    total = total,
    format = format,
    clear = clear,
    ...
  )
  
  # Return object with methods
  structure(
    list(
      id = id,
      current = 0,
      total = total,
      
      tick = function(amount = 1) {
        cli::cli_progress_update(id = id, inc = amount)
        self$current <- self$current + amount
      },
      
      update = function(current) {
        cli::cli_progress_update(id = id, set = current)
        self$current <- current
      },
      
      close = function() {
        cli::cli_progress_done(id = id)
      }
    ),
    class = "mv_progress_bar"
  )
}

#' @export
print.mv_progress_bar <- function(x, ...) {
  cat("macroverse progress bar\n")
  cat("Current:", x$current, "/", x$total, "\n")
  cat("Progress:", round(x$current / x$total * 100), "%\n")
  invisible(x)
}