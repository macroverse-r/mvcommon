#' Check Memory Usage of Data Objects
#'
#' @description
#' Reports memory usage of data objects and estimates memory requirements
#' for operations. Useful for preventing out-of-memory errors when working
#' with large datasets.
#'
#' @param object R object to check
#' @param operation Optional operation to estimate ("join", "pivot", "aggregate")
#' @param ... Additional objects for multi-object operations
#'
#' @return Invisibly returns a list with size information. Prints a
#'   formatted summary to the console.
#'
#' @export
#' @examples
#' # Check single object
#' mv_memory_usage(iris)
#' 
#' # Estimate join operation
#' df1 <- data.frame(id = 1:1000, value = rnorm(1000))
#' df2 <- data.frame(id = 1:1000, other = rnorm(1000))
#' mv_memory_usage(df1, operation = "join", df2)
mv_memory_usage <- function(object, operation = NULL, ...) {
  # Get object name
  obj_name <- deparse(substitute(object))
  
  # Calculate size
  size_bytes <- object.size(object)
  size_info <- .format_bytes(size_bytes)
  
  # Get dimensions if applicable
  dims <- .get_dimensions(object)
  
  # Print basic info
  mv_inform("Memory usage for {.val {obj_name}}",
            "i" = "Size: {size_info$formatted}",
            "i" = if (!is.null(dims)) "Dimensions: {dims}" else NULL)
  
  # Estimate operation memory if requested
  if (!is.null(operation)) {
    other_objects <- list(...)
    estimate <- .estimate_operation_memory(object, operation, other_objects)
    
    contextual::cx_alert("Estimated memory for {.val {operation}} operation: {estimate$formatted}",
             type = if (estimate$warning) "warning" else "info")
    
    if (estimate$warning) {
      contextual::cx_warn("Operation may require significant memory",
              "!" = "Consider processing in chunks or increasing available memory")
    }
  }
  
  # Return info invisibly
  invisible(list(
    size_bytes = as.numeric(size_bytes),
    size_formatted = size_info$formatted,
    dimensions = dims,
    operation_estimate = if (!is.null(operation)) estimate else NULL
  ))
}

# Internal helper functions
.format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  
  if (bytes < 1024) {
    formatted <- paste0(round(bytes), " B")
  } else if (bytes < 1024^2) {
    formatted <- paste0(round(bytes / 1024, 1), " KB")
  } else if (bytes < 1024^3) {
    formatted <- paste0(round(bytes / 1024^2, 1), " MB")
  } else {
    formatted <- paste0(round(bytes / 1024^3, 2), " GB")
  }
  
  list(bytes = bytes, formatted = formatted)
}

.get_dimensions <- function(object) {
  if (is.data.frame(object) || is.matrix(object)) {
    paste0(format(nrow(object), big.mark = ","), " rows × ", 
           ncol(object), " columns")
  } else if (is.list(object)) {
    paste0("List with ", length(object), " elements")
  } else if (is.vector(object)) {
    paste0("Vector with ", format(length(object), big.mark = ","), " elements")
  } else {
    NULL
  }
}

#' Estimate memory requirements for operations
#' @keywords internal
#' @noRd
.estimate_operation_memory <- function(object, operation, other_objects = list()) {
  base_size <- as.numeric(object.size(object))
  
  # Add sizes of other objects
  other_sizes <- sum(sapply(other_objects, function(x) as.numeric(object.size(x))))
  
  # Estimate based on operation type
  multiplier <- switch(operation,
    join = 2.5,      # Joins typically need extra memory for indices
    pivot = 3,       # Pivoting can expand data significantly
    aggregate = 1.5, # Aggregation usually reduces size
    2               # Default conservative estimate
  )
  
  estimated_bytes <- (base_size + other_sizes) * multiplier
  
  # Check if it's a large operation
  available_memory <- as.numeric(memory.limit()) * 1024^2  # Convert MB to bytes
  warning <- estimated_bytes > available_memory * 0.5  # Warn if > 50% of available
  
  formatted <- .format_bytes(estimated_bytes)
  formatted$warning <- warning
  
  formatted
}