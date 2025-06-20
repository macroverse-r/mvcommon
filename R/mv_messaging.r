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
  # Get the calling function name
  call_info <- sys.call(-1)
  if (!is.null(call_info)) {
    func_name <- as.character(call_info[[1]])
    # Format message with function name in blue/bold and message in red
    formatted_message <- paste0("in ", 
                                cli::style_bold(cli::col_blue(func_name)), 
                                "(): ", 
                                cli::col_red(message))
  } else {
    formatted_message <- cli::col_red(message)
  }
  
  cli::cli_abort(c("x" = formatted_message), ..., .envir = .envir)
}

#' Issue a formatted warning message
#'
#' @param message Warning message text
#' @param ... Additional arguments passed to cli functions
#' @param verbose Whether to show the warning (TRUE) or suppress it (FALSE)
#' @param bullet Type of bullet to use ("warn", "fire", "x")
#' @param .envir Environment for glue evaluation
#'
#' @export
mv_warn <- function(message, ..., verbose = TRUE, bullet = "warn", .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    # Validate bullet type
    valid_bullets <- c("warn", "fire", "x")
    if (!bullet %in% valid_bullets) {
      cli::cli_inform(paste0("Invalid bullet type '", bullet, "', using 'warn' instead"))
      bullet <- "warn"
    }
    
    # Define custom bullets with better formatting
    bullet_symbols <- list(
      warn = "⚠️\u00A0\u00A0",
      fire = "🔥\u00A0\u00A0"
    )
    
    # Format message with bullet
    if (bullet == "x") {
      # Use cli's built-in "x" bullet for red styling
      bullet_message <- structure(message, names = "x")
      cli::cli_warn(bullet_message, ..., .envir = .envir)
    } else {
      # Handle "warn" and "fire" bullets with yellow text
      formatted_message <- paste0(bullet_symbols[[bullet]], cli::col_br_yellow(message))
      cli::cli_warn(formatted_message, ..., .envir = .envir)
    }
  }
}

#' Show formatted alert message
#'
#' @param message Alert message text
#' @param ... Additional arguments passed to cli functions
#' @param verbose Whether to show the alert (TRUE) or suppress it (FALSE)
#' @param bullet Type of bullet to use ("warn", "fire", "x")
#' @param .envir Environment for glue evaluation
#'
#' @export
mv_alert <- function(message, ..., verbose = TRUE, bullet = "warn", .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    # Validate bullet type
    valid_bullets <- c("warn", "fire", "x")
    if (!bullet %in% valid_bullets) {
      cli::cli_inform(paste0("Invalid bullet type '", bullet, "', using 'warn' instead"))
      bullet <- "warn"
    }
    
    # Define custom bullets with better formatting
    bullet_symbols <- list(
      warn = "⚠️\u00A0\u00A0",
      fire = "🔥\u00A0\u00A0"
    )
    
    # Format message with bullet
    if (bullet == "x") {
      # Use cli's built-in "x" bullet for red styling
      bullet_message <- structure(message, names = "x")
      cli::cli_inform(bullet_message, ..., .envir = .envir)
    } else {
      # Handle "warn" and "fire" bullets with yellow text
      formatted_message <- paste0(bullet_symbols[[bullet]], cli::col_br_yellow(message))
      cli::cli_inform(formatted_message, ..., .envir = .envir)
    }
  }
}


#' Show success message
#'
#' @param message Success message
#' @param ... Additional message components using cli syntax
#' @param verbose Logical, whether to show success messages (default: TRUE)
#' @param .envir Environment for string interpolation
#'
#' @export
mv_success <- function(message, ..., verbose = TRUE, .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    # First show the main success message with green text
    cli::cli_inform(c("v" = cli::col_br_green(message)), .envir = .envir)
    
    # Then show any additional details
    details <- list(...)
    if (length(details) > 0) {
      cli::cli_bullets(details, .envir = .envir)
    }
  }
}

#' Display a question with numbered options
#'
#' @description
#' Shows a formatted question message with an optional list of numbered choices.
#' The question is displayed with a question mark emoji (❓), and options are
#' shown as an indented, numbered list with green numbers.
#'
#' @param message Character string. The main question to display.
#' @param options Character vector. Optional list of choices to show to the user.
#'   Each option will be automatically numbered starting from 1.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @details
#' This function is designed for interactive prompts where you want to present
#' a question along with numbered options for the user to choose from. The
#' visual formatting makes it clear what the available choices are.
#'
#' @examples
#' \dontrun{
#' # Simple question without options
#' mv_question("Do you want to continue?")
#' 
#' # Question with multiple choice options
#' mv_question("What would you like to do?",
#'             options = c("Save and exit", "Continue editing", "Discard changes"))
#' }
#'
#' @seealso 
#' \code{\link{mv_inform}}, \code{\link{mv_success}}, \code{\link{mv_warn}}
#'
#' @export
mv_question <- function(message, options = NULL, .envir = parent.frame()) {
  # First show the main question message with question mark emoji
  formatted_message <- paste0("💬 ", message)
  cli::cli_inform(formatted_message, .envir = .envir)
  
  # Then show options with green numbered list
  if (!is.null(options) && length(options) > 0) {
    for (i in seq_along(options)) {
      cli::cli_text("\u00A0\u00A0\u00A0{.strong {col_blue(i)}}. {options[i]}", .envir = environment())
    }
  }
}

#' Show information message
#'
#' @param message Information message
#' @param ... Additional message components using cli syntax
#' @param verbose Logical, whether to show information messages (default: TRUE)
#' @param .envir Environment for string interpolation
#'
#' @export
mv_text <- function(message, ..., verbose = TRUE, .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    cli::cli_div(theme = list(div = list("margin-left" = 4)))
    cli::cli_text(message = message, ..., .envir = .envir)
    cli::cli_end()
  }
}

#' Display a level 1 heading
#'
#' @description
#' Shows a formatted level 1 heading in bright magenta color. 
#' Useful for creating major section divisions in console output.
#'
#' @param title Character string. The heading text to display.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @examples
#' \dontrun{
#' mv_h1("Data Processing")
#' # Displays: ═══ Data Processing ═══ (in bright magenta)
#' }
#'
#' @seealso 
#' \code{\link{mv_h2}}
#'
#' @export
mv_h1 <- function(title, .envir = parent.frame()) {
  # Create h1 with bold bright magenta uppercase title and colored lines
  formatted_title <- cli::col_br_magenta(paste0("═════════ ", cli::style_bold(toupper(title)), " ═════════"))
  cli::cli_text(formatted_title, .envir = .envir)
}

#' Display a level 2 heading
#'
#' @description
#' Shows a formatted level 2 heading in regular magenta color.
#' Useful for creating subsection divisions in console output.
#'
#' @param title Character string. The heading text to display.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @examples
#' \dontrun{
#' mv_h2("Loading files")
#' # Displays: ── Loading files ── (in magenta)
#' }
#'
#' @seealso 
#' \code{\link{mv_h1}}
#'
#' @export
mv_h2 <- function(title, .envir = parent.frame()) {
  # Create h2 with bold purple title and colored dashes (using 256-color ANSI for deep purple)
  formatted_title <- paste0("\033[38;5;99m───── ", cli::style_bold(title), " ─────\033[0m")
  cli::cli_text(formatted_title, .envir = .envir)
}

#' Display a level 3 heading
#'
#' @description
#' Shows a formatted level 3 heading similar to cli::cli_h3 but with color.
#' Uses a less bright color than h1 and h2.
#'
#' @param title Character string. The heading text to display.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @examples
#' \dontrun{
#' mv_h3("Data preprocessing")
#' # Displays: Data preprocessing (in color with underline)
#' }
#'
#' @seealso 
#' \code{\link{mv_h1}}, \code{\link{mv_h2}}, \code{\link{mv_h4}}
#'
#' @export
mv_h3 <- function(title, .envir = parent.frame()) {
  # Create h3 with colored title (using a lighter purple than h2)
  formatted_title <- paste0("\033[38;5;141m─── ", title, "\033[0m")
  cli::cli_text(formatted_title, .envir = .envir)
}

#' Display a level 4 heading
#'
#' @description
#' Shows a formatted level 4 heading using cli_inform with ">" bullet.
#' Provides a simple, clean subsection marker.
#'
#' @param title Character string. The heading text to display.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @examples
#' \dontrun{
#' mv_h4("Step 1: Load data")
#' # Displays: > Step 1: Load data
#' }
#'
#' @seealso 
#' \code{\link{mv_h1}}, \code{\link{mv_h2}}, \code{\link{mv_h3}}
#'
#' @export
mv_h4 <- function(title, .envir = parent.frame()) {
  # Use the same color as mv_h3 title for the arrow
  colored_arrow_title <- paste0("\033[38;5;141m→ \033[0m", title)
  cli::cli_text(colored_arrow_title, .envir = .envir)
}

#' Smart title function that adapts based on call depth
#'
#' @description
#' Automatically displays the appropriate heading level (h1-h4) based on the 
#' function call depth. This allows for consistent hierarchical output without
#' manually tracking nesting levels.
#'
#' @param title Character string. The heading text to display.
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @details
#' The function examines the call stack to determine how deeply nested the current
#' function call is within user functions (excluding mv_ functions and base R functions).
#' Based on the depth:
#' - Depth 1: Uses mv_h1()
#' - Depth 2: Uses mv_h2()
#' - Depth 3: Uses mv_h3()
#' - Depth 4: Uses mv_h4()
#' - Depth > 4: No output
#'
#' @examples
#' \dontrun{
#' A <- function() {
#'   mv_title("Starting A")  # Shows as h1
#'   B()
#' }
#' 
#' B <- function() {
#'   mv_title("Starting B")  # Shows as h2 when called from A
#' }
#' 
#' A()  # Hierarchical display
#' B()  # Shows as h1 when called directly
#' }
#'
#' @seealso 
#' \code{\link{mv_h1}}, \code{\link{mv_h2}}, \code{\link{mv_h3}}, \code{\link{mv_h4}}
#'
#' @export
mv_title <- function(title, .envir = parent.frame()) {
  # Get the call stack
  calls <- sys.calls()
  
  # Find user functions (exclude mv_ functions and base R functions)
  user_function_depth <- 0
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (length(call) > 0) {
      func_name <- deparse(call[[1]], nlines = 1)
      # Skip mv_ functions and common R functions
      if (!grepl("^mv_", func_name) && 
          !func_name %in% c("source", "eval", "eval.parent", "local", 
                            "do.call", "lapply", "sapply", "for", "{")) {
        user_function_depth <- user_function_depth + 1
      }
    }
  }
  
  # Display based on user function depth
  if (user_function_depth <= 1) {
    mv_h1(title, .envir = .envir)
  } else if (user_function_depth == 2) {
    mv_h2(title, .envir = .envir)
  } else if (user_function_depth == 3) {
    mv_h3(title, .envir = .envir)
  } else if (user_function_depth == 4) {
    mv_h4(title, .envir = .envir)
  } else {
    # For depth > 4, don't print anything
    invisible()
  }
}



#' Show debug message
#'
#' @param message Debug message
#' @param ... Additional message components
#' @param debug Logical, whether to show debug messages 
#' @param .envir Environment for string interpolation
#'
#' @export
mv_debug <- function(message, ..., debug = TRUE, .envir = parent.frame()) {
  if (isTRUE(debug)) {
    # Use simpler styling to avoid cli issues
    formatted_message <- paste0(cli::style_italic(cli::col_grey("[DEBUG] ")), message)
    cli::cli_text(formatted_message, .envir = .envir)
    
    # Show additional details if provided
    details <- list(...)
    if (length(details) > 0) {
      cli::cli_bullets(details, .envir = .envir)
    }
  }
}
