# ============================================================================
# SECTION 0: OPTIONS
# ============================================================================
# Debug and auto-numbering controls are now in .mvcommon_env (Section 2)

# ============================================================================
# SECTION 1: REQUIREMENTS
# ============================================================================
# See requirements.md for full specification

# ============================================================================
# SECTION 2: onLoad - Package initialization elements
# ============================================================================
# Elements that need to be incorporated into package onLoad:

# Create a package environment for state
.mvcommon_env <- new.env(parent = emptyenv())

# Initialize state - CONTEXT-AWARE COUNTERS
.mvcommon_env$levels <- c(0, 0, 0)  # levels[1], levels[2], levels[3]
# Track script-level context separately from function-level numbering
.mvcommon_env$script_context_level <- 0  # Level of most recent script-level mv_title call
# Indentation configuration for mv_text
.mvcommon_env$indentation_size <- 3  # Base indentation size
# Debug control
.mvcommon_env$debug <- TRUE  # Global debug flag
# Auto-numbering control: "all", "title", or "none"
.mvcommon_env$auto_number <- "all"  # Default: all functions auto-number

# Track universal execution anchor for all reset detection (unified system)
.mvcommon_env$last_execution_anchor <- NULL  # Last execution anchor we saw

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper function to determine if a function should auto-number based on global setting
.should_auto_number <- function(function_type) {
  setting <- .mvcommon_env$auto_number
  if (setting == "all") return(TRUE)
  if (setting == "title" && function_type == "mv_title") return(TRUE) 
  if (setting == "none") return(FALSE)
  return(FALSE)  # Default fallback
}

# Get current section depth for indentation calculation
.get_current_section_depth <- function() {
  # Count how many levels are currently active (non-zero)
  active_levels <- sum(.mvcommon_env$levels > 0)
  return(active_levels)
}

# Shared reset detection logic
.handle_reset_detection <- function(reset = FALSE) {
  execution_anchor_reset <- FALSE
  if (!reset) {  # Only check if not already resetting
    current_execution_anchor <- .create_execution_anchor()
    
    if (is.null(.mvcommon_env$last_execution_anchor)) {
      # First execution ever
      .mvcommon_env$last_execution_anchor <- current_execution_anchor
    } else if (.mvcommon_env$last_execution_anchor != current_execution_anchor) {
      # Different execution detected (works for both scripts and functions)
      execution_anchor_reset <- TRUE
      .mvcommon_env$last_execution_anchor <- current_execution_anchor
    }
  }
  
  # Combine all reset conditions: explicit or execution anchor change
  should_reset <- reset || execution_anchor_reset
  
  if (should_reset) {
    # Reset numbering for new execution
    .mvcommon_env$levels <- c(0, 0, 0)
    .mvcommon_env$script_context_level <- 0
  }
  
  return(should_reset)
}

# Shared level counter update logic
.update_level_counters <- function(effective_depth) {
  # Increment counter for given level and reset deeper levels
  .mvcommon_env$levels[effective_depth] <- .mvcommon_env$levels[effective_depth] + 1
  # Reset deeper levels
  if (effective_depth < 3) {
    .mvcommon_env$levels[(effective_depth + 1):3] <- 0
  }
}

# Shared section number building logic
.build_section_number <- function(effective_depth) {
  if (effective_depth == 1) {
    section_number <- as.character(.mvcommon_env$levels[1])
  } else if (effective_depth == 2) {
    if (.mvcommon_env$levels[1] > 0) {
      section_number <- paste0(.mvcommon_env$levels[1], ".", .mvcommon_env$levels[2])
    } else {
      section_number <- as.character(.mvcommon_env$levels[2])
    }
  } else if (effective_depth == 3) {
    if (.mvcommon_env$levels[1] > 0 && .mvcommon_env$levels[2] > 0) {
      section_number <- paste0(.mvcommon_env$levels[1], ".", .mvcommon_env$levels[2], ".", .mvcommon_env$levels[3])
    } else if (.mvcommon_env$levels[2] > 0) {
      section_number <- paste0(.mvcommon_env$levels[2], ".", .mvcommon_env$levels[3])
    } else {
      section_number <- as.character(.mvcommon_env$levels[3])
    }
  }
  return(section_number)
}

# Unified formatting function for all heading levels
.format_heading <- function(title, level, .envir = parent.frame()) {
  if (level == 1) {
    # Level 1: bright magenta with double lines, uppercase
    formatted_title <- cli::col_br_magenta(paste0("═════════ ", cli::style_bold(toupper(title)), " ═════════"))
    cli::cli_text(formatted_title, .envir = .envir)
  } else if (level == 2) {
    # Level 2: purple with dashes, title case
    formatted_title <- paste0("\033[38;5;99m───── ", cli::style_bold(title), " ─────\033[0m")
    cli::cli_text(formatted_title, .envir = .envir)
  } else if (level == 3) {
    # Level 3: lighter purple with short dashes
    formatted_title <- paste0("\033[38;5;141m─── ", title, "\033[0m")
    cli::cli_text(formatted_title, .envir = .envir)
  } else if (level == 4) {
    # Level 4: arrow with no numbering
    colored_arrow_title <- paste0("\033[38;5;141m→ \033[0m", title)
    cli::cli_text(colored_arrow_title, .envir = .envir)
  }
}

# Unified numbered heading function - consolidates all numbering logic
.mv_heading_numbered <- function(title, level, reset = FALSE, .envir = parent.frame()) {
  # Handle reset detection using shared logic
  should_reset <- .handle_reset_detection(reset)
  
  # Update script context level for function subordination
  .mvcommon_env$script_context_level <- level
  
  # Update level counters using shared logic
  .update_level_counters(level)
  
  # Build section number using shared logic
  section_number <- .build_section_number(level)
  
  # Create numbered title
  numbered_title <- paste0(section_number, ". ", title)
  
  # Format and display using unified formatter
  .format_heading(numbered_title, level, .envir = .envir)
}

# Internal function for mv_title - handles numbering with pre-calculated context
.mv_title_with_depth <- function(title, effective_depth, script_context_level, reset_applied = FALSE, .envir = parent.frame()) {
  # Update script context level (this was calculated by mv_title)
  .mvcommon_env$script_context_level <- script_context_level
  
  if (effective_depth <= 3) {
    # Update level counters using shared logic
    .update_level_counters(effective_depth)
    
    # Build section number using shared logic
    section_number <- .build_section_number(effective_depth)
    
    # Create numbered title
    numbered_title <- paste0(section_number, ". ", title)
    
    # Format and display using unified formatter
    .format_heading(numbered_title, effective_depth, .envir = .envir)
  } else if (effective_depth == 4) {
    # Level 4 - use formatter directly 
    .format_heading(title, level = 4, .envir = .envir)
  }
  # Depth > 4: no output
}

# ============================================================================
# SECTION 3: SPECIFIC FUNCTIONS
# ============================================================================
# Messaging functions (copied from R/mv_messaging.r)

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

#' Show information message with context-aware indentation
#'
#' @param message Information message
#' @param ... Additional message components using cli syntax
#' @param verbose Logical, whether to show information messages (default: TRUE)
#' @param .envir Environment for string interpolation
#'
#' @export
mv_text <- function(message, ..., verbose = TRUE, .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    # Calculate indentation based on current section depth
    # No sections (0) = 0 indent, Section 1 (1) = 3 indent, Section 1.1 (2) = 6 indent, etc.
    # Level 4+ capped at 12 indent
    current_depth <- .get_current_section_depth()
    indentation <- min(current_depth * .mvcommon_env$indentation_size, 12)
    
    cli::cli_div(theme = list(div = list("margin-left" = indentation)))
    cli::cli_text(message = message, ..., .envir = .envir)
    cli::cli_end()
  }
}

#' Display a level 1 heading with automatic numbering
#'
#' @description
#' Shows a formatted level 1 heading in bright magenta color with automatic section numbering.
#' Participates in the hierarchical numbering system.
#'
#' @param title Character string. The heading text to display.
#' @param auto_number Logical. Whether to automatically number this heading (default: TRUE).
#' @param reset Logical. If TRUE, reset numbering before displaying this title (default: FALSE).
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @export
mv_h1 <- function(title, auto_number = TRUE, reset = FALSE, .envir = parent.frame()) {
  # Combine individual parameter with global setting
  effective_auto_number <- auto_number && .should_auto_number("mv_h")
  
  if (effective_auto_number) {
    .mv_heading_numbered(title, level = 1, reset = reset, .envir = .envir)
  } else {
    .format_heading(title, level = 1, .envir = .envir)
  }
}

#' Display a level 2 heading with automatic numbering
#'
#' @description
#' Shows a formatted level 2 heading in regular magenta color with automatic section numbering.
#' Participates in the hierarchical numbering system.
#'
#' @param title Character string. The heading text to display.
#' @param auto_number Logical. Whether to automatically number this heading (default: TRUE).
#' @param reset Logical. If TRUE, reset numbering before displaying this title (default: FALSE).
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @export
mv_h2 <- function(title, auto_number = TRUE, reset = FALSE, .envir = parent.frame()) {
  # Combine individual parameter with global setting
  effective_auto_number <- auto_number && .should_auto_number("mv_h")
  
  if (effective_auto_number) {
    .mv_heading_numbered(title, level = 2, reset = reset, .envir = .envir)
  } else {
    .format_heading(title, level = 2, .envir = .envir)
  }
}

#' Display a level 3 heading with automatic numbering
#'
#' @description
#' Shows a formatted level 3 heading with color and automatic section numbering.
#' Participates in the hierarchical numbering system.
#'
#' @param title Character string. The heading text to display.
#' @param auto_number Logical. Whether to automatically number this heading (default: TRUE).
#' @param reset Logical. If TRUE, reset numbering before displaying this title (default: FALSE).
#' @param .envir Environment. Environment for string interpolation in cli functions.
#'   Default is parent.frame().
#'
#' @export
mv_h3 <- function(title, auto_number = TRUE, reset = FALSE, .envir = parent.frame()) {
  # Combine individual parameter with global setting
  effective_auto_number <- auto_number && .should_auto_number("mv_h")
  
  if (effective_auto_number) {
    .mv_heading_numbered(title, level = 3, reset = reset, .envir = .envir)
  } else {
    .format_heading(title, level = 3, .envir = .envir)
  }
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
#' @export
mv_h4 <- function(title, .envir = parent.frame()) {
  .format_heading(title, level = 4, .envir = .envir)
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

# ============================================================================
# SECTION 4: mv_title - Main functionality
# ============================================================================

# Create universal execution anchor for all reset detection (unified system)
.create_execution_anchor <- function() {
  calls <- sys.calls()
  
  if (length(calls) > 0) {
    # We have a call stack - use environment-based anchor
    top_env_str <- capture.output(print(sys.frame(1)))[1]
    env_hex <- gsub(".*0x([0-9a-f]+).*", "\\1", top_env_str)
    return(paste0("EXEC_", env_hex))
  } else {
    # No call stack (Rscript, R --file, etc.) - use timestamp
    return(paste0("DIRECT_", round(as.numeric(Sys.time()) * 1000000)))
  }
}

#' Reset section numbering
#' 
#' @description
#' Resets the section numbering system used by mv_title.
#' Call this at the beginning of a new analysis or report section.
#' 
#' @export
mv_reset_numbering <- function() {
  .mvcommon_env$levels <- c(0, 0, 0)
  .mvcommon_env$script_context_level <- 0
  .mvcommon_env$last_execution_anchor <- NULL
  invisible()
}

# Get current effective context level (script + function context)
.get_current_effective_context_level <- function(user_depth) {
  # For functions, determine effective context based on call depth
  # Shallow calls (like "Continuing A") should use the level they started at
  
  if (user_depth <= 1) {
    # Shallow function calls - use script context, not deep function context
    # This prevents "Continuing A" from being subordinate to deeply nested B() calls
    script_level <- .mvcommon_env$script_context_level
    return(script_level)
  } else {
    # Deep function calls - calculate based on user_depth for proper hierarchical nesting
    # user_depth=2 → context should be at least 2 (from shallow calls)
    # user_depth=3 → context should be at least 3 
    # user_depth=4 → context should be at least 4
    # This ensures E() (user_depth=5) gets effective_depth=5 (no output)
    script_level <- .mvcommon_env$script_context_level
    min_context_for_depth <- script_level + user_depth - 1
    return(min_context_for_depth)
  }
}

# Calculate user function depth - FIXED VERSION
.get_user_function_depth <- function(calls) {
  excluded_functions <- c("source", "eval", "eval.parent", "local", "eval.with.vis",
                         "do.call", "lapply", "sapply", "for", "{", "withVisible",
                         "%||%", ".build_section_number", ".GlobalEnv")
  
  depth <- 0
  seen_functions <- character()
  
  for (i in seq_along(calls)) {
    if (length(calls[[i]]) > 0) {
      func_name <- deparse(calls[[i]][[1]], nlines = 1)
      is_mv <- grepl("^mv_", func_name)
      is_excluded <- func_name %in% excluded_functions
      
      if (!is_mv && !is_excluded) {
        # Only count unique functions to avoid double-counting recursive calls
        if (!func_name %in% seen_functions) {
          depth <- depth + 1
          seen_functions <- c(seen_functions, func_name)
        }
      }
    }
  }
  return(depth)
}

#' Smart title function with automatic numbering
#' 
#' @param title Character string. The heading text to display.
#' @param level_adjust Integer. Number of levels to add to the calculated depth (default: 0).
#' @param reset Logical. If TRUE, reset numbering before displaying this title (default: FALSE).
#' @param auto_number Logical. Whether to automatically number this heading (default: TRUE).
#' @param .envir Environment. Environment for string interpolation.
#' 
#' @export
mv_title <- function(title, level_adjust = 0, reset = FALSE, auto_number = TRUE, .envir = parent.frame()) {
  # Calculate user function depth
  calls <- sys.calls()
  user_depth <- .get_user_function_depth(calls)
  
  # Debug output
  mv_debug(sprintf("mv_title: user_depth=%d, title='%s', counters=[%d,%d,%d]", 
                   user_depth, substr(title, 1, 20), 
                   .mvcommon_env$levels[1], .mvcommon_env$levels[2], .mvcommon_env$levels[3]), debug = .mvcommon_env$debug)
  
  # Handle reset detection using shared logic
  should_reset <- .handle_reset_detection(reset)
  
  # HIERARCHICAL LOGIC: Unified depth calculation
  # Key insight: A() calls should be subordinate to wherever they're called from
  
  has_existing_numbering <- (.mvcommon_env$levels[1] > 0)
  
  # Unified calculation: determine base depth, then add level_adjust
  if (user_depth == 0) {
    # Script-level mv_title call - continue document structure at level 1
    base_depth <- 1
    script_context_level <- base_depth + level_adjust
  } else if (has_existing_numbering) {
    # Function call with existing context → subordinate to current context
    effective_context_level <- .get_current_effective_context_level(user_depth)
    base_depth <- effective_context_level + 1
    script_context_level <- .mvcommon_env$script_context_level  # Keep existing
  } else {
    # Function calls without context: Start at level 1
    base_depth <- 1
    script_context_level <- .mvcommon_env$script_context_level  # FIXED: Keep existing, don't override
  }
  
  effective_depth <- base_depth + level_adjust
  
  # Safety check - minimum level is 1
  if (effective_depth < 1) effective_depth <- 1
  
  # Combine individual parameter with global setting
  effective_auto_number <- auto_number && .should_auto_number("mv_title")
  
  if (effective_auto_number) {
    # Delegate to internal function for numbered titles
    .mv_title_with_depth(title, effective_depth, script_context_level, reset_applied = should_reset, .envir = .envir)
  } else {
    # Use formatter directly for non-numbered titles
    if (effective_depth <= 4) {
      .format_heading(title, level = effective_depth, .envir = .envir)
    }
    # Depth > 4: no output
  }
  
  invisible()
}


# ============================================================================
# SECTION 5: Load test functions
# ============================================================================

# Load test function (Claude cannot change this comment nor the lines of code below it)
source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/excl/test_functions.R")
