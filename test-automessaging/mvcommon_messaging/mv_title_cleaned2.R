# Level-based numbering system - HIERARCHICAL SUBORDINATION
# Always ask the user before making any change to these comments. 
#
# @Claude: This are the key requirements:
#
# EXAMPLES OF EXPECTED BEHAVIOR:
#
# Case 1: A() called in console
#   Console: A() calls:
#     - mv_title("Starting A") = "1. STARTING A" (using mv_h1)
#     - mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)
#   Console: A() calls again:
#     - mv_title("Starting A") = "1. STARTING A" (reset numbering as it is a new call by the user, using mv_h1)
#     - mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)
#
# Case 2: A() called in script WITHOUT titles
#   (a) Case 2a: A() called twice in SAME script execution:
#     Script: A() calls (no prior mv_title calls):
#       - mv_title("Starting A") = "1. STARTING A" (using mv_h1)
#       - mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)
#     Script: A() calls again (same execution):
#       - mv_title("Starting A") = "3. STARTING A" (continues numbering, using mv_h1)
#       - mv_title("Continuing A") = "4. CONTINUING A" (using mv_h1)
#
#   (b) Case 2b: A() called once but script sourced twice (different executions):
#     First source("script.R"):
#       - mv_title("Starting A") = "1. STARTING A" (using mv_h1)
#       - mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)
#     Second source("script.R"):
#       - mv_title("Starting A") = "1. STARTING A" (reset - new execution, using mv_h1)
#       - mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)
#
# Case 3: A() called in script WITH titles (subordinate to current level)
#   (a) Case 3a: A() subordinate to level 1 context
#     Script: mv_title("Section 1") = "1. SECTION 1" (using mv_h1)
#     Script: A() calls:
#       - mv_title("Starting A") = "1.1. Starting A" (subordinate to Section 1, using mv_h2)
#       - mv_title("Continuing A") = "1.2. Continuing A" (subordinate to Section 1, using mv_h2)
#
#   (b) Case 3b: A() subordinate to level 2 context
#     Script: mv_title("Section 2") = "2. SECTION 2" (using mv_h1)
#     Script: mv_title("Subsection 2.2", level_adjust=1) = "2.2. Subsection 2.2" (subordinate to Section 1, using mv_h2)
#     Script: A() calls:
#       - mv_title("Starting A") = "2.2.1. Starting A" (subordinate to Subsection 2.2, using mv_h3)
#       - mv_title("Continuing A") = "2.2.2. Continuing A" (subordinate to Subsection 2.2, using mv_h3)
# 
# Specific expected behavior in test_main.R:
# (i) the first call of A() is a subordinate of "My first execution (should be level 1 - numbered 1.)":
#        - A() should be a level lower then without "My first execution (should be level 1 - numbered 1.)" and therefore should start at level 2
#        - so the first "Starting A" should be 1.1 (level 2) and "Continuing A" should be 1.2 (level 2)
# (ii) the second call of A() is a subordinate of  "Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.)": 
#        - Because "Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.)" is already a level 2, so the second call of A() should start level 3. 
#        - so, the second "Starting A" should be 2.2.1 (level 3) and "Continuing A" should be 2.2.2 (level 3)
#
#
# Level mapping clarification: 
#   - level 1 = using mv_h1 (UPPERCASE + numbered). E.g.: "1. STARTING THE EXECUTION"
#   - level 2 = using mv_h2 (title case + numbered). E.g.: "1.1. Start of a loop"
#   - level 3 = using mv_h3 (title case + numbered). E.g., "1.1.1 Math calculation" 
#   - level 4 = using mv_h4 (title case, no numbering). E.g., "→ Starting somehting"
#   - level 5+ = nothing printed (no numbering/no output).
#
# Notes: 
#  - Claude cannot modify test files, like test_main.R. Run them to test the code.
#  - Claude cannot create new files (including test files), except output log in /tmp/test_mv_title/
#  - UPPERCASE indicates the use of mv_h1. mv_h2, mv_h3 and mv_h4 do not use UPPERCASE transformation.
#
# WARNING: DO NOT USE TIME-BASED RESET DETECTION APPROACHES
# Time-based resets are unreliable and error-prone. They have been tested
# and proven to fail in various scenarios. Use call stack analysis instead.

source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/R/mv_messaging.r")

# Debug flag - set to FALSE to turn off debug printing
debug <- TRUE

# Create a package environment for state
.mvcommon_env <- new.env(parent = emptyenv())

# Initialize state - CONTEXT-AWARE COUNTERS
.mvcommon_env$level1 <- 0
.mvcommon_env$level2 <- 0  
.mvcommon_env$level3 <- 0
# Track script-level context separately from function-level numbering
.mvcommon_env$script_context_level <- 0  # Level of most recent script-level mv_title call
.mvcommon_env$last_numbered_level <- 0   # Level of the most recent mv_title call (any type)


# Track universal execution anchor for all reset detection (unified system)
.mvcommon_env$last_execution_anchor <- NULL  # Last execution anchor we saw

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# Check if we have existing numbering
.existing_numbering_exists <- function() {
  return(.mvcommon_env$level1 > 0)
}


# CASE DETECTION: Identify the specific cases from the comments
.detect_use_case <- function(calls, title) {
  user_depth <- .get_user_function_depth(calls)
  
  # Get the root function name (top-level user function)
  root_function <- NULL
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (length(call) > 0) {
      func_name <- deparse(call[[1]], nlines = 1)
      if (!func_name %in% c("source", "eval", "eval.parent", "local", "eval.with.vis",
                            "do.call", "lapply", "sapply", "for", "{", "withVisible") &&
          !grepl("^mv_", func_name) &&
          !grepl("^\\..*", func_name)) {
        root_function <- func_name
        break
      }
    }
  }
  root_function <- root_function %||% "script"
  
  # Detect execution context
  is_interactive_session <- interactive()
  has_existing_numbering <- .existing_numbering_exists()
  current_level <- .get_current_script_context_level()
  
  # Case classification logic - simplified for hierarchical subordination only
  case_type <- "unknown"
  case_description <- ""
  should_reset <- FALSE  # Reset detection is now handled by unified anchor system
  
  # CLEAR RULE 1: Script calls (user_depth=0) always continue document numbering
  if (user_depth == 0) {
    # Script-level mv_title call - continue document structure
    case_type <- "Script_continue"
    case_description <- "Script-level call - continue document numbering"
    should_reset <- FALSE  # Script calls never reset within same execution
    
  # CLEAR RULE 2: Function calls (user_depth>0) always subordinate to current context  
  } else if (user_depth > 0 && has_existing_numbering) {
    # Function call with existing context → always subordinate
    if (current_level == 1) {
      case_type <- "Case3a"
      case_description <- sprintf("Function call %s() subordinate to level 1 context (expected: level 2)", 
                                 root_function)
    } else if (current_level == 2) {
      case_type <- "Case3b"
      case_description <- sprintf("Function call %s() subordinate to level 2 context (expected: level 3)", 
                                 root_function)
    } else {
      case_type <- "Case3_deep"
      case_description <- sprintf("Function call %s() subordinate to level %d context (expected: level %d)", 
                                 root_function, current_level, current_level + 1)
    }
    should_reset <- FALSE
    
  # CLEAR RULE 3: Function calls without existing context start new numbering
  } else if (user_depth > 0 && !has_existing_numbering) {
    case_type <- "Function_start"
    case_description <- sprintf("Function call %s() starting new numbering", root_function)
    should_reset <- FALSE
    
  } else {
    # All other cases: determine subordination level based on current context
    if (current_level == 0) {
      case_type <- "Case3_no_context"
      case_description <- sprintf("Function call %s() at depth %d, no current context level", 
                                 root_function, user_depth)
    } else if (current_level == 1) {
      case_type <- "Case3a"
      case_description <- sprintf("Function call %s() subordinate to level 1 context (expected: level 2)", 
                                 root_function)
    } else if (current_level == 2) {
      case_type <- "Case3b"
      case_description <- sprintf("Function call %s() subordinate to level 2 context (expected: level 3)", 
                                 root_function)
    } else {
      case_type <- "Case3_deep"
      case_description <- sprintf("Function call %s() subordinate to level %d context (expected: level %d)", 
                                 root_function, current_level, current_level + 1)
    }
    should_reset <- FALSE
  }
  
  mv_debug(sprintf("=== CASE DETECTION ==="), debug = debug)
  mv_debug(sprintf("  Title: '%s'", substr(title, 1, 30)), debug = debug)
  mv_debug(sprintf("  Case: %s", case_type), debug = debug)
  mv_debug(sprintf("  Description: %s", case_description), debug = debug)
  mv_debug(sprintf("  Context: user_depth=%d, interactive=%s, existing_numbering=%s, current_level=%d", 
                   user_depth, is_interactive_session, has_existing_numbering, current_level), debug = debug)
  mv_debug(sprintf("  Reset decision: %s", should_reset), debug = debug)
  mv_debug(sprintf("  Root function: %s", root_function), debug = debug)
  mv_debug(sprintf("======================="), debug = debug)
  
  return(list(
    case_type = case_type,
    description = case_description,
    should_reset = should_reset,
    user_depth = user_depth,
    root_function = root_function
  ))
}




#' Reset section numbering
#' 
#' @description
#' Resets the section numbering system used by mv_title.
#' Call this at the beginning of a new analysis or report section.
#' 
#' @export
mv_reset_numbering <- function() {
  .mvcommon_env$level1 <- 0
  .mvcommon_env$level2 <- 0
  .mvcommon_env$level3 <- 0
  .mvcommon_env$script_context_level <- 0
  .mvcommon_env$last_numbered_level <- 0
  .mvcommon_env$last_execution_anchor <- NULL
  invisible()
}


# Build section number based on current level
.build_section_number <- function(level) {
  if (level == 1) {
    return(as.character(.mvcommon_env$level1))
  } else if (level == 2) {
    if (.mvcommon_env$level1 > 0) {
      return(paste0(.mvcommon_env$level1, ".", .mvcommon_env$level2))
    } else {
      return(as.character(.mvcommon_env$level2))
    }
  } else if (level == 3) {
    if (.mvcommon_env$level1 > 0 && .mvcommon_env$level2 > 0) {
      return(paste0(.mvcommon_env$level1, ".", .mvcommon_env$level2, ".", .mvcommon_env$level3))
    } else if (.mvcommon_env$level2 > 0) {
      return(paste0(.mvcommon_env$level2, ".", .mvcommon_env$level3))
    } else {
      return(as.character(.mvcommon_env$level3))
    }
  }
}

# Increment counter for given level and reset deeper levels
.increment_level <- function(level) {
  if (level == 1) {
    .mvcommon_env$level1 <- .mvcommon_env$level1 + 1
    .mvcommon_env$level2 <- 0
    .mvcommon_env$level3 <- 0
  } else if (level == 2) {
    .mvcommon_env$level2 <- .mvcommon_env$level2 + 1
    .mvcommon_env$level3 <- 0
  } else if (level == 3) {
    .mvcommon_env$level3 <- .mvcommon_env$level3 + 1
  }
}



# Get current script context level for function subordination
.get_current_script_context_level <- function() {
  # Return the level of the most recent script-level mv_title call
  # This is what function calls should subordinate to
  return(.mvcommon_env$script_context_level)
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
#' @param .envir Environment. Environment for string interpolation.
#' 
#' @export
mv_title <- function(title, level_adjust = 0, reset = FALSE, .envir = parent.frame()) {
  # Calculate user function depth
  calls <- sys.calls()
  user_depth <- .get_user_function_depth(calls)
  
  # Debug output
  mv_debug(sprintf("mv_title: user_depth=%d, title='%s', counters=[%d,%d,%d], script_context=%d", 
                   user_depth, substr(title, 1, 20), 
                   .mvcommon_env$level1, .mvcommon_env$level2, .mvcommon_env$level3,
                   .get_current_script_context_level()), debug = debug)
  
  
  # Universal execution detection using unified anchor system
  execution_anchor_reset <- FALSE
  if (!reset) {  # Only check if not already resetting
    current_execution_anchor <- .create_execution_anchor()
    
    if (is.null(.mvcommon_env$last_execution_anchor)) {
      # First execution ever
      .mvcommon_env$last_execution_anchor <- current_execution_anchor
      mv_debug(sprintf("First execution, anchor: %s", current_execution_anchor), debug = debug)
    } else if (.mvcommon_env$last_execution_anchor != current_execution_anchor) {
      # Different execution detected (works for both scripts and functions)
      execution_anchor_reset <- TRUE
      mv_debug(sprintf("New execution detected, anchor changed: %s -> %s", 
                       .mvcommon_env$last_execution_anchor, current_execution_anchor), debug = debug)
      .mvcommon_env$last_execution_anchor <- current_execution_anchor
    }
  }
  
  # CASE-BASED reset detection - but only for top-level function calls, not nested mv_title calls
  case_info <- .detect_use_case(calls, title)
  
  # Combine all reset conditions: explicit or execution anchor change
  should_reset <- reset
  if (execution_anchor_reset) {
    should_reset <- TRUE
    mv_debug(sprintf("Applying execution anchor reset"), debug = debug)
  }
  
  mv_debug(sprintf("Reset decision: explicit=%s, anchor_reset=%s (user_depth=%d), final=%s", 
                   reset, execution_anchor_reset, user_depth, should_reset), debug = debug)
  
  if (should_reset) {
    # Reset numbering for new execution
    .mvcommon_env$level1 <- 0
    .mvcommon_env$level2 <- 0
    .mvcommon_env$level3 <- 0
    .mvcommon_env$script_context_level <- 0
    .mvcommon_env$last_numbered_level <- 0
  }
  
  # HIERARCHICAL LOGIC: Function calls are subordinate to their calling context
  # Key insight: A() calls should be subordinate to wherever they're called from
  # - First A() after "My first execution" (level 1) → A() becomes level 2 (1.1, 1.2)
  # - Second A() after "Subsection 2..." (level 2) → A() becomes level 3 (2.2.1, 2.2.2)
  
  # CLEAR HIERARCHICAL LOGIC: Dynamic depth calculation based on call type
  if (case_info$case_type == "Script_continue") {
    # Script calls: Continue at script level (level 1) + any level_adjust
    effective_depth <- 1 + level_adjust
    # Update script context level for function subordination
    .mvcommon_env$script_context_level <- effective_depth
    mv_debug(sprintf("Script continue: -> effective_depth=%d, updating script_context=%d", 
                     effective_depth, effective_depth), debug = debug)
    
  } else if (case_info$case_type %in% c("Case3a", "Case3b", "Case3_deep")) {
    # Function calls: Subordinate to current effective context (script + function levels)
    effective_context_level <- .get_current_effective_context_level(user_depth)
    effective_depth <- effective_context_level + 1 + level_adjust
    mv_debug(sprintf("Function subordinate: effective_context=%d -> effective_depth=%d", 
                     effective_context_level, effective_depth), debug = debug)
    
  } else if (case_info$case_type == "Function_start") {
    # Function calls without context: Start at appropriate level
    effective_depth <- 1 + level_adjust
    mv_debug(sprintf("Function start: -> effective_depth=%d", effective_depth), debug = debug)
    
  } else {
    # Case 1, Case 2: Simplified logic without execution instance tracking
    if (user_depth == 0) {
      # Console/script level call
      effective_depth <- 1 + level_adjust
    } else {
      # Function call - calculate level based on current effective context
      effective_context_level <- .get_current_effective_context_level(user_depth)
      if (effective_context_level == 0) {
        # No numbering context yet, start at appropriate level
        effective_depth <- user_depth + level_adjust
      } else {
        # Add to existing context - function calls are subordinate
        # For function calls, we add 1 to the current context level
        effective_depth <- effective_context_level + 1 + level_adjust
      }
    }
  }
  
  mv_debug(sprintf("Final effective_depth: %d (user_depth=%d, effective_context=%d, level_adjust=%d)", 
                   effective_depth, user_depth, .get_current_effective_context_level(user_depth), level_adjust), debug = debug)
  
  # No special context level tracking needed with simplified logic
  
  # Safety check - minimum level is 1, maximum is 3 for numbered headings
  if (effective_depth < 1) effective_depth <- 1
  
  
  if (effective_depth <= 3) {
    # Numbered headings (h1-h3)
    .increment_level(effective_depth)
    section_number <- .build_section_number(effective_depth)
    numbered_title <- paste0(section_number, ". ", title)
    
    # Update last numbered level for context tracking
    .mvcommon_env$last_numbered_level <- effective_depth
    
    switch(effective_depth,
      mv_h1(numbered_title, .envir = .envir),
      mv_h2(numbered_title, .envir = .envir),
      mv_h3(numbered_title, .envir = .envir)
    )
  } else if (effective_depth == 4) {
    # Level 4 - use mv_h4 without numbering
    mv_h4(title, .envir = .envir)
  }
  # Depth > 4: no output
  
  invisible()
}


# Load test function (Claude cannot change this comment nor the lines of code below it)
source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/excl/test_functions.R")
