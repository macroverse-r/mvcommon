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
.mvcommon_env$function_context_stack <- list()  # Stack of function context levels

# EXECUTION RUN TRACKING - Focus on individual function/script runs
.mvcommon_env$current_execution_run <- NULL  # Current execution run info
.mvcommon_env$execution_run_counter <- 0     # Simple counter for unique run IDs
.mvcommon_env$has_deep_execution <- FALSE    # Track if we've had deep calls in current run
.mvcommon_env$last_function_call <- NULL     # Track last top-level function called
.mvcommon_env$function_call_count <- 0       # Count calls to same function in console
.mvcommon_env$console_session_id <- 0        # Track console session changes

# Track execution instances for consistent numbering within same function call
.mvcommon_env$execution_instances <- list()  # Maps execution_id to assigned level
.mvcommon_env$execution_counter <- 0  # Counter for unique execution IDs
.mvcommon_env$last_execution_id <- NULL  # Track the last execution ID we saw

# Track script execution anchors for script-level reset detection
.mvcommon_env$current_script_anchor <- NULL  # Current script execution anchor

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Create script execution anchor for script-level reset detection
.create_script_execution_anchor <- function() {
  calls <- sys.calls()
  
  if (length(calls) > 0) {
    # We have a call stack - use environment-based anchor
    top_env_str <- capture.output(print(sys.frame(1)))[1]
    env_hex <- gsub(".*0x([0-9a-f]+).*", "\\1", top_env_str)
    return(paste0("SCRIPT_", env_hex))
  } else {
    # No call stack (Rscript, R --file, etc.) - use timestamp
    return(paste0("DIRECT_", round(as.numeric(Sys.time()) * 1000000)))
  }
}

# Check if we have existing numbering
.existing_numbering_exists <- function() {
  return(.mvcommon_env$level1 > 0)
}

# Get or create execution ID for the current function instance
.get_or_create_execution_id <- function(calls, root_function) {
  # Don't track execution IDs for script-level calls
  if (root_function == "script") {
    return(NULL)
  }
  
  # Find the frame of the root function in the call stack
  for (i in seq_along(calls)) {
    if (length(calls[[i]]) > 0) {
      func_name <- deparse(calls[[i]][[1]], nlines = 1)
      if (func_name == root_function) {
        # Get the function's environment
        func_frame <- sys.frame(i)
        
        # Check if execution ID already exists in this function instance
        if (!exists(".mv_execution_id", envir = func_frame, inherits = FALSE)) {
          # First mv_title call in this function execution - create new ID
          exec_id <- paste0(root_function, "_frame", i, "_", round(as.numeric(Sys.time()) * 1000000))
          assign(".mv_execution_id", exec_id, envir = func_frame)
          mv_debug(sprintf("Created new execution ID: %s", exec_id), debug = debug)
          return(list(id = exec_id, is_new = TRUE))
        } else {
          # Subsequent mv_title calls in same function execution - reuse ID
          exec_id <- get(".mv_execution_id", envir = func_frame, inherits = FALSE)
          return(list(id = exec_id, is_new = FALSE))
        }
      }
    }
  }
  
  # Couldn't find the function in the stack (shouldn't happen)
  return(NULL)
}

# Detect if we're in a new execution run (not just continuing the same one)
.detect_execution_run <- function(calls) {
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
  
  # Create execution run signature
  execution_signature <- list(
    root_function = root_function %||% "script",
    user_depth = user_depth,
    call_stack_depth = length(calls),
    is_top_level = user_depth <= 1
  )
  
  mv_debug(sprintf("Execution signature: %s (depth=%d, stack=%d, top_level=%s)", 
                   execution_signature$root_function,
                   execution_signature$user_depth,
                   execution_signature$call_stack_depth,
                   execution_signature$is_top_level), debug = debug)
  
  return(execution_signature)
}

# CASE DETECTION: Identify the specific cases from the comments
.detect_use_case <- function(calls, title) {
  current_sig <- .detect_execution_run(calls)
  user_depth <- .get_user_function_depth(calls)
  
  # Detect execution context
  is_interactive_session <- interactive()
  has_existing_numbering <- .existing_numbering_exists()
  current_level <- .get_current_script_context_level()
  
  # Track function calls for console detection
  root_function <- current_sig$root_function %||% "script"
  
  # Case classification logic
  case_type <- "unknown"
  case_description <- ""
  should_reset <- FALSE
  
  # Check for top-level execution characteristics
  is_likely_console_call <- FALSE
  is_separate_execution <- FALSE
  
  # Look for indicators of console vs script execution
  # Console calls tend to have simpler call stacks
  if (user_depth <= 1 && length(calls) <= 6) {
    # Check if this looks like a direct console call pattern
    is_likely_console_call <- TRUE
    for (i in seq_along(calls)) {
      if (length(calls[[i]]) > 0) {
        func_name <- deparse(calls[[i]][[1]], nlines = 1)
        # If we see source(), eval(), or similar, it's likely script execution
        if (func_name %in% c("source", "eval", "eval.parent", "local", "eval.with.vis")) {
          is_likely_console_call <- FALSE
          break
        }
      }
    }
  }
  
  # NEW EXECUTION DETECTION: Use execution ID tracking (CONSOLE ONLY)
  # Only apply console reset detection for interactive sessions or direct calls
  # NOT for script executions where multiple function calls should continue numbering
  execution_info <- NULL
  if (user_depth <= 1 && root_function != "script" && is_likely_console_call) {
    # Get or create execution ID for this function instance
    execution_info <- .get_or_create_execution_id(calls, root_function)
    
    if (!is.null(execution_info)) {
      mv_debug(sprintf("Execution ID: %s (new: %s)", execution_info$id, execution_info$is_new), debug = debug)
      mv_debug(sprintf("Last execution ID was: %s", .mvcommon_env$last_execution_id %||% "NULL"), debug = debug)
      mv_debug(sprintf("Has existing numbering: %s", has_existing_numbering), debug = debug)
      
      # Check if this is a different execution than the last one
      # IMPORTANT: Check even for new executions when we have existing numbering
      if (!is.null(.mvcommon_env$last_execution_id) && 
          .mvcommon_env$last_execution_id != execution_info$id) {
        
        # Only reset if we're NOT in a structured document context
        # If current_level > 0, we have document structure and should be subordinate
        if (current_level == 0) {
          # No document context - apply console reset
          case_type <- "Console_reset"
          should_reset <- TRUE
          case_description <- sprintf("Console reset - %s() new execution (ID: %s, was: %s)", 
                                     root_function, execution_info$id, .mvcommon_env$last_execution_id)
          mv_debug(sprintf("CONSOLE RESET: %s", case_description), debug = debug)
        } else {
          # In structured document - function should be subordinate, not reset
          mv_debug(sprintf("Different %s() execution detected, but in structured document (level %d) - will be subordinate", 
                          root_function, current_level), debug = debug)
        }
      }
      
      # Update last execution ID even for first calls to enable future reset detection
      .mvcommon_env$last_execution_id <- execution_info$id
    }
  }
  
  # Skip further case classification if console reset was already applied
  if (case_type == "Console_reset") {
    # Console reset case already determined - don't override
    mv_debug(sprintf("Console reset case already set - skipping further classification"), debug = debug)
  
  # CLEAR RULE 1: Script calls (user_depth=0) always continue document numbering
  } else if (user_depth == 0) {
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
    
  } else if ((is_interactive_session || is_likely_console_call || is_separate_execution) && user_depth <= 1) {
    # CASE 1: Interactive console calls OR top-level direct calls OR separate executions
    if (is.null(.mvcommon_env$last_function_call) || .mvcommon_env$last_function_call != root_function) {
      # Different function or first call
      case_type <- "Case1_first"
      case_description <- sprintf("Console call - first time calling %s()", root_function)
      should_reset <- TRUE
      .mvcommon_env$function_call_count <- 1
    } else {
      # Same function called again in console or separate execution
      .mvcommon_env$function_call_count <- .mvcommon_env$function_call_count + 1
      case_type <- "Case1_repeat"
      case_description <- sprintf("Console call - %s() called again (call #%d) - SHOULD RESET (separate_execution=%s)", 
                                 root_function, .mvcommon_env$function_call_count, is_separate_execution)
      should_reset <- TRUE
    }
    .mvcommon_env$last_function_call <- root_function
    
  } else if (!is_interactive_session && user_depth <= 1) {
    # CASE 2: Script execution at top level
    if (!has_existing_numbering) {
      case_type <- "Case2a_first"
      case_description <- sprintf("Script call - %s() first time, no prior mv_title calls", root_function)
      should_reset <- FALSE
    } else {
      case_type <- "Case2a_continue"
      case_description <- sprintf("Script call - %s() continuing numbering within same execution", root_function)
      should_reset <- FALSE
    }
    
  } else if (user_depth > 1) {
    # CASE 3: Function called from within other code (nested)
    if (!has_existing_numbering) {
      case_type <- "Case2a_nested"
      case_description <- sprintf("Nested function call - %s() at depth %d, no prior numbering context", 
                                 root_function, user_depth)
      should_reset <- FALSE
    } else {
      # Determine subordination level based on current context
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
  }
  
  # Update tracking variables - only track actual function calls, not direct script calls
  if (user_depth <= 1 && root_function != "script") {
    .mvcommon_env$last_function_call <- root_function
    mv_debug(sprintf("Updating last_function_call to: %s", root_function), debug = debug)
  } else if (user_depth <= 1 && root_function == "script") {
    mv_debug(sprintf("Not updating last_function_call for script call (keeping: %s)", 
                     .mvcommon_env$last_function_call %||% "NULL"), debug = debug)
  }
  
  # Console reset logic is now handled by execution ID tracking above
  
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

# Check if this is a new execution run
.is_new_execution_run <- function(calls) {
  current_sig <- .detect_execution_run(calls)
  
  # Only consider top-level calls for reset detection
  if (!current_sig$is_top_level) {
    return(FALSE)
  }
  
  # First execution run ever
  if (is.null(.mvcommon_env$current_execution_run)) {
    .mvcommon_env$execution_run_counter <- .mvcommon_env$execution_run_counter + 1
    .mvcommon_env$current_execution_run <- list(
      run_id = .mvcommon_env$execution_run_counter,
      root_function = current_sig$root_function,
      signature = current_sig
    )
    mv_debug(sprintf("Starting first execution run #%d for '%s'", 
                     .mvcommon_env$execution_run_counter,
                     current_sig$root_function), debug = debug)
    return(FALSE)
  }
  
  last_run <- .mvcommon_env$current_execution_run
  
  # Check if this is a new run (different function OR fresh start of same function)
  is_new_run <- FALSE
  
  # Rule 1: Different root function = definitely new run
  if (current_sig$root_function != last_run$root_function) {
    is_new_run <- TRUE
    mv_debug(sprintf("New run: Different function (%s -> %s)", 
                     last_run$root_function, current_sig$root_function), debug = debug)
  }
  
  # Rule 2: Same function - use deep execution tracking to detect fresh starts
  else if (current_sig$root_function == last_run$root_function) {
    # Same function - check if this is fresh start vs continuation
    if (current_sig$call_stack_depth <= 4) {
      # Shallow call - check if we've completed a deep execution
      if (.mvcommon_env$has_deep_execution) {
        # We had deep execution, now shallow = fresh start
        is_new_run <- TRUE
        .mvcommon_env$has_deep_execution <- FALSE  # Reset for new run
        mv_debug(sprintf("New run: Fresh start '%s' (after deep execution)", 
                         current_sig$root_function), debug = debug)
      } else {
        # No deep execution yet - continuation
        is_new_run <- FALSE
        mv_debug(sprintf("Continuing: same function '%s' (no deep execution yet)", 
                         current_sig$root_function), debug = debug)
      }
    } else {
      # Deep call - mark that we've had deep execution
      .mvcommon_env$has_deep_execution <- TRUE
      is_new_run <- FALSE
      mv_debug(sprintf("Deep execution: '%s' (depth=%d)", 
                       current_sig$root_function, current_sig$call_stack_depth), debug = debug)
    }
  }
  
  if (is_new_run) {
    # Start new execution run
    .mvcommon_env$execution_run_counter <- .mvcommon_env$execution_run_counter + 1
    .mvcommon_env$current_execution_run <- list(
      run_id = .mvcommon_env$execution_run_counter,
      root_function = current_sig$root_function,
      signature = current_sig
    )
    mv_debug(sprintf("Started new execution run #%d for '%s'", 
                     .mvcommon_env$execution_run_counter,
                     current_sig$root_function), debug = debug)
    return(TRUE)
  }
  
  # Same execution run continuing
  mv_debug(sprintf("Continuing execution run #%d for '%s'", 
                   last_run$run_id, current_sig$root_function), debug = debug)
  return(FALSE)
}

# Get current execution context for comparison
.get_execution_context <- function() {
  calls <- sys.calls()
  
  # Try to get source information from the call stack
  src_file <- NULL
  src_ref <- NULL
  execution_depth <- length(calls)
  
  # Look through the call stack for source information
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (length(call) > 0) {
      # Try to get source reference
      call_src <- getSrcref(call)
      if (!is.null(call_src)) {
        src_ref <- call_src
        src_file <- getSrcFilename(call_src)
        break
      }
    }
  }
  
  # If no source info found, check if we're in interactive mode
  if (is.null(src_file)) {
    src_file <- if (interactive()) "<interactive>" else "<unknown>"
  }
  
  # Create execution fingerprint - combination of source + call pattern
  # Get the actual root call (not mv_title call)
  root_call <- "<none>"
  if (length(calls) > 0) {
    # Find the first non-mv_ call which represents the actual execution root
    for (i in seq_along(calls)) {
      call <- calls[[i]]
      if (length(call) > 0) {
        func_name <- deparse(call[[1]], nlines = 1)
        if (!grepl("^mv_", func_name)) {
          root_call <- deparse(call)
          break
        }
      }
    }
  }
  
  execution_fingerprint <- list(
    src_file = src_file,
    is_interactive = interactive(),
    root_call = root_call,
    stack_depth = execution_depth
  )
  
  return(execution_fingerprint)
}

# Check if current execution context is different from stored context
.is_different_execution_context <- function(current_context, stored_context) {
  if (is.null(stored_context)) {
    return(FALSE)  # No stored context yet
  }
  
  # Different source file indicates different context
  if (!identical(current_context$src_file, stored_context$src_file)) {
    return(TRUE)
  }
  
  # Switch between interactive and non-interactive indicates different context
  if (current_context$is_interactive != stored_context$is_interactive) {
    return(TRUE)
  }
  
  # For script execution, we should NOT reset on different root calls
  # Only reset if we're truly in a new execution (e.g., new source() call)
  # This is handled by checking if we're coming from source() itself
  if (!current_context$is_interactive && current_context$src_file != "<unknown>") {
    # In a script - don't reset unless the source file changed
    return(FALSE)
  }
  
  # For interactive mode, different root call indicates new execution
  if (current_context$is_interactive && !identical(current_context$root_call, stored_context$root_call)) {
    return(TRUE)
  }
  
  return(FALSE)
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
  .mvcommon_env$context_source <- NULL
  .mvcommon_env$current_execution_run <- NULL
  .mvcommon_env$execution_run_counter <- 0
  .mvcommon_env$has_deep_execution <- FALSE
  .mvcommon_env$execution_instances <- list()
  .mvcommon_env$execution_counter <- 0
  .mvcommon_env$last_execution_id <- NULL
  .mvcommon_env$current_script_anchor <- NULL
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


# Generate execution instance ID - simplified approach
.get_execution_instance_id <- function(calls, user_depth) {
  if (user_depth == 0) {
    return("_script_")
  }
  
  # Find the function at the target depth
  excluded_functions <- c("source", "eval", "eval.parent", "local", "eval.with.vis",
                         "do.call", "lapply", "sapply", "for", "{", "withVisible",
                         "%||%", ".build_section_number", ".GlobalEnv")
  
  depth_count <- 0
  target_function <- NULL
  
  for (i in seq_along(calls)) {
    if (length(calls[[i]]) > 0) {
      func_name <- deparse(calls[[i]][[1]], nlines = 1)
      is_mv <- grepl("^mv_", func_name)
      is_excluded <- func_name %in% excluded_functions
      
      if (!is_mv && !is_excluded) {
        depth_count <- depth_count + 1
        
        if (depth_count == user_depth) {
          target_function <- func_name
          break
        }
      }
    }
  }
  
  if (is.null(target_function)) {
    return("_unknown_")
  }
  
  # Use function name + depth as the execution ID
  # This will be the same for all mv_title calls within the same function at the same depth
  execution_id <- paste0(target_function, "_depth", user_depth)
  
  return(execution_id)
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
  
  # Check for execution context changes FIRST (before case detection)
  execution_context_reset <- FALSE
  if (!reset) {  # Only check if not explicitly resetting
    current_context <- .get_execution_context()
    
    if (is.null(.mvcommon_env$context_source)) {
      # First time setup - store current context
      .mvcommon_env$context_source <- current_context
    } else if (.is_different_execution_context(current_context, .mvcommon_env$context_source)) {
      execution_context_reset <- TRUE
      mv_debug("Different execution context detected, will reset", debug = debug)
      .mvcommon_env$context_source <- current_context
    }
  }
  
  # Script execution detection for script-level calls (user_depth = 0)
  script_execution_reset <- FALSE
  if (user_depth == 0 && !reset) {  # Only for script-level calls, not if already resetting
    current_script_anchor <- .create_script_execution_anchor()
    
    if (is.null(.mvcommon_env$current_script_anchor)) {
      # First script call ever
      .mvcommon_env$current_script_anchor <- current_script_anchor
      mv_debug(sprintf("First script execution, anchor: %s", current_script_anchor), debug = debug)
    } else if (.mvcommon_env$current_script_anchor != current_script_anchor) {
      # Different script execution detected
      script_execution_reset <- TRUE
      mv_debug(sprintf("New script execution detected, anchor changed: %s -> %s", 
                       .mvcommon_env$current_script_anchor, current_script_anchor), debug = debug)
      .mvcommon_env$current_script_anchor <- current_script_anchor
    }
  }
  
  # CASE-BASED reset detection - but only for top-level function calls, not nested mv_title calls
  case_info <- .detect_use_case(calls, title)
  
  # Combine all reset conditions: explicit, case-based, execution context, or script execution change
  should_reset <- reset
  if (user_depth <= 1 && case_info$should_reset) {
    should_reset <- TRUE
    mv_debug(sprintf("Applying case-based reset for top-level call"), debug = debug)
  }
  if (execution_context_reset) {
    should_reset <- TRUE
    mv_debug(sprintf("Applying execution context reset"), debug = debug)
  }
  if (script_execution_reset) {
    should_reset <- TRUE
    mv_debug(sprintf("Applying script execution reset"), debug = debug)
  }
  
  mv_debug(sprintf("Reset decision: explicit=%s, case_based=%s, context_reset=%s, script_reset=%s (user_depth=%d), final=%s", 
                   reset, case_info$should_reset, execution_context_reset, script_execution_reset, user_depth, should_reset), debug = debug)
  
  if (should_reset) {
    # Reset numbering for new execution
    .mvcommon_env$level1 <- 0
    .mvcommon_env$level2 <- 0
    .mvcommon_env$level3 <- 0
    .mvcommon_env$script_context_level <- 0
    .mvcommon_env$last_numbered_level <- 0
    .mvcommon_env$context_source <- current_context
    .mvcommon_env$execution_instances <- list()
    .mvcommon_env$execution_counter <- 0
    # Note: execution run tracking is managed by .is_new_execution_run()
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
    # Case 1, Case 2: Use execution instance logic for consistency
    execution_id <- .get_execution_instance_id(calls, user_depth)
    
    # Check if this execution instance already has an assigned level
    if (execution_id %in% names(.mvcommon_env$execution_instances)) {
      # Reuse the assigned level for consistency within the same execution
      effective_depth <- .mvcommon_env$execution_instances[[execution_id]] + level_adjust
      mv_debug(sprintf("Reusing execution instance: %s -> level %d (effective: %d)", execution_id, 
                       .mvcommon_env$execution_instances[[execution_id]], effective_depth), debug = debug)
    } else {
      # First time for this execution instance - assign a level based on context
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
      
      # Store the assigned level for this execution instance
      .mvcommon_env$execution_instances[[execution_id]] <- effective_depth - level_adjust
      mv_debug(sprintf("Assigned new execution instance: %s -> level %d", execution_id, effective_depth - level_adjust), debug = debug)
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
