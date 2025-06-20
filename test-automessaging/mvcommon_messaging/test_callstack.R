# Test script for script-level anchor ID system

# Global storage for script execution anchor
script_state <- new.env()
script_state$current_anchor <- NULL

# Function to create script execution anchor
id_anchor <- function() {
  calls <- sys.calls()
  
  if (length(calls) > 0) {
    # We have a call stack - use environment-based anchor
    top_env_str <- capture.output(print(sys.frame(1)))[1]
    env_hex <- gsub(".*0x([0-9a-f]+).*", "\\1", top_env_str)
    anchor <- paste0("SCRIPT_", env_hex)
  } else {
    # No call stack (Rscript, R --file, etc.) - use timestamp
    anchor <- paste0("DIRECT_", round(as.numeric(Sys.time()) * 1000000))
  }
  
  # Store the anchor for this script execution
  if (is.null(script_state$current_anchor)) {
    script_state$current_anchor <- anchor
    cat("[ANCHOR] New script execution detected, ID:", anchor, "\n")
  }
  
  return(script_state$current_anchor)
}

# Simplified version of mv_title
foo <- function(title) {
  anchor <- id_anchor()
  cat("[FOO] Title:", title, "| Script ID:", anchor, "\n")
}

# Test functions
fun1 <- function() {
  cat("[FUN1] Starting function 1\n")
  foo("Title from fun1")
  
  cat("[FUN1] Calling fun2 from fun1\n")
  fun2()
  
  cat("[FUN1] Back in fun1, calling foo again\n")
  foo("Second title from fun1")
}

fun2 <- function() {
  cat("[FUN2] Starting function 2\n")
  foo("Title from fun2")
  cat("[FUN2] Ending function 2\n")
}

# Test execution
cat("=== SCRIPT EXECUTION START ===\n")
foo("Script level title 1")
fun1()
foo("Script level title 2")
cat("=== SCRIPT EXECUTION END ===\n")