# Internal debug utilities for mvcommon

#' Helper function for nice debug/verbose printing
#' @keywords internal
#' @noRd
.print_debug <- function(text, verbose = TRUE, debug = FALSE, type = "info", text_type = "INFO") {
    # Only print if either verbose or debug is TRUE
    if (!verbose && !debug) return(NULL)
    
    # Define color codes
    colors <- list(
        info = .create_color("cyan"),
        info_loop = .create_color("green"), 
        debug = .create_color("white"), 
        wrapper = .create_color("purple", 2),
        warning = .create_color("red"),
        missing = .create_color("yellow"),
        empty = .create_color("blue", "bold")
    )
    reset <- "\033[0m"            # Reset color
    
    # Get current time
    current_time <- format(Sys.time(), "%H:%M:%S")
    
    # Create prefix based on whether it's verbose or debug
    prefix <- list(
        info = paste0(" [",text_type,"]"),      # Cyan for general info
        info_loop = "  --- ",    # Yellow for warnings
        debug = "[DEBUG]",      # Red for errors
        wrapper = "#",     # Green for success
        warning = "/!\\ ",     # Red for warning
        missing = "",      # Cyan for general info
        empty = ""     # Red for warning
    )
    
    # Construct and print the message with color
    message <- sprintf("%s%s %s%s", 
                      colors[[type]], 
                      prefix[[type]],
                      text,
                      reset)
    cat(message, "\n")
}

#' Helper function to create color codes
#'
#' @description
#' Helper function to create color codes
#'
#' @param color Character ("cyan", "green", "white", 'black', 'purple', etc.)
#' @param style Character ("normal", "bold", "dim", "italic", "underline")
#' @return ANSI escape code
#'
#' @keywords internal
#' @noRd
.create_color <- function(color, style = "normal") {
    # Basic color codes
    color_codes <- list(
        black = 30,
        red = 31,
        green = 32,
        yellow = 33,
        blue = 34,
        purple = 35,
        cyan = 36,
        white = 37
    )
    
    # Style codes
    style_codes <- list(
        normal = 0,
        bold = 1,
        dim = 2,
        italic = 3,
        underline = 4
    )

    # Create ANSI escape code
    sprintf("\033[%d;%dm", 
            style_codes[[style]], 
            color_codes[[color]])
}