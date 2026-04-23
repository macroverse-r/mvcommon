#' Utility Functions for macroverse Ecosystem
#'
#' @description
#' General utility functions for checking system status, packages, and
#' other common operations across the macroverse ecosystem.
#'
#' @name mv_utils
NULL

#' Check Internet Connectivity
#'
#' @description
#' Checks if internet connection is available by attempting to connect
#' to reliable servers.
#'
#' @param timeout Timeout in seconds for connection attempt
#' @param test_url URL to test (default: Google DNS)
#'
#' @return Logical indicating whether internet is available
#' @export
#' @examples
#' if (mv_check_internet()) {
#'   message("Internet connection available")
#' }
mv_check_internet <- function(timeout = 5, test_url = "8.8.8.8") {
  # Skip if checking is disabled
  if (!mv_get_config("check_internet")) {
    return(TRUE)
  }
  
  # Try to connect
  connected <- .try_connection(test_url, timeout)
  
  if (!connected) {
    mv_warn("No internet connection detected",
            "!" = "Some features may not work properly",
            "i" = "Working offline with cached data only")
  }
  
  connected
}

#' Check if Package is Available
#'
#' @description
#' Checks if a package is installed and optionally loads it.
#' Useful for optional dependencies.
#'
#' @param package Package name
#' @param load Logical, whether to load the package
#' @param min_version Minimum required version (optional)
#'
#' @return Logical indicating whether package is available
#' @export
#' @examples
#' if (mv_check_package("ggplot2", load = TRUE)) {
#'   # Use ggplot2 functions
#' }
mv_check_package <- function(package, load = FALSE, min_version = NULL) {
  # Check if installed
  installed <- requireNamespace(package, quietly = TRUE)
  
  if (!installed) {
    mv_debug("Package '{package}' not installed")
    return(FALSE)
  }
  
  # Check version if specified
  if (!is.null(min_version)) {
    current_version <- utils::packageVersion(package)
    if (current_version < min_version) {
      mv_warn("Package version too old",
              "x" = "{package} version {current_version} < required {min_version}",
              "i" = "Please update with install.packages('{package}')")
      return(FALSE)
    }
  }
  
  # Load if requested
  if (load) {
    suppressPackageStartupMessages({
      loaded <- require(package, character.only = TRUE, quietly = TRUE)
    })
    
    if (!loaded) {
      mv_warn("Could not load package '{package}'")
      return(FALSE)
    }
  }
  
  TRUE
}

# Internal connection test
#' @keywords internal
#' @noRd
.try_connection <- function(url, timeout) {
  # Different method based on OS
  if (.Platform$OS.type == "windows") {
    # Windows: use base R
    tryCatch({
      con <- url(paste0("http://", url))
      suppressWarnings({
        open(con, "r", blocking = TRUE, timeout = timeout)
      })
      close(con)
      TRUE
    }, error = function(e) FALSE)
  } else {
    # Unix-like: use system ping
    cmd <- sprintf("ping -c 1 -W %d %s >/dev/null 2>&1", timeout, url)
    system(cmd) == 0
  }
}

#' Get System Information
#'
#' @description
#' Returns useful system information for debugging and support.
#'
#' @return List with system information
#' @export
mv_system_info <- function() {
  info <- list(
    os = Sys.info()[["sysname"]],
    os_release = Sys.info()[["release"]],
    r_version = R.version.string,
    locale = Sys.getlocale(),
    timezone = Sys.timezone(),
    working_directory = getwd(),
    macroverse_version = utils::packageVersion("mvcommon"),
    loaded_packages = loadedNamespaces()
  )
  
  class(info) <- c("mv_system_info", "list")
  info
}

#' @export
print.mv_system_info <- function(x, ...) {
  cli::cli_h2("System Information")
  cli::cli_dl(c(
    "OS" = paste(x$os, x$os_release),
    "R" = x$r_version,
    "Timezone" = x$timezone,
    "Working Dir" = x$working_directory,
    "mvcommon" = as.character(x$macroverse_version)
  ))
  
  cli::cli_h3("Loaded macroverse packages")
  mv_pkgs <- grep("^(mv|macroverse|pplot|isomapper|macrodata|mvlazy)", 
                  x$loaded_packages, value = TRUE)
  if (length(mv_pkgs) > 0) {
    cli::cli_ul(mv_pkgs)
  } else {
    cli::cli_text("{.emph No macroverse packages currently loaded}")
  }
  
  invisible(x)
}

#' Create a Temporary Directory for macroverse
#'
#' @description
#' Creates a temporary directory with proper permissions for
#' macroverse operations.
#'
#' @param prefix Prefix for directory name
#' @param cleanup Logical, whether to delete on exit
#'
#' @return Path to temporary directory
#' @export
mv_temp_dir <- function(prefix = "macroverse_", cleanup = TRUE) {
  # Create temp dir
  temp_base <- tempdir()
  temp_name <- tempfile(pattern = prefix, tmpdir = temp_base)
  dir.create(temp_name, recursive = TRUE)
  
  # Register cleanup if requested
  if (cleanup) {
    reg.finalizer(
      e = new.env(),
      f = function(e) {
        if (dir.exists(temp_name)) {
          unlink(temp_name, recursive = TRUE)
        }
      },
      onexit = TRUE
    )
  }
  
  mv_debug("Created temporary directory: {temp_name}")
  temp_name
}