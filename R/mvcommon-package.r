#' mvcommon: Common Utilities for macroverse Ecosystem
#'
#' @description
#' The mvcommon package provides shared utilities, validation functions, and common
#' infrastructure for the macroverse ecosystem. This lightweight package ensures
#' consistency across all macroverse packages without introducing heavy dependencies.
#'
#' @details
#' ## Key Features
#'
#' ### Data Validation
#' - \code{\link{mv_validate_data}}: Master validation for encoding, structure, and integrity
#' - \code{\link{mv_validate_date_format}}: Ensure proper date/time formatting
#' - \code{\link{mv_as_valid_data}}: Convert and validate in one step
#'
#' ### Performance Monitoring
#' - \code{\link{mv_memory_usage}}: Check memory usage and estimate operation requirements
#'
#' ### Configuration Management
#' - \code{\link{mv_set_config}}, \code{\link{mv_get_config}}: Manage macroverse ecosystem settings
#'
#' ### Text Processing
#' - \code{\link{mv_standardize_country_name}}: Prepare country names for ISO matching
#'
#' ### Progress Reporting
#' - \code{\link{mv_progress_bar}}: Progress bars for long operations
#'
#' ### Metadata Management
#' - \code{\link{mv_add_metadata}}, \code{\link{mv_get_metadata}}: Track data provenance
#'
#' ### Utility Functions
#' - \code{\link{mv_check_internet}}: Check connectivity before downloads
#' - \code{\link{mv_check_package}}: Check for optional dependencies
#' - \code{\link{mv_get_colors}}: Color palettes for visualization
#'
#' @section Configuration:
#' The mvcommon package uses a configuration system that affects all macroverse
#' packages. Use \code{mv_set_config()} to change settings and \code{mv_get_config()}
#' to view current settings.
#'
#' @section See Also:
#' Other macroverse packages:
#' \itemize{
#'   \item \code{pplot}: Panel data visualization
#'   \item \code{isomapper}: ISO codes and country mapping
#'   \item \code{macrodata}: Data loading and processing
#'   \item \code{mvlazy}: Convenience functions
#'   \item \code{macroverse}: Umbrella package
#' }
#'
#' @docType package
#' @name mvcommon-package
#' @aliases mvcommon
#' @keywords internal
"_PACKAGE"