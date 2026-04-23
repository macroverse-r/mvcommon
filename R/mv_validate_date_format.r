#' Validate and Parse Date Formats
#'
#' @description
#' Validates date formats and converts them to standard formats used
#' throughout the macroverse ecosystem. Handles years, quarters, and
#' other date formats consistently.
#'
#' @param dates Character vector or Date vector of dates to validate
#' @param format Expected format: "year", "quarter", "date", or "auto"
#' @param convert Logical, whether to convert to Date objects
#'
#' @return If convert = TRUE, returns Date vector. Otherwise returns
#'   validated character vector in standard format.
#'
#' @export
#' @examples
#' # Validate years
#' mv_validate_date_format(c("2020", "2021"), format = "year")
#' 
#' # Validate quarters
#' mv_validate_date_format(c("2020Q1", "2020Q2"), format = "quarter")
#' 
#' # Auto-detect format
#' mv_validate_date_format(c("2020", "2020Q1"), format = "auto")
mv_validate_date_format <- function(dates, format = "auto", convert = FALSE) {
  # Handle NULL or empty input
  if (is.null(dates) || length(dates) == 0) {
    return(dates)
  }
  
  # Convert to character for processing
  dates_char <- as.character(dates)
  
  # Auto-detect format if needed
  if (format == "auto") {
    format <- .detect_date_format(dates_char)
  }
  
  # Validate based on format
  result <- switch(format,
    year = .validate_years(dates_char),
    quarter = .validate_quarters(dates_char),
    date = .validate_dates(dates_char),
    mv_stop("Invalid date format",
            "x" = "Format '{format}' not recognized",
            "i" = "Use 'year', 'quarter', 'date', or 'auto'")
  )
  
  # Convert to Date if requested
  if (convert) {
    result <- switch(format,
      year = as.Date(paste0(result, "-01-01")),
      quarter = .quarter_to_date(result),
      date = as.Date(result),
      result
    )
  }
  
  return(result)
}

# Internal functions
.detect_date_format <- function(dates) {
  # Remove NAs for detection
  dates_clean <- dates[!is.na(dates)]
  
  if (length(dates_clean) == 0) {
    return("date")  # Default
  }
  
  # Check patterns
  if (all(grepl("^\\d{4}$", dates_clean))) {
    return("year")
  } else if (all(grepl("^\\d{4}Q[1-4]$", dates_clean))) {
    return("quarter")
  } else {
    return("date")
  }
}

.validate_years <- function(years) {
  # Check format
  valid <- grepl("^\\d{4}$", years) | is.na(years)
  
  if (!all(valid)) {
    invalid_years <- unique(years[!valid])
    mv_stop("Invalid year format",
            "x" = "Years must be 4-digit numbers",
            "i" = "Invalid values: {.val {invalid_years}}")
  }
  
  # Check range
  year_nums <- as.integer(years[!is.na(years)])
  if (any(year_nums < 1800 | year_nums > 2100)) {
    mv_warn("Unusual year values detected",
            "!" = "Years outside 1800-2100 range",
            "i" = "Please verify these are correct")
  }
  
  return(years)
}

.validate_quarters <- function(quarters) {
  # Check format
  valid <- grepl("^\\d{4}Q[1-4]$", quarters) | is.na(quarters)
  
  if (!all(valid)) {
    invalid_quarters <- unique(quarters[!valid])
    mv_stop("Invalid quarter format",
            "x" = "Quarters must be in format 'YYYYQ#' (e.g., '2020Q1')",
            "i" = "Invalid values: {.val {invalid_quarters}}")
  }
  
  return(quarters)
}

.validate_dates <- function(dates) {
  # Try to parse as dates
  parsed <- suppressWarnings(as.Date(dates))
  
  if (any(is.na(parsed) & !is.na(dates))) {
    invalid_dates <- unique(dates[is.na(parsed) & !is.na(dates)])
    mv_stop("Invalid date format",
            "x" = "Could not parse as dates",
            "i" = "Invalid values: {.val {invalid_dates}}")
  }
  
  return(dates)
}

.quarter_to_date <- function(quarters) {
  # Convert quarters to first day of quarter
  year <- as.integer(substr(quarters, 1, 4))
  quarter <- as.integer(substr(quarters, 6, 6))
  
  month <- (quarter - 1) * 3 + 1
  as.Date(paste(year, month, "01", sep = "-"))
}

#' Parse quarters from dates
#' @keywords internal
#' @noRd
.parse_quarters <- function(dates) {
  dates <- as.Date(dates)
  year <- format(dates, "%Y")
  quarter <- quarters(dates)
  quarter_num <- match(quarter, c("Q1", "Q2", "Q3", "Q4"))
  paste0(year, "Q", quarter_num)
}

#' Parse years from dates  
#' @keywords internal
#' @noRd
.parse_years <- function(dates) {
  format(as.Date(dates), "%Y")
}