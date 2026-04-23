#' Standardize Country Names
#'
#' @description
#' Standardizes country names to facilitate matching with ISO codes.
#' Handles common variations, special characters, and government terminology.
#'
#' @param country Character vector of country names to standardize
#' @param remove_articles Logical, remove articles (the, la, el, etc.)
#' @param to_ascii Logical, convert to ASCII characters
#'
#' @return Character vector of standardized country names
#' @export
#' @examples
#' # Basic standardization
#' mv_standardize_country_name(c("USA", "U.S.A.", "United States"))
#' 
#' # Handle special characters
#' mv_standardize_country_name("C\u00f4te d'Ivoire")
#' 
#' # Government terms
#' mv_standardize_country_name("Republic of Korea")
mv_standardize_country_name <- function(country, 
                                       remove_articles = TRUE,
                                       to_ascii = TRUE) {
  # Handle NULL or empty input
  if (is.null(country) || length(country) == 0) {
    return(country)
  }
  
  # Convert to character and preserve NAs
  country_char <- as.character(country)
  not_na <- !is.na(country_char)
  
  # Work only on non-NA values
  standardized <- country_char
  standardized[not_na] <- country_char[not_na]
  
  # Step 1: Trim whitespace and convert to title case
  standardized[not_na] <- trimws(standardized[not_na])
  
  # Step 2: Handle special characters
  if (to_ascii) {
    standardized[not_na] <- .convert_to_ascii(standardized[not_na])
  }
  
  # Step 3: Standardize government terms
  standardized[not_na] <- .standardize_gov_terms(standardized[not_na])
  
  # Step 4: Remove articles if requested
  if (remove_articles) {
    standardized[not_na] <- .remove_articles(standardized[not_na])
  }
  
  # Step 5: Common replacements
  standardized[not_na] <- .apply_common_replacements(standardized[not_na])
  
  # Step 6: Final cleanup
  standardized[not_na] <- gsub("\\s+", " ", standardized[not_na])  # Multiple spaces to single
  standardized[not_na] <- trimws(standardized[not_na])
  
  return(standardized)
}

# Internal helper functions
.convert_to_ascii <- function(text) {
  # Common character replacements
  replacements <- c(
    "\u00e1|\u00e0|\u00e4|\u00e2|\u00e3|\u00e5|\u0105" = "a",
    "\u00c1|\u00c0|\u00c4|\u00c2|\u00c3|\u00c5|\u0104" = "A",
    "\u00e9|\u00e8|\u00eb|\u00ea|\u0119" = "e",
    "\u00c9|\u00c8|\u00cb|\u00ca|\u0118" = "E",
    "\u00ed|\u00ec|\u00ef|\u00ee" = "i",
    "\u00cd|\u00cc|\u00cf|\u00ce" = "I",
    "\u00f3|\u00f2|\u00f6|\u00f4|\u00f5|\u00f8" = "o",
    "\u00d3|\u00d2|\u00d6|\u00d4|\u00d5|\u00d8" = "O",
    "\u00fa|\u00f9|\u00fc|\u00fb" = "u",
    "\u00da|\u00d9|\u00dc|\u00db" = "U",
    "\u00fd|\u00ff" = "y",
    "\u00dd|\u0178" = "Y",
    "\u00f1" = "n",
    "\u00d1" = "N",
    "\u00e7" = "c",
    "\u00c7" = "C",
    "\u00df" = "ss",
    "\u00e6" = "ae",
    "\u00c6" = "AE",
    "\u0153" = "oe",
    "\u0152" = "OE"
  )
  
  result <- text
  for (pattern in names(replacements)) {
    result <- gsub(pattern, replacements[pattern], result)
  }
  
  # Remove any remaining non-ASCII
  result <- iconv(result, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  result
}

#' Standardize government terms
#' @keywords internal  
#' @noRd
.standardize_gov_terms <- function(text) {
  # Define patterns and replacements
  gov_patterns <- c(
    "^Republic of " = "",
    "^The Republic of " = "",
    "^Democratic Republic of " = "DR ",
    "^The Democratic Republic of " = "DR ",
    "^People's Republic of " = "PR ",
    "^The People's Republic of " = "PR ",
    "^Kingdom of " = "",
    "^The Kingdom of " = "",
    "^State of " = "",
    "^The State of " = "",
    "^Commonwealth of " = "",
    "^The Commonwealth of " = "",
    "^Federation of " = "",
    "^The Federation of " = "",
    " Republic$" = "",
    " Kingdom$" = "",
    " Islands$" = "",
    " Island$" = ""
  )
  
  result <- text
  for (pattern in names(gov_patterns)) {
    result <- gsub(pattern, gov_patterns[pattern], result, ignore.case = TRUE)
  }
  
  result
}

.remove_articles <- function(text) {
  # Articles in various languages
  articles <- c("^The ", "^La ", "^Le ", "^Les ", "^El ", "^Los ", "^Las ",
                "^Der ", "^Die ", "^Das ", "^Den ", "^Det ", "^De ")
  
  result <- text
  for (article in articles) {
    result <- gsub(article, "", result, ignore.case = TRUE)
  }
  
  result
}

.apply_common_replacements <- function(text) {
  # Common variations and abbreviations
  replacements <- list(
    # USA variations
    "^United States$|^US$|^U\\.S\\.$|^U\\.S\\.A\\.$|^United States of America$" = "USA",
    
    # UK variations  
    "^United Kingdom$|^U\\.K\\.$|^Great Britain$|^Britain$" = "UK",
    "^United Kingdom of Great Britain and Northern Ireland$" = "UK",
    
    # Korea variations
    "^South Korea$|^Republic of Korea$|^Korea, Republic of$" = "Korea",
    "^North Korea$|^Democratic People's Republic of Korea$|^Korea, Democratic People's Republic of$" = "Korea DPR",
    
    # China variations
    "^People's Republic of China$|^China, People's Republic of$" = "China",
    
    # Other common variations
    "^Russian Federation$" = "Russia",
    "^Czech Republic$" = "Czechia",
    "^Syrian Arab Republic$" = "Syria",
    "^Lao People's Democratic Republic$" = "Laos",
    " and " = " & ",
    "Saint " = "St. "
  )
  
  result <- text
  for (pattern in names(replacements)) {
    matches <- grepl(pattern, result, ignore.case = TRUE)
    if (any(matches)) {
      result[matches] <- replacements[[pattern]]
    }
  }
  
  result
}

#' Handle special characters in country names
#' @keywords internal
#' @noRd  
.handle_special_chars <- function(text) {
  # Preserve special cases
  special_cases <- c(
    "Cote d'Ivoire" = "Ivory Coast",
    "Timor-Leste" = "East Timor",
    "Myanmar" = "Burma"  # Historical name sometimes used
  )
  
  result <- text
  for (pattern in names(special_cases)) {
    matches <- grepl(pattern, result, ignore.case = TRUE)
    if (any(matches)) {
      result[matches] <- special_cases[[pattern]]
    }
  }
  
  result
}
