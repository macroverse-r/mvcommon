% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mv_standardize_country_name.r
\name{mv_standardize_country_name}
\alias{mv_standardize_country_name}
\title{Standardize Country Names}
\usage{
mv_standardize_country_name(country, remove_articles = TRUE, to_ascii = TRUE)
}
\arguments{
\item{country}{Character vector of country names to standardize}

\item{remove_articles}{Logical, remove articles (the, la, el, etc.)}

\item{to_ascii}{Logical, convert to ASCII characters}
}
\value{
Character vector of standardized country names
}
\description{
Standardizes country names to facilitate matching with ISO codes.
Handles common variations, special characters, and government terminology.
}
\examples{
# Basic standardization
mv_standardize_country_name(c("USA", "U.S.A.", "United States"))

# Handle special characters
mv_standardize_country_name("Côte d'Ivoire")

# Government terms
mv_standardize_country_name("Republic of Korea")
}
