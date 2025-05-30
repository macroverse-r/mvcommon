% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mv_config.r
\name{mv_config}
\alias{mv_config}
\title{Configuration Management for macroverse Ecosystem}
\description{
Functions to set and get configuration options for the macroverse ecosystem.
These settings affect behavior across all macroverse packages.
}
\examples{
# Set configuration options
mv_set_config(verbose = TRUE, encoding = "UTF-8")

# Get specific option
mv_get_config("verbose")

# Get all options
mv_get_config()

# Reset to defaults
mv_set_config(.reset = TRUE)
}
