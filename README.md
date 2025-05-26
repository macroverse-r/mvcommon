
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mvcommon

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end -->

The goal of mvcommon is to provide shared utilities, validation
functions, and common infrastructure for the macroverse ecosystem. This
lightweight package ensures consistency across all macroverse packages
without introducing heavy dependencies.

This package is part of the [macroverse
ecosystem](https://github.com/macroverse-r/macroverse).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macroverse-r/mvcommon")
```

## Key Features

### Data Validation

``` r
library(mvcommon)

# Validate and fix common data issues
data <- mv_validate_data(my_data)

# Validate date formats
dates <- mv_validate_date_format(c("2020", "2020Q1"), format = "auto")

# Convert and validate in one step
clean_data <- mv_as_valid_data(messy_data)
```

### Messaging and Progress

``` r
# Consistent error messages across packages
mv_stop("Invalid input", 
        "x" = "Expected numeric vector",
        "i" = "Got character vector instead")

# Progress bars for long operations
pb <- mv_progress_bar(100, 
                      format = "Processing {cli::pb_current}/{cli::pb_total} items")
for (i in 1:100) {
  # Do work...
  pb$tick()
}
```

### Configuration Management

``` r
# Set global options for the ecosystem
mv_set_config(verbose = TRUE, 
              debug = FALSE,
              encoding = "UTF-8")

# Check current settings
mv_get_config("verbose")
#> [1] TRUE
```

### Country Name Standardization

``` r
# Standardize country names for ISO matching
names <- c("USA", "U.S.A.", "United States", "Côte d'Ivoire")
mv_standardize_country_name(names)
#> [1] "USA" "USA" "USA" "Ivory Coast"
```

### Metadata Management

``` r
# Add metadata to track data provenance
data <- mv_add_metadata(data,
                        source = "World Bank",
                        download_date = Sys.Date(),
                        indicators = c("GDP", "Population"))

# Retrieve metadata later
mv_get_metadata(data)
```

### Memory and Performance

``` r
# Check memory usage before operations
mv_memory_usage(large_dataset, operation = "join", another_dataset)

# Check internet connectivity
if (mv_check_internet()) {
  # Download data...
}
```

### Color Palettes

``` r
# Get colorblind-friendly palettes
colors <- mv_get_colors("default")
plot(1:8, col = colors, pch = 19, cex = 3)

# Get all available palettes
all_palettes <- mv_get_colors("all")
```

## macroverse Ecosystem

The mvcommon package is the foundation of the macroverse ecosystem: -
**mvcommon**: Common utilities (this package) -
**[pplot](https://github.com/macroverse-r/pplot)**: Panel data
visualization -
**[isomapper](https://github.com/macroverse-r/isomapper)**: ISO codes
and country mapping -
**[macrodata](https://github.com/macroverse-r/macrodata)**: Data loading
and processing - **[mvlazy](https://github.com/macroverse-r/mvlazy)**:
Convenience functions -
**[macroverse](https://github.com/macroverse-r/macroverse)**:
Meta-package loading all components

## License

This package is available under a dual-licensing model: - **Open
Source**: AGPL-3.0 for academic and non-commercial use - **Commercial**:
Alternative licensing available for commercial applications

See LICENSE.md for details.
