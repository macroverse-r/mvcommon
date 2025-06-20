#' Get Color Palettes for macroverse Ecosystem
#'
#' @description
#' Returns color palettes designed for the macroverse ecosystem.
#' These palettes are colorblind-friendly and optimized for data visualization.
#'
#' @param palette Character string specifying which palette to return.
#'   Options are "default", "series", "bar", "scatter", or "all".
#'   If NULL or "all", returns all palettes.
#'
#' @return If palette is specified, returns a character vector of colors.
#'   If palette is "all" or NULL, returns a list of all palettes.
#'
#' @export
#' @examples
#' # Get default palette
#' colors <- mv_get_colors("default")
#' 
#' # Get all palettes
#' all_palettes <- mv_get_colors()
#' 
#' # Use in plotting
#' plot(1:8, col = mv_get_colors("default"))
mv_get_colors <- function(palette = NULL) {
  # Define palettes
  palettes <- list(
    # Default colorblind-friendly palette
    default = c("#000000", "#56B4E9", "#E69F00", "#D55E00",
                "#009E73", "#0072B2", "#CC79A7", "#F0E442"),
    
    # Extended palette for series plots
    series = c("#000000", "#56B4E9", "#E69F00", "#D55E00",
               "#009E73", "#0072B2", "#CC79A7", "#F0E442",
               "#999999", "#4B4B4B", "#B8B8B8", "#2B2B2B"),
    
    # Palette for bar charts (fewer colors, more distinct)
    bar = c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7"),
    
    # Palette for scatter plots (gradient-friendly)
    scatter = c("#0072B2", "#56B4E9", "#009E73", "#E69F00", "#D55E00"),
    
    # Legacy wpd colors for backward compatibility
    macroverse_colors = c("#000000", "#56B4E9", "#E69F00", "#D55E00",
                          "#009E73", "#0072B2", "#CC79A7", "#F0E442")
  )
  
  # Return requested palette
  if (is.null(palette) || palette == "all") {
    return(palettes)
  } else if (palette %in% names(palettes)) {
    return(palettes[[palette]])
  } else {
    contextual::cx_stop("Invalid palette name",
            "x" = "Palette '{palette}' not found",
            "i" = "Available palettes: {.val {names(palettes)}}")
  }
}

# Internal color utilities
.get_color_gradient <- function(n, palette = "default") {
  base_colors <- mv_get_colors(palette)
  if (n <= length(base_colors)) {
    return(base_colors[1:n])
  } else {
    # Generate more colors by interpolation
    colorRampPalette(base_colors)(n)
  }
}
