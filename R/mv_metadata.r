#' Add and Retrieve Metadata for Data Objects
#'
#' @description
#' Functions to add and retrieve metadata attributes from data objects.
#' Metadata is stored as attributes with a special prefix to avoid conflicts.
#'
#' @name mv_metadata
#' @examples
#' # Add metadata to a data frame
#' data <- iris
#' data <- mv_add_metadata(data,
#'   source = "Anderson 1935",
#'   date_accessed = Sys.Date(),
#'   preprocessing = "None"
#' )
#' 
#' # Retrieve all metadata
#' mv_get_metadata(data)
#' 
#' # Retrieve specific metadata
#' mv_get_metadata(data, "source")
NULL

# Prefix for metadata attributes
.macroverse_metadata_prefix <- "mv_meta_"

#' Add Metadata to Data Object
#'
#' @param data Data object (typically data frame)
#' @param ... Named metadata items to add
#' @param .replace Logical, whether to replace existing metadata
#'
#' @return The data object with metadata attributes added
#' @export
mv_add_metadata <- function(data, ..., .replace = TRUE) {
  metadata <- list(...)
  
  if (length(metadata) == 0) {
    mv_warn("No metadata provided",
            "i" = "Use named arguments to add metadata")
    return(data)
  }
  
  # Add each metadata item
  for (name in names(metadata)) {
    attr_name <- paste0(.macroverse_metadata_prefix, name)
    
    # Check if already exists
    if (!.replace && !is.null(attr(data, attr_name))) {
      mv_warn("Metadata '{name}' already exists",
              "i" = "Use .replace = TRUE to overwrite")
      next
    }
    
    attr(data, attr_name) <- metadata[[name]]
  }
  
  # Add metadata timestamp
  attr(data, paste0(.macroverse_metadata_prefix, "last_updated")) <- Sys.time()
  
  data
}

#' Get Metadata from Data Object
#'
#' @param data Data object with metadata
#' @param key Optional specific metadata key to retrieve
#' @param .remove_prefix Logical, remove the prefix from returned names
#'
#' @return If key is provided, returns the specific metadata value.
#'   Otherwise returns a list of all metadata.
#' @export
mv_get_metadata <- function(data, key = NULL, .remove_prefix = TRUE) {
  # Get all attributes
  all_attrs <- attributes(data)
  
  # Filter for metadata attributes
  meta_names <- grep(paste0("^", .macroverse_metadata_prefix), 
                     names(all_attrs), value = TRUE)
  
  if (length(meta_names) == 0) {
    if (!is.null(key)) {
      return(NULL)
    } else {
      return(list())
    }
  }
  
  # Get specific key if requested
  if (!is.null(key)) {
    attr_name <- paste0(.macroverse_metadata_prefix, key)
    return(attr(data, attr_name))
  }
  
  # Get all metadata
  metadata <- all_attrs[meta_names]
  
  # Remove prefix if requested
  if (.remove_prefix) {
    names(metadata) <- gsub(paste0("^", .macroverse_metadata_prefix), 
                           "", names(metadata))
  }
  
  metadata
}

#' Remove Metadata from Data Object
#'
#' @param data Data object with metadata
#' @param keys Character vector of metadata keys to remove. If NULL, removes all.
#'
#' @return The data object with metadata removed
#' @export
mv_remove_metadata <- function(data, keys = NULL) {
  if (is.null(keys)) {
    # Remove all metadata
    all_attrs <- attributes(data)
    meta_names <- grep(paste0("^", .macroverse_metadata_prefix), 
                       names(all_attrs), value = TRUE)
    
    for (attr_name in meta_names) {
      attr(data, attr_name) <- NULL
    }
    
    if (length(meta_names) > 0) {
      mv_success("Removed all metadata ({length(meta_names)} items)")
    }
  } else {
    # Remove specific keys
    removed <- 0
    for (key in keys) {
      attr_name <- paste0(.macroverse_metadata_prefix, key)
      if (!is.null(attr(data, attr_name))) {
        attr(data, attr_name) <- NULL
        removed <- removed + 1
      }
    }
    
    if (removed > 0) {
      mv_success("Removed {removed} metadata item{?s}")
    }
  }
  
  data
}

#' Check if Data Has Metadata
#'
#' @param data Data object to check
#' @param key Optional specific metadata key to check for
#'
#' @return Logical indicating whether metadata exists
#' @export
mv_has_metadata <- function(data, key = NULL) {
  if (is.null(key)) {
    # Check for any metadata
    all_attrs <- attributes(data)
    meta_names <- grep(paste0("^", .macroverse_metadata_prefix), 
                       names(all_attrs), value = TRUE)
    return(length(meta_names) > 0)
  } else {
    # Check for specific key
    attr_name <- paste0(.macroverse_metadata_prefix, key)
    return(!is.null(attr(data, attr_name)))
  }
}