#' Validate DataFrame Columns
#'
#' Validates that an object is a data.frame and contains all required columns.
#' This function is called for its side effects and will stop execution with
#' an error message if validation fails.
#'
#' @param data Object to validate as a data.frame
#' @param required_cols Character vector of required column names
#'
#' @return NULL (called for side effects)
#' @export
validate_df_cols <- function(data, required_cols) {
  
  # Input validation for the validator function itself
  if (!is.character(required_cols)) {
    stop(paste0("Input 'required_cols' must be a character vector. Received: ", 
                class(required_cols)[1]))
  }
  
  # Check if object is a data.frame
  if (!is.data.frame(data)) {
    stop(paste0("Input data must be a data.frame. Received: ", class(data)[1]))
  }
  
  # Check for empty data
  if (nrow(data) == 0) {
    stop("Input data is empty (0 rows)")
  }
  
  # Check required columns exist
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    available_cols <- paste(names(data), collapse = ", ")
    stop(paste0("Required columns missing from data: ", 
                paste(missing_cols, collapse = ", "), 
                ". Available columns: ", available_cols))
  }
}