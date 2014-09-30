#' Take a dataframe and perform some basic cleaning tasks
#'
#' @param data A data frame.
#' @export
#' @noRd

clean_data <- function(df) {
  # Describe dataset
  str(df)
  # Inform user
  n_missing <- sum(!complete.cases(df))
  message(paste('Removing', n_missing, 'incomplete records'))
  # Remove incomplete cases
  df <- df[complete.cases(df), ]
  # return cleaned data frame
  return(df)
}