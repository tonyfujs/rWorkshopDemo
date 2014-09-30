#' Take a dataframe containing pre/post scores and runs a paired ttest
#'
#' @param data A data frame.
#' @export
#' @noRd
#' @import broom

analyze_data <- function(df) {
  # Reshape df
  df <- reshape2::melt(df, id = "state")
  names(df) <- c('state', 'prepost', 'score')
    
  # Create name
  my_state <- toupper(unique(df$state))
  
  # Run a ttest
  out <- lm(score ~ prepost, data = df)
  
  # Tidy output
  out <- tidy(out)
 
  # Summary paragraph
  change <- ifelse(out[2, 2] < 0, 'a decrease', 'an increase')
  paragraph <- sprintf("On average, students' scores showed %s of %1.1f points (p-value: %1.3e)",
                       change,
                       out[2, 2],
                       out[2, 'p.value'])
  # Return plot
  cat(paragraph)
}
