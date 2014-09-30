#' Take a dataframe containing pre/post scores and returns a density plot
#'
#' @param data A data frame.
#' @export
#' @noRd

plot_data <- function(df) {
  # Reshape df
  df <- melt(df, id = "state")
  names(df) <- c('state', 'prepost', 'score')
  
  # Create title
  my_state <- toupper(unique(df$state))
  my_title <- paste0(my_state, ": Distribution of PRE and POST test scores\n")
  
  # Create a plot
  p <- ggplot(df, aes(x = prepost, y = score, fill = prepost)) + geom_boxplot(alpha = .5)
  p <- p + theme_igray() + scale_fill_tableau(palette = "trafficlight")
  p <- p + ggtitle(my_title)
  p <- p + ylim(0, 100)
  p <- p + theme(
    plot.title = element_text(family = my_font, face="bold", size = 12),
    axis.title.x = element_text(family = my_font),
    axis.title.y = element_text(family = my_font),
    axis.text.x = element_text(family = my_font),
    axis.text.y = element_text(family = my_font),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  )
 
  # Return plot
  return(p)
}