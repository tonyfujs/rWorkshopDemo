#' Create a plot for each data set in a folder
#'
#' @param path_in A character vector. Path to data folder
#' @param path_out A character vector. Path to output folder: Where plots will be saved as .png
#' @export
#' @examples
#' library(rWorkshop1Demo)
#' plot_data_files(path_in = './data/', path_out = './output/')
#' list.files('./output/')

plot_data_files <- function(path_in = "./", path_out = "./") {
  # Read .csv files in directory
  files <- list.files(path_in)
  files <- files[grepl(".csv$", files)] 
  
  # Define values to be used in plot_data()
  my_font <- "sans"
  
  for (i in seq_along(files)) {
    # Read file
    df <- read.csv(paste0(path_in, files[i]), header = TRUE, stringsAsFactors = FALSE)
    
    # Plot file
    p <- plot_data(df)
    
    # save plot as .png
    plot_name <- sub('.csv$', '.png', files[i])
    ggsave(p, filename = paste0(path_out, plot_name), width = 20, height = 14, units = 'cm')
  }
}
