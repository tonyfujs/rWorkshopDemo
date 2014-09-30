#' Analyze each data set in a folder
#'
#' @param path_in A character vector. Path to data folder
#' @export
#' @examples
#' library(rWorkshop1Demo)
#' analyze_data_files(path_in = './data/')
#' list.files('./output/')

analyze_data_files <- function(path_in = "./") {
  # Read .csv files in directory
  files <- list.files(path_in)
  files <- files[grepl(".csv$", files)] 
  
  for (i in seq_along(files)) {
    # Read file
    df <- read.csv(paste0(path_in, files[i]), header = TRUE, stringsAsFactors = FALSE)
    
    # Analyze file
    p <- analyze_data(df)
    
    print(p)
  }
}
