#' Clean all data files in a folder
#'
#' @param path_in A character vector. Path to data folder
#' @param path_out A character vector. Path to output folder: Where cleaned data files will be saved as .csv
#' @export
#' @examples
#' library(rWorkshop1Demo)
#' clean_data_files(path_in = './data/', path_out = './output/')
#' list.files('./output/')

clean_data_files <- function(path_in = "./", path_out = "./") {
  # Read .csv files in directory
  files <- list.files(path_in)
  files <- files[grepl(".csv$", files)] 
  
  for (i in seq_along(files)) {
    # Read file
    df <- read.csv(paste0(path_in, files[i]), header = TRUE, stringsAsFactors = FALSE)
    
    # Clean file
    df <- clean_data(df)
    
    # save df as a .csv file
    write.csv(df, file = paste0(path_out, files[i]), row.names = FALSE)
    Sys.sleep(.2)   
  }
}
