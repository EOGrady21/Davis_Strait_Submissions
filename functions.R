# gather all unique variable names

check_variable_names <- function(data) {
  require(tidyverse)
  require(here)
  require(readr)
  if (!is.data.frame(data)){
    stop('data must be in dataframe format!')
  }
  
  # read in existing variable map
  map <- readr::read_csv(
    here('../extdata', 'data_column_dictionary.csv'),
    show_col_types = FALSE)
  
  data_names <- names(data)
  
  for (var in data_names) {
    if (var %in% map$original){
      # variable found
    } else {
      cat(paste(var, 'not found in data column map! \n'))
    }
  }
}

# Insert a new row at specific index of dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}