# Davis Strait raw data checks

# IN DEVELOPMENT

library(tidyverse)
library(gt)

data_fns <- choose.files(default = '../data', caption = "Select data files: ", multi = TRUE)

for (i in 1:length(data_fns)) {

  data <- read_csv(data_fns[i], col_types = cols (.default = "c"))
  
  
# check for all required metadata columns

# check for empty data columns

# check for entries in reference tables eg expos.csv

# check for unique sample IDs
  samp_id_col <- grep(names(data), pattern = 'sample', ignore.case = TRUE)
  if (nrow(data) != length(unique(data[[samp_id_col]]))) {
    dup_ids <- data[[samp_id_col]][which(duplicated(data[[samp_id_col]]))]
    dup_index <- which(duplicated(data[[samp_id_col]]))
    
    # check that data is actually duplicated
    for (ii in 1:length(dup_index)) {
      cat('Duplicated sample ID found! \n')
      cat(paste(dup_ids[ii], '\n'))
      tt <- gt(data[data[[samp_id_col]] == dup_ids[ii],])
    print(tt)
    ans <- menu(title = 'Should the duplicate row be removed?',choices = c('Yes', 'No') )
    if (ans == 1) {
      # make sure that data is merged between rows and data is not lost
      dupdata <- data[data[[samp_id_col]] == dup_ids[ii],]
      result <- data.frame(sapply(dupdata, function(x) coalesce(x[1], x[2]))) 
      new_row <- as.data.frame(t(result))
      data[data[[samp_id_col]] == dup_ids[ii], ] <- new_row
      
      data <- data[-dup_index[ii],]
    }
    }
  }
  
  new_data <- data
  
  still_duped <- which(duplicated(data[[samp_id_col]]))

# check times
  dir.create('preprocessed_data')
  file_name <- str_split_i(data_fns[i], pattern = "\\\\", i = -1)
  write.csv(data, file = file.path('preprocessed_data', file_name), row.names = FALSE)
}
