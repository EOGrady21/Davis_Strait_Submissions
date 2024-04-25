# generate OCADS data file
library(librarian)
shelf(tidyverse, readr, readxl)
source('functions.R')

# gather data files ----
data_fns <- list.files('../data/', pattern = '2021', full.names = TRUE)

# Load in dependencies

varmap <- read_csv('extdata/data_column_dictionary.csv',
                          show_col_types = FALSE)
flag_map <- read_csv('extdata/flags.csv',
                     show_col_types = FALSE)
expos <- read_csv('extdata/expocodes.csv',
                  show_col_types = FALSE)

# Check variable names in raw data
# for (i in 1:length(data_fns)) {
#   data <- readr::read_csv(data_fns[i], show_col_types = FALSE)
#   check_variable_names(data)
# }

# loop for data files 
for (i in 1:length(data_fns)) {
  
  data <- readr::read_csv(data_fns[i], show_col_types = FALSE)
  
  # remove any NA rows
  #data <- data[-which(is.na(data$`Cruise ID`)), ]
  
  # pull out date columns and reformat
  data$date <- paste(data$year, data$month, data$day, sep = '-')
  
  # add EXPOCODE
  data$EXPOCODE <- expos$expocode[grep(unique(data$`Cruise ID`), expos$cruise_name, ignore.case = TRUE)]

  #translate headers ----
  varmap_ind <- match(names(data), varmap$original)

  cchdo_names <- varmap$CCHDO[varmap_ind]

  expdata_names <- vector()

  for (ii in 1:length(cchdo_names)) {
    if (is.na(cchdo_names[ii])){
      expdata_names[ii] <- varmap$ocads[varmap_ind[ii]]
    } else {
      expdata_names[ii] <- cchdo_names[ii]
    }
  }
  
  # throw warning if any names are NA
  if (any(is.na(expdata_names))) {
    cat('WARNING: NA variable names will be removed from exported data! \n Removing columns from data:\n')
    cat(paste('\t', names(data)[which(is.na(expdata_names))], '\n'))
  }
  
  expdata <- data
  names(expdata) <- expdata_names
  expdata <- expdata[, which(!is.na(expdata_names))]
  
  #translate flags ----
  # check which flag scheme is used in original data
  qc_cols <- grep(names(expdata), pattern = 'FLAG')
  # qc_vals <- na.omit(unique(unlist(as.vector(unique(expdata[, qc_cols])))))
  # cat(paste(head(qc_vals), '\n'))
  # 
  
  # Set flags to WOCE assumes lab flag scheme
  # loop through each flag column
  for (colind in qc_cols) {
    qc_col <- expdata[, colind]
    
  # B00 -> 1
  qc_col_WOCE = ifelse(qc_col == 'B00', 1, qc_col)
  # B01-B09 -> 2
  qc_col_WOCE = ifelse(grepl('B0', qc_col_WOCE) == 'TRUE', 2, qc_col_WOCE)
  # B10-B19 -> 5
  qc_col_WOCE = ifelse(grepl('B1', qc_col_WOCE) == 'TRUE', 5, qc_col_WOCE)
  # B20-B29 -> 3
  qc_col_WOCE = ifelse(grepl('B2', qc_col_WOCE) == 'TRUE', 3, qc_col_WOCE)
  # B30-B39 -> 6
  qc_col_WOCE = ifelse(grepl('B3', qc_col_WOCE) == 'TRUE', 6, qc_col_WOCE)
  # B40-B49 -> 4
  qc_col_WOCE = ifelse(grepl('B4', qc_col_WOCE) == 'TRUE', 4, qc_col_WOCE)
  # B50-B59 -> 7
  qc_col_WOCE = ifelse(grepl('B5', qc_col_WOCE) == 'TRUE', 7, qc_col_WOCE)
  # B60-B69 -> 9
  qc_col_WOCE = ifelse(grepl('B6', qc_col_WOCE) == 'TRUE', 9, qc_col_WOCE)
  # B70-B79 -> 8
  qc_col_WOCE = ifelse(grepl('B7', qc_col_WOCE) == 'TRUE', 8, qc_col_WOCE)
  
  
  # Set M flags to WOCE
  
  # M00 -> 1
  qc_col_WOCE = ifelse(qc_col == 'M00', 1, qc_col)
  # M01-M09 -> 9
  qc_col_WOCE = ifelse(grepl('M0', qc_col_WOCE) == 'TRUE', 9, qc_col_WOCE)
  # M20-M29 -> 3
  qc_col_WOCE = ifelse(grepl('M2', qc_col_WOCE) == 'TRUE', 5, qc_col_WOCE)
  # M40-M49 -> 4
  qc_col_WOCE = ifelse(grepl('M4', qc_col_WOCE) == 'TRUE', 4, qc_col_WOCE)
  # M60-M69 -> 9
  qc_col_WOCE = ifelse(grepl('M6', qc_col_WOCE) == 'TRUE', 3, qc_col_WOCE)
  # M80-M89 -> 8
  qc_col_WOCE = ifelse(grepl('M8', qc_col_WOCE) == 'TRUE', 2, qc_col_WOCE)
  
  # Set L and I flags to WOCE
  
  # BAD - L20-L29, I01-I10 -> 4
  qc_col_WOCE = ifelse(grepl('L2', qc_col_WOCE) == 'TRUE', 4, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I0', qc_col_WOCE) == 'TRUE', 4, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I1', qc_col_WOCE) == 'TRUE', 4, qc_col_WOCE)
  
  # QUESTIONABLE - L40-49, I20-I30 -> 3
  qc_col_WOCE = ifelse(grepl('L4', qc_col_WOCE) == 'TRUE', 3, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I2', qc_col_WOCE) == 'TRUE', 3, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I3', qc_col_WOCE) == 'TRUE', 3, qc_col_WOCE)
  
  # NOT REPORTED - L01-L09
  qc_col_WOCE = ifelse(grepl('L0', qc_col_WOCE) == 'TRUE', 5, qc_col_WOCE)
  
  # ACCEPTABLE - L60-L69, I40-I50
  qc_col_WOCE = ifelse(grepl('L6', qc_col_WOCE) == 'TRUE', 2, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I5', qc_col_WOCE) == 'TRUE', 2, qc_col_WOCE)
  qc_col_WOCE = ifelse(grepl('I4', qc_col_WOCE) == 'TRUE', 2, qc_col_WOCE)
  
  expdata[, colind] <- unlist(qc_col_WOCE)
  }
  
  expdata_qc <- expdata
  # add standard WOCE
  data_cols <- str_replace(names(expdata)[qc_cols], pattern = '_FLAG_W', replacement = '' )
  
  for (colname in data_cols) {
    qccolname <- paste0(colname, '_FLAG_W')
    
    # for missing data points, flag 9
    expdata_qc[[qccolname]][which(is.na(expdata_qc[, colname]))] <- 9
    
    # for all others, flag 2 (valid)
    expdata_qc[[qccolname]][which(is.na(expdata_qc[,qccolname]))] <- 2
    
    # WARNING I think some of this data is average of replicates, this would need a specific flag in WOCE
    # need to confirm with the lab which variables are averages
  }
  
  # if entire flag column is 9 (all missing data) - remove column from export
  
  for (qccolname in names(expdata)[qc_cols]) {
    if (length(unique(expdata_qc[[qccolname]])) == 1) {
      if (unique(expdata_qc[[qccolname]]) == 9) {
        datacolname <- str_replace(qccolname, pattern = '_FLAG_W', replacement = '')
        cat('Removing', datacolname, ', no non-missing data!  \n')
        
        remove_ind <- grep(names(expdata_qc), pattern = datacolname)
        #remove_ind <- unique(c(remove_ind, grep(names(expdata_qc), pattern = qccolname)))
        
        cat('Columns removed: \n')
        cat(names(expdata_qc)[remove_ind], '\n')
        
        expdata_qc <- expdata_qc[,-remove_ind]
      }
    }
  }
  
  # fill in NA (-999) values
  for (colname in names(expdata_qc)) {
    if (colname %in% varmap$CCHDO[varmap$tag == 'metadata']) {
      if (anyNA(expdata_qc[[colname]])) {
        cat('NA metadata value in', colname, '! \n')
        expdata_qc[[colname]][which(is.na(expdata_qc[[colname]]))] <- '-999'
      }
    } else {
      expdata_qc[[colname]][which(is.na(expdata_qc[[colname]]))] <- '-999'
    }
  }
  
 
# final formatting ----
  
  expdata_units <- varmap$CCHDO_units[match(names(expdata_qc), varmap$CCHDO)]
  expdata_final <- as.data.frame(lapply(expdata_qc, as.character))
  # fix names from character conversion
  names(expdata_final) <- names(expdata_qc)
  expdata_final <- insertRow(expdata_final, as.list(expdata_units), 1)
  
  # order columns (metadata, ctd, btl)
  colorder <- c(varmap$CCHDO[varmap$tag == 'metadata'],
                varmap$CCHDO[varmap$tag == 'ctd'],
                varmap$CCHDO[varmap$tag == 'btl']
  )
  colorder <- unique(colorder[colorder %in% names(expdata_final)])
  
  expdata_final <- expdata_final[, colorder]

  # WARNING THIS REMOVES ANY NON CCHDO COLUMNS
  cat('Columns removed! \n')
  cat(names(expdata_qc)[!names(expdata_qc) %in% names(expdata_final)])
  
  # export data file ----
  exp_fn <- paste0(na.omit(unique(expdata_final$EXPOCODE)), '_data.csv')
  write.csv(expdata_final, file = file.path('../data/OCADS', exp_fn), row.names = FALSE)
}
