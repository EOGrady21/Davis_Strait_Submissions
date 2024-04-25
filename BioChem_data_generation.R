# Translate Davis Strait data to BioChem BCD/BCS format
library(librarian)
shelf(tidyverse, readr, readxl, here)
source('functions.R')


# read in dictionary tables
bcs_dict <- readxl::read_xlsx(here('../', 'extdata', 'biochem_dictionary.xlsx'), sheet = 'BCS')
bcd_dict <- readxl::read_xlsx(here('../', 'extdata', 'biochem_dictionary.xlsx'), sheet = 'BCD')
expos <- read_csv('../extdata/expocodes.csv', show_col_types = FALSE)

# read in original data
# TODO make this more reproducible, cleaner, maybe file select
data_fns <- list.files('../data/', pattern = '_EO.csv', full.names = TRUE)


# BCD ----
# loop for all data files
for (i in 1:length(data_fns)) {
  
data <- read_csv(data_fns[i], show_col_types = FALSE)
# TODO check for empty columns before translating

# translate columns
bcd_dict_y <- bcd_dict[bcd_dict$original %in% names(data),]

# Create a named vector from the dictionary
name_changes <- setNames(bcd_dict_y$biochem, bcd_dict_y$original)

# Check which columns in df are not in the dictionary
missing_cols <- setdiff(colnames(data), bcd_dict_y$original)

# If there are any such columns, remove them and print a warning
if (length(missing_cols) > 0) {
  warning(paste("The following columns were not found in the dictionary and have been removed:", paste(missing_cols, collapse = ", ")))
  data <- data[, !(colnames(data) %in% missing_cols)]
}

# Use rename_with to rename the columns
data <- data %>% rename_with(~ name_changes[.x], everything())

# generate necessary columns ----

# DIS_SAMPLE_KEY_VALUE
# (mission_event_sampleID)

# check mission name - adjust to expocode
if (unique(data$MISSION_NAME) %in% expos$cruise_name) {
  data$MISSION_NAME <- expos$expocode[expos$cruise_name == unique(data$MISSION_NAME)]
} else{
  warning('EXPOCODE not found, MISSION_NAME was left in original condition! Please add an entry to extdata/expocodes.csv')
}

# pad event number to three digits
data$COLLECTOR_EVENT_ID <- str_pad(data$COLLECTOR_EVENT_ID, width = 3, pad = '0')

# generate dis_sample_key_value
data <- data %>%
  dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste0(MISSION_NAME, '_', COLLECTOR_EVENT_ID, '_', COLLECTOR_SAMPLE_ID))


# EVENT_SDATE

data$START_DATE <- make_date(data$year, data$month, data$day)

# Convert the date to the format DD-MON-YY
data$START_DATE <- format(data$START_DATE, "%d-%b-%y")

# CREATED_DATE
data$CREATED_DATE <- format(Sys.Date(), "%d-%b-%y")

# reformat to long data (with flags)

# DIS_HEADR_GEAR_SEQ



# fill in standard values
# MISSION_INSTITUTE = DFO BIO
# CREATED_BY = EMILY OGRADY
# DATA_CENTER_CODE = 20
# PROCESS_FLAG = NR

# pull out QC columns



# translate METHOD

# translate flags

# add base flags (1s, 9s)

# pull any notes into MISSION_COLLECTOR_COMMENT1

# order columns

# export data file

}
