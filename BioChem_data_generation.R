# Translate Davis Strait data to BioChem BCD/BCS format
library(librarian)
shelf(tidyverse, readr, readxl, here)
source('functions.R')


# read in dictionary tables
bcs_dict <- readxl::read_xlsx(here('extdata', 'biochem_dictionary.xlsx'), sheet = 'BCS')
bcd_dict <- readxl::read_xlsx(here('extdata', 'biochem_dictionary.xlsx'), sheet = 'BCD')
expos <- read_csv('extdata/expocodes.csv', show_col_types = FALSE)

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

# gather metadata columns
bcd_meta <- data[names(data) %in% bcd_dict$biochem[bcd_dict$tag == 'metadata']]

# generate necessary columns ----

# DIS_SAMPLE_KEY_VALUE
# (mission_event_sampleID)

# check mission name - adjust to expocode
if (unique(bcd_meta$MISSION_NAME) %in% expos$cruise_name) {
  bcd_meta$MISSION_NAME <- expos$expocode[expos$cruise_name == unique(bcd_meta$MISSION_NAME)]
} else{
  warning('EXPOCODE not found, MISSION_NAME was left in original condition! Please add an entry to extdata/expocodes.csv')
}

# pad event number to three digits
bcd_meta$COLLECTOR_EVENT_ID <- str_pad(bcd_meta$COLLECTOR_EVENT_ID, width = 3, pad = '0')

# generate dis_sample_key_value
bcd_meta <- bcd_meta %>%
  dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste0(MISSION_NAME, '_', COLLECTOR_EVENT_ID, '_', COLLECTOR_SAMPLE_ID))


# EVENT_SDATE

bcd_meta$START_DATE <- make_date(bcd_meta$year, bcd_meta$month, bcd_meta$day)

# Convert the date to the format DD-MON-YY
bcd_meta$START_DATE <- format(bcd_meta$START_DATE, "%d-%b-%y")

# remove year month day columns
bcd_meta <- bcd_meta[-grep(names(bcd_meta), pattern = 'year')]
bcd_meta <- bcd_meta[-grep(names(bcd_meta), pattern = 'month')]
bcd_meta <- bcd_meta[-grep(names(bcd_meta), pattern = 'day')]

# CREATED_DATE
bcd_meta$CREATED_DATE <- format(Sys.Date(), "%d-%b-%y")

# fill in standard values

# MISSION_INSTITUTE = DFO BIO
bcd_meta$MISSION_INSTITUTE <- 'DFO-BIO'
# CREATED_BY = EMILY OGRADY
bcd_meta$CREATED_BY <- "Emily O'Grady"
# DATA_CENTER_CODE = 20
bcd_meta$DATA_CENTER_CODE <- '20'
# PROCESS_FLAG = NR
bcd_meta$PROCESS_FLAG <- 'NR'


# reformat to long data (with flags) ----

bcd_data <- data[names(data) %in% bcd_dict$biochem[bcd_dict$tag != 'metadata']]

bcd_wide <- cbind(bcd_meta, bcd_data)

data_cols <- names(bcd_wide)[which(names(bcd_wide) %in% bcd_dict$biochem[bcd_dict$tag %in% c('btl', 'ctd')])]
qc_cols <- names(bcd_wide)[which(names(bcd_wide) %in% bcd_dict$biochem[bcd_dict$tag == 'qc'])]

# make all data columns numeric
bcd_wide <- bcd_wide %>%
  mutate_at(vars(data_cols), as.numeric)

bcd_long <- bcd_wide %>%
  pivot_longer(., all_of(data_cols), names_to = 'METHOD', values_to = 'DATA_VALUE') %>%
  pivot_longer()
# pull out QC columns



# DIS_HEADR_GEAR_SEQ

# Depth?


# translate flags

# add base flags (1s, 9s)

# pull any notes into MISSION_COLLECTOR_COMMENT1

# order columns

# export data file

}
