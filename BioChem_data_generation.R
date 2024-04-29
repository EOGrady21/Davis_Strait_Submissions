# Translate Davis Strait data to BioChem BCD/BCS format
library(librarian)
shelf(tidyverse, readr, readxl, here, oce )


# read in dictionary tables
bcd_dict <- readxl::read_xlsx(here('extdata', 'biochem_dictionary.xlsx'),
                              sheet = 'BCD')
expos <- read_csv('extdata/expocodes.csv', show_col_types = FALSE)
flags <- read_xlsx(here('extdata', 'flag_dictionary.xlsx'))
flag_definitions <- read_csv('extdata/flags.csv', show_col_types = FALSE)
data_type_seq_table <- read_csv('extdata/biochem_data_types.csv', show_col_types = FALSE)

# read in original data
data_fns <- choose.files(default = '../data', caption = "Select data files: ", multi = TRUE)
#data_fns <- list.files('../data/', pattern = '_EO.csv', full.names = TRUE)


# BCD ----
# loop for all data files
for (i in 1:length(data_fns)) {
  
data <- read_csv(data_fns[i], show_col_types = FALSE)
# TODO check for empty columns before translating



# translate columns ----
bcd_dict_y <- bcd_dict[bcd_dict$original %in% names(data),]

# Create a named vector from the dictionary
name_changes <- setNames(bcd_dict_y$biochem, bcd_dict_y$original)

# Check which columns in df are not in the dictionary
missing_cols <- setdiff(colnames(data), bcd_dict_y$original)

# If there are any such columns, remove them and print a warning
if (length(missing_cols) > 0) {
  warning(paste("The following columns were not found in the dictionary and have been removed:",
                paste(missing_cols, collapse = ", ")))
  data <- data[, !(colnames(data) %in% missing_cols)]
}

# Use rename_with to rename the columns
data <- data %>% rename_with(~ name_changes[.x], everything())

# remove empty rows
data <- data[-which(is.na(data$MISSION_NAME)), ]

# gather metadata columns
bcd_meta <- data[names(data) %in% bcd_dict$biochem[bcd_dict$tag == 'metadata']]

# generate necessary columns ----

# DIS_SAMPLE_KEY_VALUE
# (mission_event_sampleID)

# check mission name - adjust to expocode
if (unique(bcd_meta$MISSION_NAME) %in% expos$cruise_name) {
  bcd_meta$MISSION_NAME <- expos$expocode[expos$cruise_name == unique(bcd_meta$MISSION_NAME)]
  bcd_meta$MISSION_DESCRIPTOR <- expos$MEDS[expos$expocode == unique(bcd_meta$MISSION_NAME)]
  
} else{
  stop('EXPOCODE not found, MISSION_NAME was left in original condition! \n
          No Mission Descriptor provided! \n
          Please add an entry to extdata/expocodes.csv')
}

# pad event number to three digits
bcd_meta$EVENT_COLLECTOR_EVENT_ID <- str_pad(bcd_meta$EVENT_COLLECTOR_EVENT_ID,
                                       width = 3,
                                       pad = '0')

# generate dis_sample_key_value
bcd_meta <- bcd_meta %>%
  dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste0(
    MISSION_NAME, '_', EVENT_COLLECTOR_EVENT_ID, '_', DIS_DETAIL_COLLECTOR_SAMP_ID
    ))


# EVENT_SDATE

bcd_meta$DIS_HEADER_SDATE <- make_date(bcd_meta$year, bcd_meta$month, bcd_meta$day)

# Convert the date to the format DD-MON-YY
bcd_meta$DIS_HEADER_SDATE <- format(bcd_meta$DIS_HEADER_SDATE, "%d-%b-%y")

# format time TODO Check format
bcd_meta$DIS_HEADER_STIME <- format(bcd_meta$DIS_HEADER_STIME, "%H%m")

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
# batch_seq = 0
bcd_meta$BATCH_SEQ <- '0'


# reformat to long data (with flags) ----

bcd_data <- data[names(data) %in% bcd_dict$biochem[bcd_dict$tag != 'metadata']]

bcd_wide <- cbind(bcd_meta, bcd_data)


# separate column types
data_cols <- names(bcd_wide)[
  which(names(bcd_wide) %in% bcd_dict$biochem[bcd_dict$tag %in% c('btl', 'ctd')])
  ]
qc_cols <- names(bcd_wide)[
  which(names(bcd_wide) %in% bcd_dict$biochem[bcd_dict$tag == 'qc'])
  ]
metadata_cols <- names(bcd_wide)[
  -which(names(bcd_wide) %in% bcd_dict$biochem[bcd_dict$tag %in% c('ctd', 'btl', 'qc')])
  ]

# make all data columns numeric
bcd_wide <- bcd_wide %>%
  mutate_at(vars(data_cols), as.numeric)


# Separate data and qc columns into two dataframes
data_df <- bcd_wide %>% select(all_of(c(metadata_cols, data_cols)))
qc_df <- bcd_wide %>% select(all_of(c(metadata_cols, qc_cols)))

# Pivot both dataframes to long format
data_long <- data_df %>% pivot_longer(cols = all_of(data_cols),
                                      names_to = "DATA_TYPE_METHOD",
                                      values_to = "DIS_DETAIL_DATA_VALUE")
qc_long <- qc_df %>% pivot_longer(cols = all_of(qc_cols),
                                  names_to = "DATA_TYPE_METHOD",
                                  values_to = "DIS_DETAIL_DATA_QC_CODE")

# Remove the '_qc' suffix from the METHOD column in the qc_long dataframe
qc_long$DATA_TYPE_METHOD <- str_remove(qc_long$DATA_TYPE_METHOD, "_qc")

# Join the data and qc dataframes
bcd_long <- left_join(data_long, qc_long, by = c("DATA_TYPE_METHOD",
                                                 as.character(metadata_cols)))

# remove any NA data rows
bcd_long <- bcd_long %>%
  dplyr::filter(!is.na(DIS_DETAIL_DATA_VALUE))
# Add more metadata in long format ----

# Depth
# calculate from pressure
pressure_data <- bcd_long %>%
  filter(., DATA_TYPE_METHOD == 'Pressure') %>%
  select(., DIS_SAMPLE_KEY_VALUE, DIS_DETAIL_DATA_VALUE)
pressure_data$DIS_HEADER_START_DEPTH <- oce::swDepth(pressure_data$DIS_DETAIL_DATA_VALUE,
                           latitude = bcd_long$DIS_HEADER_SLON[1])
pressure_data$DIS_HEADER_END_DEPTH <- oce::swDepth(pressure_data$DIS_DETAIL_DATA_VALUE,
                                                     latitude = bcd_long$DIS_HEADER_SLON[1])
pressure_data <- pressure_data %>%
  select(-DIS_DETAIL_DATA_VALUE) %>%
  distinct(., DIS_SAMPLE_KEY_VALUE, .keep_all = TRUE)

# join depth data 
bcd_final <- bcd_long %>%
  dplyr::left_join(pressure_data)


# clean up
remove(pressure_data, data_long, qc_long, data_df, qc_df)


# get dis_data_type_seq
for (ii in 1:length(bcd_final$DATA_TYPE_METHOD)) {
  bcd_final$DIS_DETAIL_DATA_TYPE_SEQ[ii] <- as.character(data_type_seq_table$DATA_TYPE_SEQ[data_type_seq_table$METHOD == bcd_final$DATA_TYPE_METHOD[ii]])
}

# add DIS_DATA_NUM 
bcd_final$DIS_DATA_NUM <- seq(1:length(bcd_final$MISSION_NAME))

# translate flags ----
for (ii in 1:length(bcd_final$DIS_DETAIL_DATA_QC_CODE)) {
  lab_qc <- bcd_final$DIS_DETAIL_DATA_QC_CODE[ii]
  
  if (!is.na(lab_qc) && nchar(lab_qc) < 4) { # grab biochem flag from dictionary if lab flag exists
    bc_qc <- flags$BioChem[grep(flags$lab, pattern = str_sub(lab_qc, 1, 2))]
  } else {
    if (is.na(lab_qc)) { # if no flag then assume acceptable
      bc_qc <- 1
    } else if (nchar(lab_qc) > 4) { # if there is text ask for user translation
      bc_qc <- menu(choices = flag_definitions$biochem_definition[flag_definitions$flag > 0 ], 
            title = paste("Translate this QC to BioChem flag: \n", lab_qc))
    }
  }
  bcd_final$DIS_DETAIL_DATA_QC_CODE[ii] <- bc_qc
}

# order columns
bcd_columns <- c('DIS_DATA_NUM',
                 'MISSION_DESCRIPTOR',
                 'MISSION_NAME',
                 'EVENT_COLLECTOR_EVENT_ID',
                 'EVENT_COLLECTOR_STN_NAME',
                 'DIS_HEADER_START_DEPTH',
                 'DIS_HEADER_END_DEPTH',
                 'DIS_HEADER_SLAT',
                 'DIS_HEADER_SLON',
                 'DIS_HEADER_SDATE',
                 'DIS_HEADER_STIME',
                 'DIS_DETAIL_DATA_TYPE_SEQ',
                 'DATA_TYPE_METHOD',
                 'DIS_DETAIL_DATA_VALUE',
                 'DIS_DETAIL_DATA_QC_CODE',
                 'DIS_DETAIL_COLLECTOR_SAMP_ID',
                 'MISSION_INSTITUTE',
                 'CREATED_BY',
                 'CREATED_DATE',
                 'DATA_CENTER_CODE',
                 'PROCESS_FLAG',
                 'BATCH_SEQ',
                 'DIS_SAMPLE_KEY_VALUE'
)

bcd_export <- bcd_final %>%
  select(all_of(bcd_columns))
# export data file
bcd_name <- file.path('../data/BioChem/', paste0(unique(bcd_export$MISSION_DESCRIPTOR), '_BCD.csv'))
write.csv(bcd_export, bcd_name, row.names = FALSE)

}

