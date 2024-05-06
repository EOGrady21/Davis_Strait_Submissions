# Generate BCS files
library(librarian)
shelf(tidyverse, readr, readxl, here, oce, marmap )

# read in dictionary tables ----

# gather from OSC cruise
oscdata <- read_xlsx('osccruise_davisstrait.xlsx')

expos <- read_csv('extdata/expocodes.csv', show_col_types = FALSE)

# dependency tables
bcs_dict <- readxl::read_xlsx(here('extdata', 'biochem_dictionary.xlsx'),
                              sheet = 'BCS')
bcd_dict <- readxl::read_xlsx(here('extdata', 'biochem_dictionary.xlsx'),
                              sheet = 'BCD')

bcs_columns <- c('DIS_SAMPLE_KEY_VALUE',
                 'MISSION_DESCRIPTOR',
                 'EVENT_COLLECTOR_EVENT_ID',
                 'EVENT_COLLECTOR_STN_NAME',
                 'MISSION_NAME',
                 'MISSION_LEADER',
                 'MISSION_SDATE',
                 'MISSION_EDATE',
                 'MISSION_INSTITUTE',
                 'MISSION_PLATFORM',
                 'MISSION_PROTOCOL',
                 'MISSION_GEOGRAPHIC_REGION',
                 'MISSION_COLLECTOR_COMMENT1',
                 'MISSION_COLLECTOR_COMMENT2',
                 'MISSION_DATA_MANAGER_COMMENT',
                 'EVENT_SDATE',
                 'EVENT_EDATE',
                 'EVENT_STIME',
                 'EVENT_ETIME',
                 'EVENT_MIN_LAT',
                 'EVENT_MAX_LAT',
                 'EVENT_MIN_LON',
                 'EVENT_MAX_LON',
                 'EVENT_UTC_OFFSET',
                 'EVENT_COLLECTOR_COMMENT1',
                 'EVENT_COLLECTOR_COMMENT2',
                 'EVENT_DATA_MANAGER_COMMENT',
                 'DIS_HEADR_GEAR_SEQ',
                 'DIS_HEADR_SDATE',
                 'DIS_HEADR_EDATE',
                 'DIS_HEADR_STIME',
                 'DIS_HEADR_ETIME',
                 'DIS_HEADR_TIME_QC_CODE',
                 'DIS_HEADR_SLAT',
                 'DIS_HEADR_ELAT',
                 'DIS_HEADR_SLON',
                 'DIS_HEADR_ELON',
                 'DIS_HEADR_POSITION_QC_CODE',
                 'DIS_HEADR_START_DEPTH',
                 'DIS_HEADR_END_DEPTH',
                 'DIS_HEADR_SOUNDING',
                 'DIS_HEADR_COLLECTOR_DEPLMT_ID',
                 'DIS_HEADR_COLLECTOR_SAMPLE_ID',
                 'DIS_HEADR_COLLECTOR',
                 'DIS_HEADR_COLLECTOR_COMMENT1',
                 'DIS_HEADR_DATA_MANAGER_COMMENT',
                 'DIS_HEADR_RESPONSIBLE_GROUP',
                 'DIS_HEADR_SHARED_DATA',
                 'CREATED_BY',
                 'CREATED_DATE',
                 'DATA_CENTER_CODE',
                 'PROCESS_FLAG',
                 'BATCH_SEQ' )

# read in all original data sources
# data
data_fn <- choose.files(default = '../data', caption = "Select data files: ", multi = TRUE)
raw_data <- read.csv(data_fn)
# remove empty rows
# raw_data <- raw_data[-which(is.na(raw_data[,1])), ]

# bcd
bcd_fn <- choose.files(default = '../data/BioChem',
                       #caption = paste('Select BCD file for', unique(raw_data$`Cruise ID`), '[', unique(raw_data$year), ']', ':')
                       )
bcd <- read.csv(bcd_fn)


# pull columns from bcd ----

bcs <- bcd %>%
  dplyr::select(., DIS_SAMPLE_KEY_VALUE,
                MISSION_DESCRIPTOR,
                EVENT_COLLECTOR_EVENT_ID,
                EVENT_COLLECTOR_STN_NAME,
                CREATED_BY,
                CREATED_DATE,
                DATA_CENTER_CODE,
                PROCESS_FLAG,
                BATCH_SEQ
                )

bcs$DIS_HEADR_SDATE <- bcd$DIS_HEADER_SDATE
bcs$DIS_HEADR_EDATE <- bcd$DIS_HEADER_SDATE
bcs$DIS_HEADR_STIME <- bcd$DIS_HEADER_STIME
bcs$DIS_HEADR_ETIME <- bcd$DIS_HEADER_STIME
bcs$DIS_HEADR_SLAT <- bcd$DIS_HEADER_SLAT
bcs$DIS_HEADR_ELAT <- bcd$DIS_HEADER_SLAT
bcs$DIS_HEADR_SLON <- bcd$DIS_HEADER_SLON
bcs$DIS_HEADR_ELON <- bcd$DIS_HEADER_SLON
bcs$DIS_HEADR_START_DEPTH <- bcd$DIS_HEADER_START_DEPTH
bcs$DIS_HEADR_END_DEPTH <- bcd$DIS_HEADER_END_DEPTH
bcs$DIS_HEADR_COLLECTOR_SAMPLE_ID <- bcd$DIS_DETAIL_COLLECTOR_SAMP_ID

# filter to unique dis_sample_key_values

bcs <- bcs %>%
  distinct(DIS_SAMPLE_KEY_VALUE, .keep_all = TRUE)


# fill standard values ----
bcs$MISSION_GEOGRAPHIC_REGION <- 'Davis Strait'
bcs$DIS_HEADR_TIME_QC_CODE <- 0
bcs$DIS_HEADR_POSITION_QC_CODE <- 0
bcs$DIS_HEADR_GEAR_SEQ <- '90000171' # CTD + Rosette
bcs$MISSION_INSTITUTE <- 'DFO-BIO'


# pull values from existing tables ----

# mission name
raw_name <- str_trim(unique(raw_data[grep(names(raw_data), pattern = 'cruise', ignore.case = TRUE)])[1,])
bcs$MISSION_NAME <- expos$biochem_name[expos$cruise_name %in% raw_name]

osc_year <- oscdata[oscdata$year == na.omit(unique(raw_data$year)), ]

bcs$MISSION_LEADER <- paste(osc_year$first_name, osc_year$last_name)
bcs$MISSION_SDATE <- format(osc_year$start_date, '%d-%b-%y')
bcs$MISSION_EDATE <- format(osc_year$end_date, '%d-%b-%y')
bcs$MISSION_PLATFORM <- osc_year$platform
bcs$MISSION_PROTOCOL <- osc_year$protocol
bcs$DIS_HEADR_RESPONSIBLE_GROUP <- osc_year$responsible_group

# match sounding by event
event_col <- grep('event', names(raw_data))
names(raw_data)[event_col] <- 'event'

raw_sounding <- raw_data %>%
  select(event, sounding) %>%
  distinct(event, .keep_all = TRUE)
bcs <- bcs %>%
  right_join(., raw_sounding, by = join_by( EVENT_COLLECTOR_EVENT_ID == event), relationship = 'many-to-many') %>%
  rename(., DIS_HEADR_SOUNDING = sounding)

remove(raw_sounding)

# TODO if sounding is not available, pull from NOAA bathymetry map
# initialize event_data_manager_comment
bcs$EVENT_DATA_MANAGER_COMMENT <- ''
miss_sound <- na.omit(unique(bcs$EVENT_COLLECTOR_EVENT_ID[is.na(bcs$DIS_HEADR_SOUNDING) | 
                                             is.null(bcs$DIS_HEADR_SOUNDING)]))
if (length(miss_sound) > 0 ) {
for (iii in 1:length(miss_sound)) {

miss_sound_loc <- c(bcs$DIS_HEADR_SLAT[bcs$EVENT_COLLECTOR_EVENT_ID == miss_sound[iii]][1],
                    bcs$DIS_HEADR_SLON[bcs$EVENT_COLLECTOR_EVENT_ID == miss_sound[iii]][1])

bathy_dat <- marmap::getNOAA.bathy(lon1 = miss_sound_loc[2], 
                      lon2 = miss_sound_loc[2]+0.25,
                      lat1 = miss_sound_loc[1],
                      lat2 = miss_sound_loc[1]+0.25,
                      resolution = 1 )

bathy_dat <- as.xyz(bathy_dat)

fill_sounding <- abs(round(mean(bathy_dat$V3), 0))

bcs$DIS_HEADR_SOUNDING[bcs$EVENT_COLLECTOR_EVENT_ID == miss_sound[iii]] <- fill_sounding
bcs$EVENT_DATA_MANAGER_COMMENT[bcs$EVENT_COLLECTOR_EVENT_ID == miss_sound[iii]] <- "Missing sounding filled based on approximate bathymetric data"
}
}

# pull event summary
event_table <- bcd %>%
  group_by(EVENT_COLLECTOR_EVENT_ID) %>%
  summarize(EVENT_SDATE = min(DIS_HEADER_SDATE),
            EVENT_EDATE = max(DIS_HEADER_SDATE),
            EVENT_STIME = min(DIS_HEADER_STIME),
            EVENT_ETIME = max(DIS_HEADER_STIME),
            EVENT_MIN_LAT = min(DIS_HEADER_SLAT),
            EVENT_MAX_LAT = max(DIS_HEADER_SLAT),
            EVENT_MIN_LON = min(DIS_HEADER_SLON),
            EVENT_MAX_LON = max(DIS_HEADER_SLON))
bcs <- bcs %>%
  left_join(event_table, by = 'EVENT_COLLECTOR_EVENT_ID') 

# write data_manager_comment 
# calculation of depth from pressure
bcs$MISSION_DATA_MANAGER_COMMENT <- "Depth calculated from pressure; start depth equals end depth; sounding manually input;"

# gather comment from raw data
event_comments <- raw_data %>%
  group_by(event) %>%
  reframe(EVENT_COLLECTOR_COMMENT1 = str_c(unique(notes), collapse = ';')) %>%
  filter(!is.na(EVENT_COLLECTOR_COMMENT1))

if (nrow(event_comments) > 0) {
bcs <- bcs %>%
  left_join(., event_comments, by = join_by( 'EVENT_COLLECTOR_EVENT_ID' == 'event'))
} else {
  bcs$EVENT_COLLECTOR_COMMENT1 <- ''
}


# initiate NA columns
na_cols <- bcs_columns[-which(bcs_columns %in% names(bcs))]

if (length(na_cols) > 0) {
bcs <- bcs %>%
  mutate(!!!setNames(rep('', length(na_cols)), na_cols))
warning(paste("Empty columns added to BCS file! \n", str_c(na_cols, collapse = '\n')))
}



# format & export ----
bcs_export <- bcs %>%
  select(all_of(bcs_columns))

if (length(which(is.na(bcs_export$MISSION_DESCRIPTOR))) > 0 ) {
  bcs_export <- bcs_export[-which(is.na(bcs_export$MISSION_DESCRIPTOR)), ]
}

# export
bcs_name <- file.path('../data/BioChem/', paste0(unique(bcs_export$MISSION_DESCRIPTOR), '_BCS.csv'))
write.csv(bcs_export, bcs_name, row.names = FALSE)

