# FIX EVENT idS AFTER GETTING SAVED IN EXCEL BY ACCIDENT


library(tidyverse)

# read in the BCD and BCS file

fns <- list.files('C:/Users/ogradye/Documents/Davis_Strait/data/BioChem', pattern = 'BCD')

for (i in 1:length(fns)){
dat <- read_csv(paste0('C:/Users/ogradye/Documents/Davis_Strait/data/BioChem/', fns[i]),
                # READ ALL COLUMNS AS CHARACTER
                col_types = cols(.default = col_character()))

if (length(grep(fns[i],pattern = 'BCD')) > 0) {

  # add empty columns
  dat <- dat %>%
    mutate('DIS_DETAIL_DETECTION_LIMIT' = NA) %>%
    mutate('DIS_DETAIL_DETAIL_COLLECTOR' = NA)
  
  # pad 0s in events again
  dat$EVENT_COLLECTOR_EVENT_ID <- as.character(str_pad(as.character(dat$EVENT_COLLECTOR_EVENT_ID), 3, pad = '0'))
  
  # reorder BCD columns
  # order columns
  bcd_columns <-
    c(
      "DIS_DATA_NUM",
      "MISSION_DESCRIPTOR",
      "EVENT_COLLECTOR_EVENT_ID",
      "EVENT_COLLECTOR_STN_NAME",
      "DIS_HEADER_START_DEPTH",
      "DIS_HEADER_END_DEPTH",
      "DIS_HEADER_SLAT",
      "DIS_HEADER_SLON",
      "DIS_HEADER_SDATE",
      "DIS_HEADER_STIME",
      "DIS_DETAIL_DATA_TYPE_SEQ",
      "DATA_TYPE_METHOD",
      "DIS_DETAIL_DATA_VALUE",
      "DIS_DETAIL_DATA_QC_CODE",
      "DIS_DETAIL_DETECTION_LIMIT",
      "DIS_DETAIL_DETAIL_COLLECTOR",
      "DIS_DETAIL_COLLECTOR_SAMP_ID",
      "CREATED_BY",
      "CREATED_DATE",
      "DATA_CENTER_CODE",
      "PROCESS_FLAG",
      "BATCH_SEQ",
      "DIS_SAMPLE_KEY_VALUE"
    )
  dat <- dat %>%
    select(all_of(bcd_columns))
  
} else {
  
  dat$EVENT_COLLECTOR_EVENT_ID <- str_pad(dat$EVENT_COLLECTOR_EVENT_ID, 3, pad = '0')
  
  bcs_columns <-
    c(
      "DIS_SAMPLE_KEY_VALUE",
      "MISSION_DESCRIPTOR",
      "EVENT_COLLECTOR_EVENT_ID",
      "EVENT_COLLECTOR_STN_NAME",
      "MISSION_NAME",
      "MISSION_LEADER",
      "MISSION_SDATE",
      "MISSION_EDATE",
      "MISSION_INSTITUTE",
      "MISSION_PLATFORM",
      "MISSION_PROTOCOL",
      "MISSION_GEOGRAPHIC_REGION",
      "MISSION_COLLECTOR_COMMENT1",
      "MISSION_COLLECTOR_COMMENT2",
      "MISSION_DATA_MANAGER_COMMENT",
      "EVENT_SDATE",
      "EVENT_EDATE",
      "EVENT_STIME",
      "EVENT_ETIME",
      "EVENT_MIN_LAT",
      "EVENT_MAX_LAT",
      "EVENT_MIN_LON",
      "EVENT_MAX_LON",
      "EVENT_UTC_OFFSET",
      "EVENT_COLLECTOR_COMMENT1",
      "EVENT_COLLECTOR_COMMENT2",
      "EVENT_DATA_MANAGER_COMMENT",
      "DIS_HEADR_GEAR_SEQ",
      "DIS_HEADR_SDATE",
      "DIS_HEADR_EDATE",
      "DIS_HEADR_STIME",
      "DIS_HEADR_ETIME",
      "DIS_HEADR_TIME_QC_CODE",
      "DIS_HEADR_SLAT",
      "DIS_HEADR_ELAT",
      "DIS_HEADR_SLON",
      "DIS_HEADR_ELON",
      "DIS_HEADR_POSITION_QC_CODE",
      "DIS_HEADR_START_DEPTH",
      "DIS_HEADR_END_DEPTH",
      "DIS_HEADR_SOUNDING",
      "DIS_HEADR_COLLECTOR_DEPLMT_ID",
      "DIS_HEADR_COLLECTOR_SAMPLE_ID",
      "DIS_HEADR_COLLECTOR",
      "DIS_HEADR_COLLECTOR_COMMENT1",
      "DIS_HEADR_DATA_MANAGER_COMMENT",
      "DIS_HEADR_RESPONSIBLE_GROUP",
      "DIS_HEADR_SHARED_DATA",
      "CREATED_BY",
      "CREATED_DATE",
      "DATA_CENTER_CODE",
      "PROCESS_FLAG",
      "BATCH_SEQ"
    )
  
  dat <- dat %>%
    select(all_of(bcs_columns))
  
}



# write the file back to csv (all character strings)

write_csv(dat, paste0('C:/Users/ogradye/Documents/Davis_Strait/data/BioChem/', fns[i]),na = ''
              )

}
