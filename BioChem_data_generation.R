# Translate Davis Strait data to BioChem BCD/BCS format
library(librarian)
shelf(tidyverse, readr, readxl)
source('functions.R')


# read in dictionary tables

# read in original data


# BCD ----
# generate necesary columns
# DIS_SAMPLE_KEY_VALUE
# EVENT_SDATE
# DIS_HEADR_GEAR_SEQ
# DIS_HEADR_SDATE
# CREATED_DATE

# fill in standard values
# MISSION_INSTITUTE = DFO BIO
# CREATED_BY = EMILY OGRADY
# DATA_CENTER_CODE = 20
# PROCESS_FLAG = NR

# pull out QC columns

# reformat to long data (with flags)

# translate METHOD

# translate flags

# add base flags (1s, 9s)

# pull any notes into MISSION_COLLECTOR_COMMENT1

# order columns

# export data file


