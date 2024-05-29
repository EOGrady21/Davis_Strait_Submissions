# Merge 2020 data
# There was an issue where some data was duplicated in original 2020 datset, 
# causing an offset to TIC values throughout the entire dataset.
# It was cleaned up by Carrie-Ellen May 2024

# This script merges the new cleaned dataset (for bottle data)
# with the existing table of CTD and other metadata

# This should be a one time issue
library(tidyverse)

# read in both data files
data2020 <- readr::read_csv('../data/2020_Davis_Strait_EO.csv')
upd_data <- readxl::read_xlsx('../data/2020 Davis St all chem final_corrected.xlsx')

ognames <- names(data2020)[19:22]
dataog <- data2020 %>%
  select(-c(19:22)) 

datanew <- upd_data %>%
  select(c(3, 5, 46:49)) %>%
  rename('sample ID' = 'Sample ID...3') %>%
  rename('event #' = 'event') 

names(datanew)[3:6] <- ognames


# merge them

data <- right_join(dataog, datanew)

# check for duplicated IDs left over

length(unique(data$`sample ID`))

#data[which(duplicated(data$`sample ID`)),]

# export file
write.csv(data, '../data/2020_Davis_Strait_EO_v2.csv', row.names = FALSE)

# rerun BCD generation
# rerun BCS generation

# reload to BioChem
#reload to OCADS