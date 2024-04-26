# Generate BCS files

# read in dictionary table

# gather from OSC cruise

# read in all original data sources
# data
# metadata
# dependency tables

# generate columns
# DIS_HEADR_GEAR_SEQ
# generate based on dictionary tag?
btl_type <- bcd_dict$biochem[bcd_dict$tag == 'btl']
ctd_type <- bcd_dict$biochem[bcd_dict$tag == 'ctd']

btl_gear_seq <- '90000019'
ctd_gear_seq <- '90000171'

bcd_long <- bcd_long %>%
  dplyr::mutate(., case_when())


# fill standard values

# pull values from existing tables

# format

# export
