# OCADS metadata file
library(readr)
library(readxl)
library(openxlsx)
library(tidyverse)

# read in other dependency tables
OA_lab_meta <- read_xlsx('extdata/OA_lab_metadata_CEG.xlsx',
                         sheet = 1)

crm <- read_csv('extdata/CRM.csv',
                show_col_types = FALSE)

investigators <- read_xlsx("extdata/investigators.xlsx")

additional_vars <- read_xlsx('extdata/additional_variables.xlsx')

# pull mission level info
general_md <- read_xlsx('extdata/DS_OCADS_metadata_KAS.xlsx',
                        sheet = 1)

# get blank metadata form
md_blank <- read_xlsx('extdata/SubmissionForm_OADS_v7.xlsx', sheet = 1, skip = 1)
md_filled <- md_blank

data_fns <- list.files("../data/OCADS/", pattern = '26D420200830_data', full.names = TRUE)

for (i in 1:length(data_fns)) {
  # get associated data file
  data <- read_csv(data_fns[i], show_col_types = FALSE)
  data_year <- format(as.Date(data$DATE[2]), '%Y')
  
# fill in metadata from dependencies
#submission date
md_filled[1,]$`Your input` <- format(Sys.Date(), '%Y-%m-%d')

# accession number
#md_filled[2,]$`Your input`

#investigator 1
md_filled[3,]$`Your input` <- "Kumiko Azetsu-Scott"
md_filled[4,]$`Your input` <- 'Bedford Institute of Oceanography'
md_filled[5,]$`Your input` <- "1 Challenger Dr Dartmouth NS Canada"
md_filled[7,]$`Your input` <- "kumiko.azetsu-scott@dfo-mpo.gc.ca"
md_filled[8,]$`Your input` <- "0000-0002-1466-6386"
md_filled[9,]$`Your input` <- "ORCID"

# investigator 2
investigators_rel <- investigators[which(investigators$year %in% data_year),]
md_filled[10,]$`Your input` <- investigators_rel$name
md_filled[11,]$`Your input` <- investigators_rel$institute
md_filled[12,]$`Your input` <- investigators_rel$address
md_filled[14,]$`Your input` <- investigators_rel$email

#investigator 3

# data submitter
md_filled[24,]$`Your input` <- "Emily O'Grady"
md_filled[25,]$`Your input` <- 'Bedford Institute of Oceanography'
md_filled[26,]$`Your input` <- "1 Challenger Dr Dartmouth NS Canada"
md_filled[28,]$`Your input` <- "emily.ogrady@dfo-mpo.gc.ca"
md_filled[29,]$`Your input` <- "0000-0002-1072-5879"
md_filled[30,]$`Your input` <- 'ORCID'

# Title
md_filled[31,]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Title'])
# Abstract
md_filled[32,]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Abstract'])
# Purpose
md_filled[33, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Purpose'])
# start date
md_filled[34, ]$`Your input` <- as.character(min(as.Date(data$DATE), na.rm = TRUE))
# end date
md_filled[35, ]$`Your input` <- as.character(max(as.Date(data$DATE), na.rm = TRUE))

# geographic limits
md_filled[36, ]$`Your input` <-  as.character(max(data$LONGITUDE, na.rm = TRUE))
md_filled[37, ]$`Your input` <- as.character(min(data$LONGITUDE, na.rm = TRUE))
md_filled[38, ]$`Your input` <- as.character(max(data$LATITUDE, na.rm = TRUE))
md_filled[39, ]$`Your input` <- as.character(min(data$LATITUDE, na.rm = TRUE))

# geographic names
md_filled[41, ]$`Your input` <- 'Davis Strait'

# funding
md_filled[43, ]$`Your input` <- "Government of Canada: Fisheries and Oceans Canada"
md_filled[44, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == "Funding project title"])
md_filled[45, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Funding project ID (Grant no.)'])

# research projects
md_filled[46, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Research projects'])

# platform
md_filled[47, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Platform-1 name'])
md_filled[48, ]$`Your input` <- stringr::str_sub(
  na.omit(general_md[[data_year]][general_md$Field == 'EXPOCODE']),
  start = 1, end = 4)
md_filled[49, ]$`Your input` <- 'Research Vessel'
md_filled[50, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Platform-1 owner'])
md_filled[51, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'Platform-1 country'])

# expocode
md_filled[62, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == 'EXPOCODE'])
# cruise ID
md_filled[63, ]$`Your input` <- na.omit(unique(data$NAME))

# author list
md_filled[65, ]$`Your input` <- na.omit(general_md[[data_year]][general_md$Field == "Author list for citation"])

#variables ----

# check variables present in data
varnames <- names(data)
metadatacols <- c('NAME',
                  'CASTNO',
                  'LATITUDE',
                  'LONGITUDE',
                  'SAMPNO',
                  'SOUNDING',
                  'STNNBR',
                  'BTL_TIME',
                  'DATE',
                  'EXPOCODE')
varnames <- varnames[-which(varnames %in% metadatacols)]

# WOCE flag schemes
# QC flags - bottle
WOCE_btl =  '1 = Sample drawn from bottle but analysis not recieved; 2 = QC Performed: Acceptable Measurement; 3 = QC Performed: Questionable Measurement; 4 = QC Performed: Bad Measurement; 5 = QC Performed: Not Reported; 6 = Mean of replicate measurements; 7 = Manual chromatographic peak measurement; 8 = Irregular digital chromatographic peak integration; 9 = Not sampled'

# QC flags - CTD
WOCE_ctd =  '"" = No QC Performed;1 = Sample drawn from bottle but analysis not recieved; 2 = QC Performed: Acceptable Measurement; 3 = QC Performed: Questionable Measurement; 4 = QC Performed: Bad Measurement; 5 = QC Performed: Not Reported; 6 = Mean of replicate measurements; 7 = Manual chromatographic peak measurement; 8 = Irregular digital chromatographic peak integration; 9 = Not sampled'


# DIC

if ('TCARBN' %in% varnames){
  
  md_filled[68, ]$`Your input` <- 'TCARBN'
  md_filled[69, ]$`Your input` <- 'Profile'
  md_filled[72, ]$`Your input` <- data$TCARBN[1]
  md_filled[73, ]$`Your input` <- 'measured'
  md_filled[75, ]$`Your input` <- 'Niskin Bottle'
  md_filled[76, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Analyzing instrument"]
  md_filled[77, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Detailed sampling and analyzing information"]
  md_filled[79, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Standardization technique description"]
  md_filled[80, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Frequency of standardization"]
  md_filled[81, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: CRM manufacturer"]
  md_filled[82, ]$`Your input` <- crm$Batch[crm$Cruise == na.omit(unique(data$NAME))]
  md_filled[83, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Poison used to kill the sample"]
  md_filled[84, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Poison volume"]
  md_filled[85, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Poisoning correction description"]
  md_filled[86, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Uncertainty"]
  md_filled[87, ]$`Your input` <- WOCE_btl
  md_filled[88, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Method reference (citation)"]
  md_filled[89, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Researcher Name"]
  md_filled[90, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "DIC: Researcher Institution"]
  }

# TA
if ('ALKALI' %in% varnames) {
  md_filled[91, ]$`Your input` <- 'ALKALI'
  md_filled[92, ]$`Your input` <- 'Profile'
  md_filled[95, ]$`Your input` <- data$ALKALI[1]
  md_filled[96, ]$`Your input` <- 'Measured'
  md_filled[98, ]$`Your input` <- 'Niskin Bottle'
  md_filled[99, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Analyzing instrument"]
  md_filled[100, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Type of titration"]
  md_filled[101, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Cell type (open or closed)"]
  md_filled[102, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Curve fitting method"]
  md_filled[103, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Detailed sampling and analyzing information"]
  md_filled[105, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Standardization technique description"]
  md_filled[106, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Frequency of standardization"]
  md_filled[107, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: CRM manufacturer"]
  md_filled[108, ]$`Your input` <- crm$Batch[crm$Cruise == na.omit(unique(data$NAME))]
  md_filled[109, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Poison used to kill the sample"]
  md_filled[110, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Poison volume"]
  md_filled[111, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Poisoning correction description"]
  md_filled[113, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Uncertainty"]
  md_filled[114, ]$`Your input` <- WOCE_btl
  md_filled[115, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Method reference (citation)"]
  md_filled[116, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Researcher Name"]
  md_filled[117, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "TA: Researcher Institution"]
}

# ph
if ('PH_TOT' %in% varnames) {
  md_filled[118, ]$`Your input` <- 'PH_TOT'
  md_filled[119, ]$`Your input` <- 'Profile'
  md_filled[122, ]$`Your input` <- 'Measured'
  md_filled[124, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Sampling instrument"]
  md_filled[125, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Analyzing instrument"]
  md_filled[126, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: pH scale"]
  md_filled[127, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Temperature of measurement"]
  md_filled[128, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Detailed sampling and analyzing information"]
  md_filled[130, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Standardization technique description"]
  md_filled[131, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Frequency of standardization"]
  md_filled[132, ]$`Your input` <- crm$Batch[crm$Cruise == na.omit(unique(data$NAME))]
  md_filled[133, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Temperature of standardization"]
  md_filled[134, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Temperature correction method"]
  md_filled[135, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: at what temperature was pH reported"]
  md_filled[136, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Uncertainty"]
  md_filled[137, ]$`Your input` <- WOCE_btl
  md_filled[138, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Method reference (citation)"]
  md_filled[139, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Researcher Name"]
  md_filled[140, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pH: Researcher Institution"]
}

# pco2

if ('PCO2' %in% varnames) {
  md_filled[178, ]$`Your input` <- 'PCO2'
  md_filled[179, ]$`Your input` <- 'Profile'
  md_filled[182, ]$`Your input` <- data$PCO2[1]
  md_filled[183, ]$`Your input` <- 'measured'
  md_filled[186, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Analyzing instrument"]
  md_filled[187, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Storage method"]
  md_filled[188, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Seawater volume (mL)"]
  md_filled[189, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Headspace volume (mL)"]
  md_filled[190, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Temperature of measurement"]
  md_filled[191, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Detailed sampling and analyzing information"]
  md_filled[193, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Manufacturer of the gas detector"]
  md_filled[194, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Model of the gas detector"]
  md_filled[195, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Resolution of the gas detector"]
  md_filled[196, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Uncertainty of the gas detector"]
  md_filled[197, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Standardization technique description"]
  md_filled[198, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Frequency of standardization"]
  md_filled[199, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Temperature of standardization"]
  md_filled[200, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Manufacturer of standard gas"]
  md_filled[201, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Concentrations of standard gas"]
  md_filled[202, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Uncertainties of standard gas"]
  md_filled[203, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Water vapor correction method"]
  md_filled[204, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Temperature correction method"]
  md_filled[205, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: at what temperature was pCO2 reported"]
  md_filled[206, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Uncertainty"]
  md_filled[207, ]$`Your input` <- WOCE_btl
  md_filled[208, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Method reference (citation)"]
  md_filled[209, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Researcher Name"]
  md_filled[210, ]$`Your input` <- OA_lab_meta$BIO[OA_lab_meta$Field == "pCO2D: Researcher Institution"]
}

# other vars
# TODO loop through any other vars present
# minimal metadata (abbreviation, name, unit, uncertainty)
# read in from additional variables table

oa_vars <- c('TCARBN', 'ALKALI', 'PH_TOT', 'PCO2')
othervarnames <- varnames[-which(varnames %in% oa_vars)] # remove oa vars
othervarnames <- othervarnames[-grep(othervarnames, pattern = 'FLAG_W')] # remove flag columns

if (length(othervarnames) > 10) {
  cat('More than 10 additional variables! Metadata slots will be added!')
}

genericvarlist <- grep(md_filled$`Metadata element name`, pattern = 'Var1:', value = TRUE)
genericvarlist <- gsub(genericvarlist, pattern = 'Var1: ', replacement = "")

varnum <- length(othervarnames)

allvars <- list()

for (ii in 1:varnum) {
  # generate metadata list for each variable in a flexible way
  varlead <- paste0("Var", ii, ": ")
  varlist <- paste0(varlead, genericvarlist)
  
  # then fill based on data from additional vars table
  var_filled <- data.frame('name' = varlist, value = '')
  # abbreviation
  var_filled[1, ]$value <- othervarnames[ii]
  # full name
  var_filled[2, ]$value <- additional_vars$variable_name[additional_vars$abbreviation == othervarnames[ii]]
  # observation type
  var_filled[3, ]$value <- additional_vars$observation_type[additional_vars$abbreviation == othervarnames[ii]]
  #unit
  var_filled[5, ]$value <- data[[othervarnames[ii]]][1]
  # sampling instrument
  var_filled[8, ]$value <- additional_vars$sampling_instrument[additional_vars$abbreviation == othervarnames[ii]]
  # analyzing instrument
  var_filled[9, ]$value <- additional_vars$analyzing_instrument[additional_vars$abbreviation == othervarnames[ii]]
  # detail info
  var_filled[11, ]$value <- additional_vars$detailed_sampling_analyzing_info[additional_vars$abbreviation == othervarnames[ii]]
  # replicates
  var_filled[12, ]$value <- additional_vars$field_replicate_info[additional_vars$abbreviation == othervarnames[ii]]
  # uncertainty
  var_filled[13, ]$value <- additional_vars$uncertainty[additional_vars$abbreviation == othervarnames[ii]]
  # flags
  var_filled[14, ]$value <- ifelse(var_filled[8, ]$value == 'CTD',
                                   yes = WOCE_ctd,
                                   no = WOCE_btl)
  # method reference
  var_filled[15, ]$value <- additional_vars$method_reference[additional_vars$abbreviation == othervarnames[ii]]
  # researcher info
  var_filled[19, ]$value <- additional_vars$researcher_name[additional_vars$abbreviation == othervarnames[ii]]
  var_filled[20, ]$value <- additional_vars$researcher_institution[additional_vars$abbreviation == othervarnames[ii]]
  
  allvars[[ii]] <- var_filled
}

# get allvars_df ready to merge with md_filled
allvars_df <- do.call("rbind", allvars)
allvars_df <- allvars_df %>%
  dplyr::mutate('No'= seq(211, 210+length(allvars_df$name))) %>%
  dplyr::rename('Metadata element name' = name) %>%
  dplyr::rename('Your input' = value)

# fill vars into md_filled
md_filled_export <-  merge(md_filled[,1:3], allvars_df, by = c('No', 'Metadata element name'), all.x = TRUE, all.y = TRUE)
# Replace the 'Your input' values in md_filled with the corresponding values from allvars_df
md_filled_export$`Your input.x`[is.na(md_filled_export$`Your input.x`)] <- md_filled_export$`Your input.y`[is.na(md_filled_export$`Your input.x`)]
# clean up df
md_filled_export <- md_filled_export %>%
  dplyr::rename('Your input' = 'Your input.x') %>%
  dplyr::select('No', 'Metadata element name', 'Your input')

# export as xlsx
fn <- paste0(na.omit(unique(data$EXPOCODE)), '_metadata.xlsx')
write.xlsx(md_filled_export, file.path('../data/OCADS/', fn))

}
