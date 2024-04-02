# OCADS metadata file


# TODO: update to connect directly to BioChem
# Biochem code metadata - download most recent at http://www.dfo-mpo.gc.ca/science/documents/data-donnees/biochem/BioChem_code-tables.zip
data_types = read.table('../extdata/BioChem_code_tables/BC_Data_Types_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
data_retrievals = read.table('../extdata/BioChem_code_tables/BC_Data_Retrievals_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
preservations = read.table('../extdata/BioChem_code_tables/BC_Preservations_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
storages = read.table('../extdata/BioChem_code_tables/BC_Storages_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
sample_handlings = read.table('../extdata/BioChem_code_tables/BC_Sample_Handlings_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
units = read.table('../extdata/BioChem_code_tables/BC_Units_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')
gear_types = read.table('../extdata/BioChem_code_tables/BC_Gears_17July2023.tsv', header = TRUE, sep = '\t')
analyses = read.table('../extdata/BioChem_code_tables/BC_Analyses_17July2023.tsv', header = TRUE, sep = '\t', comment.char = '')


OA_lab_meta <- read_xlsx('../extdata/OA_lab_metadata_CEG.xlsx',
                         sheet = 1)

crm <- read_csv('../extdata/CRM.csv',
                show_col_types = FALSE)

# read in other dependency tables

# pull mission level info

# pull info for each variable

# format with header and row numbers

# export as xlsx

