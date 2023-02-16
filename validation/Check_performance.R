library(osfr)
source("~/Projects/ASySD/validation/calculate_performance_function.R")
library(readr)
library(dplyr)
library(ASySD)

# get labelled duplicate sets
asysd_validation_project <- osf_retrieve_node("https://osf.io/2b8uq/")
files <- osf_ls_files(asysd_validation_project)
osf_download(files[7,], path="validation")

# used to validate and check false positives/negatives for ASySD v2
SRSR_duplicates_labelled <- read_csv("validation/duplicates_labelled/SRSR_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Diabetes_duplicates_labelled <- read_csv("validation/duplicates_labelled/Diabetes_duplicates_labelled.csv", col_types = cols(record_id = col_character()))

# test sets for ASySD v2 (not used for further developments)
NeuroImaging_duplicates_labelled <- read_csv("validation/duplicates_labelled/NeuroImaging_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Cardiac_duplicates_labelled <- read_csv("validation/duplicates_labelled/Cardiac_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Depression_duplicates_labelled <- read_csv("validation/duplicates_labelled/Depression_duplicates_labelled.csv", col_types = cols(record_id = col_character()))

calculate_performance(SRSR_duplicates_labelled)
calculate_performance(NeuroImaging_duplicates_labelled)
calculate_performance(Cardiac_duplicates_labelled)
calculate_performance(Depression_duplicates_labelled)
calculate_performance(Diabetes_duplicates_labelled)

