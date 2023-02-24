library(osfr)
source("validation/calculate_performance_function.R")
library(readr)
library(dplyr)
library(ASySD)

# get labelled duplicate sets from OSF project
asysd_validation_project <- osf_retrieve_node("https://osf.io/2b8uq/")
files <- osf_ls_files(asysd_validation_project)
osf_download(files[7,], path="validation")

# used to validate and check initial ASySD version #1
SRSR_duplicates_labelled <- read_csv("validation/duplicates_labelled/SRSR_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Diabetes_duplicates_labelled <- read_csv("validation/duplicates_labelled/Diabetes_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
NeuroImaging_duplicates_labelled <- read_csv("validation/duplicates_labelled/NeuroImaging_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Cardiac_duplicates_labelled <- read_csv("validation/duplicates_labelled/Cardiac_duplicates_labelled.csv", col_types = cols(record_id = col_character()))
Depression_duplicates_labelled <- read_csv("validation/duplicates_labelled/Depression_duplicates_labelled.csv", col_types = cols(record_id = col_character()))


# check performance hasn't changed substantially or has only improved
# check both types of dedup - merge and keep one
calculate_performance_1(SRSR_duplicates_labelled)
calculate_performance_2(SRSR_duplicates_labelled)

calculate_performance_1(NeuroImaging_duplicates_labelled)
calculate_performance_2(NeuroImaging_duplicates_labelled)

calculate_performance_1(Cardiac_duplicates_labelled)
calculate_performance_2(Cardiac_duplicates_labelled)

calculate_performance_1(Diabetes_duplicates_labelled)
calculate_performance_2(Diabetes_duplicates_labelled)

calculate_performance_1(Depression_duplicates_labelled)
calculate_performance_2(Depression_duplicates_labelled)

