#### MANUAL CHECKS DATA PROCESSING SCRIPT ####

## Load data ##

data <- read.csv(here::here("data", "manual_validation_raw.csv"), sep = ";")

## Load packages ##
library(tidyverse)
library(lubridate)

#### RENAMING COLUMNS ####
# Some columns had additional characters and were renamed:
data <- data |>
  rename(has_summary_results_reg1_main = has_summary_results_reg1._main,
         has_summary_results_reg1_sensitivity = has_summary_results_reg1._sensitivity,
         has_summary_results_reg2_sensitivity = has_summary_results_reg2._sensitivity)

#### CORRECTING INVALID DATE ####
# After analyzing missing data in both completion_month_year_reg1 and completion_month_year_reg2
# one case in completion_month_year_reg2 was identified as a error in recording: 2012-04-31, April only has 30 days. 
# the correct day on the registry is 2012-04, and per protocol we assigned the last day of the month for cases where
# only year and month was recorded. The value should be then replaced with 2012-04-30

#identify row index of the invalid date
row_index <- which(data$completion_date_reg2 == "2012-04-31")

#replace value with new one
data$completion_date_reg2[row_index] <- "2012-04-30"

#### CREATING COMPLETION MONTH-YEAR COLUMNS ####
# For a discrepancy analysis, the decision was to compare completion date between registries at the level of year-month, excluding the day. 
# two columns extracted this information for the future analysis.

data <- data |>
  mutate(
    completion_month_year_reg1 = format(as.Date(completion_date_reg1, format = "%Y-%m-%d"), "%Y-%m"),
    completion_month_year_reg2 = format(as.Date(completion_date_reg2, format = "%Y-%m-%d"), "%Y-%m")
  ) |>
  relocate(completion_month_year_reg1, .after = completion_date_reg1) |>
  relocate(completion_month_year_reg2, .after = completion_date_reg2)

#### DATA TRANSFORMATION ####
# The aim of this transformation is to mutate the columns registry1 and registry 2, so that registry1 always show EUCTR, 
# and that registry2 shows either ClinicalTrials.gov or DRKS. For this, all the columns that refer to the registries will 
# also have to be flipped.

# Columns that need to be modified: 
# trn1, trn2, registry1, registry2, completion_date_reg1, completion_date_type_reg1, completion_month_year_reg1, completion_month_year_reg2,
# completion_date_type_reg2, completion_month_year_reg2, completion_month_year_reg2, recruitment_status_reg1, overall_recruitment_status_reg1,
# recruitment_status_reg2, overall_recruitment_status_reg2, has_summary_results_reg1_main, has_summary_results_reg1_sensitivity,
# has_summary_results_reg2_main, has_summary_results_reg2_sensitivity

#SPLIT THE DATASET
#extract data to transform
drks_ctgov_data <- data |> filter(registry1 != "EUCTR")

#keep rows in data where registry1 == EUCTR
euctr_data <- data |> filter(registry1 == "EUCTR") 

#CREATE FUNCTION FOR COLUMN FLIP
flip_columns <- function(data, col1, col2, col1_position = NULL, col2_position = NULL) {
  data |>
    mutate(
      !!paste0(col1, "_new") := !!sym(col2),         #create temporary columns to host the data
      !!paste0(col2, "_new") := !!sym(col1)) |>
    select(-all_of(c(col1, col2))) |>                #drop old columns
    relocate(
      !!sym(paste0(col1, "_new")),                   #relocate columns in new order
      .before = all_of(col1_position)) |>
    relocate(
      !!sym(paste0(col2, "_new")),                
      .after = all_of(col2_position)) |>
    rename(
      !!col1 := !!sym(paste0(col1, "_new")),         #rename to original column name
      !!col2 := !!sym(paste0(col2, "_new")))
}

#APPLYING FUNCTION
data_transformed <- drks_ctgov_data |>
  flip_columns("trn1", "trn2", 
               col1_position = 1, col2_position = "trn1_new") |>
  flip_columns("registry1", "registry2", 
               col1_position = "trn2", col2_position = "registry1_new") |>
  flip_columns("completion_date_reg1", "completion_date_reg2", 
               col1_position = "second_rater_comment", col2_position = "completion_date_type_reg1") |>
  flip_columns("completion_month_year_reg1", "completion_month_year_reg2", 
               col1_position = "completion_date_reg1", col2_position = "completion_date_reg2") |>
  flip_columns("completion_date_type_reg1", "completion_date_type_reg2", 
               col1_position = "completion_month_year_reg1", col2_position = "completion_month_year_reg2") |>
  flip_columns("recruitment_status_reg1", "recruitment_status_reg2", 
               col1_position = "completion_date_type_reg2", col2_position = "overall_recruitment_status_reg1") |>
  flip_columns("overall_recruitment_status_reg1", "overall_recruitment_status_reg2", 
               col1_position = "recruitment_status_reg1", col2_position = "recruitment_status_reg2") |>
  flip_columns("has_summary_results_reg1_main", "has_summary_results_reg2_main", 
               col1_position = "overall_recruitment_status_reg2", col2_position = "has_summary_results_reg1_main_new") |>
  flip_columns("has_summary_results_reg1_sensitivity", "has_summary_results_reg2_sensitivity", 
               col1_position = "has_summary_results_reg2_main", col2_position = "has_summary_results_reg1_sensitivity_new")

#### QUALITY CHECK OF TRANSFORMATION ####
# The idea is to compare the columns from data_transformed with the columns from drks_ctgov_data (before transformation)

#CREATE FUNCTION FOR QUALITY CHECK
compare_columns <- function(data1, col1, data2, col2) {
  if (setequal(data1[[col1]], data2[[col2]])) {
    message(sprintf("Columns %s and %s contain the same values", col1, col2))
  } else {
    message(sprintf("Columns %s and %s do NOT contain the same values", col1, col2))
  }
}

#APPLYING FUNCTION
#define column pairs
column_pairs <- list(
  c("trn1", "trn2"),
  c("registry1", "registry2"),
  c("completion_date_reg1", "completion_date_reg2"),
  c("completion_month_year_reg1", "completion_month_year_reg2"),
  c("completion_date_type_reg1", "completion_date_type_reg2"),
  c("recruitment_status_reg1", "recruitment_status_reg2"),
  c("overall_recruitment_status_reg1", "overall_recruitment_status_reg2"),
  c("has_summary_results_reg1_main", "has_summary_results_reg2_main"),
  c("has_summary_results_reg1_sensitivity", "has_summary_results_reg2_sensitivity")
)

#perform comparisons
for (pair in column_pairs) {
  compare_columns(data_transformed, pair[1], drks_ctgov_data, pair[2])
  compare_columns(data_transformed, pair[2], drks_ctgov_data, pair[1])
}

#### RE-JOINING FINAL DATASET FOR ANALYSIS ####
joined_data <- bind_rows(euctr_data, data_transformed)

### EXPORT FINAL DATASET
write.csv(joined_data, here::here("data", "manual_validation_processed.csv"), row.names = FALSE)
