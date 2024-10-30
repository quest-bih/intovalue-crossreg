#### MANUAL CHECKS DATA PROCESSING SCRIPT ####

## Load data ##
data <- read.csv("~/Desktop/Research group Strech/intovalue-crossreg/data/manual_validation_dataset_raw.csv", sep = ";")

## Load packages ##
library(tidyverse)
library(lubridate)

#### RENAMING COLUMNS ####
# Some columns had additional characters and were renamed:
data <- data |>
  rename(has_summary_results_reg1_main = has_summary_results_reg1._main,
         has_summary_results_reg1_sensitivity = has_summary_results_reg1._sensitivity,
         has_summary_results_reg2_sensitivity = has_summary_results_reg2._sensitivity)

#### CREATING COMPLETION MONTH-YEAR COLUMNS ####
#for a discrepancy analysis, the decision was to compare completion date between registries at the level of year-month, excluding the day. 
#two columns extracted this information for the future analysis.

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

#Columns that need to be modified: 
# trn1, trn2, registry1, registry2, completion_date_reg1, completion_date_type_reg1, completion_month_year_reg1, completion_month_year_reg2,
# completion_date_type_reg2, completion_month_year_reg2, completion_month_year_reg2, recruitment_status_reg1, overall_recruitment_status_reg1,
# recruitment_status_reg2, overall_recruitment_status_reg2, has_summary_results_reg1_main, has_summary_results_reg1_sensitivity,
# has_summary_results_reg2_main, has_summary_results_reg2_sensitivity

#SPLIT THE DATASET
#extract data to transform
drks_ctgov_data <- data |> filter(registry1 != "EUCTR")

#keep rows in data where registry1 == EUCTR
euctr_data <- data |> filter(registry1 == "EUCTR") 

#REGISTRY FLIP
data_transformed <- drks_ctgov_data |>
  mutate(registry1_new = registry2,                  #create temporary columns to host the data
         registry2_new = registry1) |>
  select(-registry1, -registry2) |>                  #drop old columns
  relocate(registry1_new, .after = trn2) |>          #relocate columns
  relocate(registry2_new, .after = registry1_new) |>
  rename(registry1 = registry1_new,                  #rename to original column name
         registry2 = registry2_new)

#TRN FLIP
data_transformed <- data_transformed |>
  mutate(trn1_new = trn2,
         trn2_new = trn1) |>
  select(-trn1, -trn2) |>
  relocate(trn1_new, .before = 1) |>
  relocate(trn2_new, .after = trn1_new) |>
  rename(trn1 = trn1_new,
         trn2 = trn2_new)

#FLIP COMPLETION DATE
data_transformed <- data_transformed |>
  mutate(completion_date_reg1_new = completion_date_reg2,
         completion_date_reg2_new = completion_date_reg1) |>
  select(-completion_date_reg1, -completion_date_reg2) |>
  relocate(completion_date_reg1_new, .after = second_rater_comment) |>
  relocate(completion_date_reg2_new, .after = completion_date_type_reg1) |>
  rename(completion_date_reg1 = completion_date_reg1_new,
         completion_date_reg2 = completion_date_reg2_new)

#FLIP COMPLWTION DATE YEAR/MONTH
data_transformed <- data_transformed |>
  mutate(completion_month_year_reg1_new = completion_month_year_reg2,
         completion_month_year_reg2_new = completion_month_year_reg1) |>
  select(-completion_month_year_reg1, -completion_month_year_reg2) |>
  relocate(completion_month_year_reg1_new, .after = completion_date_reg1) |>
  relocate(completion_month_year_reg2_new, .after = completion_date_reg2) |>
  rename(completion_month_year_reg1 = completion_month_year_reg1_new,
         completion_month_year_reg2 = completion_month_year_reg2_new)

#FLIP COMPlETION DATE TYPE
data_transformed <- data_transformed |>
  mutate(completion_date_type_reg1_new = completion_date_type_reg2,
         completion_date_type_reg2_new = completion_date_type_reg1) |>
  select(-completion_date_type_reg1, -completion_date_type_reg2) |>
  relocate(completion_date_type_reg1_new, .after = completion_month_year_reg1) |>
  relocate(completion_date_type_reg2_new, .after = completion_month_year_reg2) |>
  rename(completion_date_type_reg1 = completion_date_type_reg1_new,
         completion_date_type_reg2 = completion_date_type_reg2_new)

#FLIP RECRUITMENT STATUS
data_transformed <- data_transformed |>
  mutate(recruitment_status_reg1_new = recruitment_status_reg2,
         recruitment_status_reg2_new = recruitment_status_reg1) |>
  select(-recruitment_status_reg1, -recruitment_status_reg2) |>
  relocate(recruitment_status_reg1_new, .after = completion_date_type_reg2) |>
  relocate(recruitment_status_reg2_new, .after = overall_recruitment_status_reg1) |>
  rename(recruitment_status_reg1 = recruitment_status_reg1_new,
         recruitment_status_reg2 = recruitment_status_reg2_new)

#FLIP OVERALL RECRUITMENT STATUS
data_transformed <- data_transformed |>
  mutate(overall_recruitment_status_reg1_new = overall_recruitment_status_reg2,
         overall_recruitment_status_reg2_new = overall_recruitment_status_reg1) |>
  select(-overall_recruitment_status_reg1, -overall_recruitment_status_reg2) |>
  relocate(overall_recruitment_status_reg1_new, .after = recruitment_status_reg1) |>
  relocate(overall_recruitment_status_reg2_new, .after = recruitment_status_reg2) |>
  rename(overall_recruitment_status_reg1 = overall_recruitment_status_reg1_new,
         overall_recruitment_status_reg2 = overall_recruitment_status_reg2_new)

#HAS SUMMARY RESULTS MAIN FLIP
data_transformed <- data_transformed |>
  mutate(has_summary_results_reg1_main_new = has_summary_results_reg2_main,
         has_summary_results_reg2_main_new = has_summary_results_reg1_main) |>
  select(-has_summary_results_reg1_main, -has_summary_results_reg2_main) |>
  relocate(has_summary_results_reg1_main_new, .after = overall_recruitment_status_reg2) |>
  relocate(has_summary_results_reg2_main_new, .after = has_summary_results_reg1_main_new) |>
  rename(has_summary_results_reg1_main = has_summary_results_reg1_main_new,
         has_summary_results_reg2_main = has_summary_results_reg2_main_new)

#HAS SUMMARY RESULTS SENSITIVITY FLIP
data_transformed <- data_transformed |>
  mutate(has_summary_results_reg1_sensitivity_new = has_summary_results_reg2_sensitivity,
         has_summary_results_reg2_sensitivity_new = has_summary_results_reg1_sensitivity) |>
  select(-has_summary_results_reg1_sensitivity, -has_summary_results_reg2_sensitivity) |>
  relocate(has_summary_results_reg1_sensitivity_new, .after = has_summary_results_reg2_main) |>
  relocate(has_summary_results_reg2_sensitivity_new, .after = has_summary_results_reg1_sensitivity_new) |>
  rename(has_summary_results_reg1_sensitivity = has_summary_results_reg1_sensitivity_new,
         has_summary_results_reg2_sensitivity = has_summary_results_reg2_sensitivity_new)

#### QUALITY CHECK OF TRANSFORMATION ####
# The idea is to compare the columns from data_transformed with the columns from drks_ctgov_data (before transformation)

#comparison trn
if (setequal(data_transformed$trn1, drks_ctgov_data$trn2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$trn2, drks_ctgov_data$trn1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison registry
if (setequal(data_transformed$registry1, drks_ctgov_data$registry2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$registry2, drks_ctgov_data$registry1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison completion date
if (setequal(data_transformed$completion_date_reg1, drks_ctgov_data$completion_date_reg2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$completion_date_reg2, drks_ctgov_data$completion_date_reg1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparsion completion date month/year
if (setequal(data_transformed$completion_month_year_reg1, drks_ctgov_data$completion_month_year_reg2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$completion_month_year_reg2, drks_ctgov_data$completion_month_year_reg1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison completion date type
if (setequal(data_transformed$completion_date_type_reg1, drks_ctgov_data$completion_date_type_reg2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$completion_date_type_reg2, drks_ctgov_data$completion_date_type_reg1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison recruitment status
if (setequal(data_transformed$recruitment_status_reg1, drks_ctgov_data$recruitment_status_reg2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$recruitment_status_reg2, drks_ctgov_data$recruitment_status_reg1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison overall recruitment status
if (setequal(data_transformed$overall_recruitment_status_reg1, drks_ctgov_data$overall_recruitment_status_reg2)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$overall_recruitment_status_reg2, drks_ctgov_data$overall_recruitment_status_reg1)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison has summary results main
if (setequal(data_transformed$has_summary_results_reg1_main, drks_ctgov_data$has_summary_results_reg2_main)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$has_summary_results_reg2_main, drks_ctgov_data$has_summary_results_reg1_main)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#comparison has summary results sensitivity 
if (setequal(data_transformed$has_summary_results_reg1_sensitivity, drks_ctgov_data$has_summary_results_reg2_sensitivity)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

if (setequal(data_transformed$has_summary_results_reg2_sensitivity, drks_ctgov_data$has_summary_results_reg1_sensitivity)) {
  print("Both columns contain the same values")
} else {
  print("The columns do not contain the same values")
}

#### RE-JOINING FINAL DATASET FOR ANALYSIS ####
joined_data <- bind_rows(euctr_data, data_transformed)


### EXPORT FINAL DATASET
write.csv(joined_data, "~/Desktop/Research group Strech/intovalue-crossreg/data/manual_validation_dataset_processed.csv",
          row.names = FALSE)
