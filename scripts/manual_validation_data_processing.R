#### MANUAL CHECKS DATA PROCESSING SCRIPT ####

library(readxl)
library(dplyr)
library(readr)
library(here)

#### LOAD THE MANUAL VALIDATION DATASET ####

# Note that this throws the following warning: "Expecting date in AD232 / R232C30: got '2012-04-31'" and converted the original date (which is invalid) '2012-04-31' to NA. 
# We correct this to '2012-04-30' later.

data <- read_xlsx(here("data", "manual_validation_raw.xlsx"),
                  col_types = c("text", "text", "text", "text", "numeric",
                                "text", "text", "text", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "text", "text", "text", "logical",
                                "text", "text", "text", "date", "text",
                                "text", "text", "date", "text", "date",
                                "text", "text", "text", "text", "text",
                                "logical", "logical", "logical", "logical",
                                "text"))

#### RENAMING COLUMNS ####

# Some column names in the Excel sheet have white spaces and need to be renamed
names(data) <- str_replace_all(names(data), c(" " = "."))

data <- data |> 
  rename(has_summary_results_reg1_main = has_summary_results_reg1._main,
         has_summary_results_reg1_sensitivity = has_summary_results_reg1._sensitivity,
         has_summary_results_reg2_sensitivity = has_summary_results_reg2._sensitivity)

#### IMPLEMENT CORRECTIONS ####

# Correct date for NCT01510704 from NA to 2012-04-30
data[data$trn2 == "NCT01510704",]$completion_date_reg2 <- as.Date("2012-04-30", format = "%Y-%m-%d")

# For NCT00351403 - 2006-000358-38, `completion_date_type_reg1` should be changed from “Global” to “Actual”
data[data$trn1 == "NCT00351403",]$completion_date_type_reg1 <- "Actual"

# For 2009-017520-88 - NCT01231854, `has_summary_results_reg1 _main` should be FALSE
data[data$trn1 == "2009-017520-88",]$has_summary_results_reg1_main <- FALSE

#For 2012-004555-36 - NCT01797861, information regarding “no DE protocol available” needs to be moved from the population_comment column to the general_comment column

  # Paste text in general_comment column
data$general_comment[data$trn1 == "2012-004555-36"] <- paste(data$population_comment[data$trn1 == "2012-004555-36"],
                                                             data$general_comment[data$trn1 == "2012-004555-36"])
  # Delete text from population_comment column
data$population_comment[data$trn1 == "2012-004555-36"] <- ""

# For 2009-014076-22 - NCT01065246, value from general_comment (no DE protocol available) needs to be moved to the correct row: 2012-002699-14 - NCT01883531

  # Paste text in the right row
data$general_comment[data$trn1 == "2012-002699-14"] <- paste(data$general_comment[data$trn1 == "2009-014076-22"],
                                                             data$general_comment[data$trn1 == "2012-002699-14"])
  # Delete comment from population_comment column 
data$general_comment[data$trn1 == "2009-014076-22"] <- ""

#### CREATE ROUNDED COMPLETION DATE COLUMNS FOR DISCREPANCY ANALYSIS ####

data <- data |> 
  mutate(
    completion_month_year_reg1 = lubridate::floor_date(completion_date_reg1, unit = "month"),
    completion_month_year_reg2 = lubridate::floor_date(completion_date_reg2, unit = "month")
  ) |> 
  relocate(completion_month_year_reg1, .after = completion_date_reg1) |>
  relocate(completion_month_year_reg2, .after = completion_date_reg2)

#### DERIVE OVERALL RECRUITMENT STATUS ####

# Remove manually encoded overall recruitment status columns
data <- data |> 
  select (
    -overall_recruitment_status_reg1,
    -overall_recruitment_status_reg2
  ) |>
  # Re-generate these columns computationally
  mutate(
    overall_recruitment_status_reg1 = case_when(
      recruitment_status_reg1 %in% c("Completed", "Prematurely Ended", "Terminated", "Recruiting complete", "Recruiting stopped") ~ "Completed",
      recruitment_status_reg1 %in% c("Ongoing", "Temporarily Halted", "Restarted") ~ "Ongoing",
      recruitment_status_reg1 %in% c("Unknown status") ~ "Other"
    )
  ) |>
  mutate(
    overall_recruitment_status_reg2 = case_when(
      recruitment_status_reg2 %in% c("Completed", "Prematurely Ended", "Terminated", "Recruiting complete", "Recruiting stopped") ~ "Completed",
      recruitment_status_reg2 %in% c("Ongoing", "Temporarily Halted", "Restarted") ~ "Ongoing",
      recruitment_status_reg2 %in% c("Unknown status") ~ "Other"
    )
  ) |> 
  relocate(overall_recruitment_status_reg1, .after = recruitment_status_reg1) |>
  relocate(overall_recruitment_status_reg2, .after = recruitment_status_reg2)

#Note: in the DRKS registry, "Recruiting complete" can be followed by either "Recruitment complete, study complete" or "Recruiting complete, study continuing".
# for the DRKS trials reviewed, they were only the first case, and were recorded as "Recruiting complete" in the column recruitment_status_reg1 and 
#recruitment_status_reg2.


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
