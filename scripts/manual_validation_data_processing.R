#### MANUAL CHECKS DATA PROCESSING SCRIPT ####

library(readxl)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
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

# Convert dates to Date format with lubridate()
data$completion_date_reg1 <- ymd(data$completion_date_reg1)
data$completion_date_reg2 <- ymd(data$completion_date_reg2)
data$date_of_check <- ymd(data$date_of_check)

#### RENAMING COLUMNS ####

# Some column names in the Excel sheet have white spaces and need to be renamed
names(data) <- stringr::str_replace_all(names(data), c(" " = "."))

data <- data |> 
  rename(has_summary_results_reg1_main = has_summary_results_reg1._main,
         has_summary_results_reg1_sensitivity = has_summary_results_reg1._sensitivity,
         has_summary_results_reg2_sensitivity = has_summary_results_reg2._sensitivity)

#### IMPLEMENT CORRECTIONS ####

# Correct date for NCT01510704 from NA to 2012-04-30
data[data$trn2 == "NCT01510704",]$completion_date_reg2 <- ymd("2012-04-30")

# For NCT00351403 - 2006-000358-38, `completion_date_type_reg1` should be changed from “Global” to “Actual”
data[data$trn1 == "NCT00351403",]$completion_date_type_reg1 <- "Actual"

# For 2009-017520-88 - NCT01231854, `has_summary_results_reg1 _main` should be FALSE
data[data$trn1 == "2009-017520-88",]$has_summary_results_reg1_main <- FALSE

# For 2012-004555-36 - NCT01797861, information regarding “no DE protocol available” needs to be moved from the population_comment column to the general_comment column

  # Paste text in general_comment column
data$general_comment[data$trn1 == "2012-004555-36"] <- paste(data$population_comment[data$trn1 == "2012-004555-36"],
                                                             data$general_comment[data$trn1 == "2012-004555-36"])
  # Delete text from population_comment column
data$population_comment[data$trn1 == "2012-004555-36"] <- NA

# For 2009-014076-22 - NCT01065246, value from general_comment (no DE protocol available) needs to be moved to the correct row: 2012-002699-14 - NCT01883531

  # Paste text in the right row
data$general_comment[data$trn1 == "2012-002699-14"] <- paste(data$general_comment[data$trn1 == "2009-014076-22"],
                                                             data$general_comment[data$trn1 == "2012-002699-14"])
  # Delete comment from general_comment column 
data$general_comment[data$trn1 == "2009-014076-22"] <- NA

#### REMOVE SUMMARY RESULT COLUMNS ####
# These columns will be re-generated computationally in a section below.

data <- data |>
  select(
    -has_summary_results_reg1_main,
    -has_summary_results_reg2_main,
    -has_summary_results_reg1_sensitivity,
    -has_summary_results_reg2_sensitivity,
    -comment_summary_results
  )

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

# Note: in the DRKS registry, "Recruiting complete" can be followed by either "Recruitment complete, study complete" or "Recruiting complete, study continuing".
# for the DRKS trials reviewed, they were only the first case, and were recorded as "Recruiting complete" in the column recruitment_status_reg1 and 
# recruitment_status_reg2.

#### DATA TRANSFORMATION ####

# The aim of this transformation is to mutate the columns registry1 and registry 2, so that registry1 always show EUCTR, 
# and that registry2 shows either ClinicalTrials.gov or DRKS. For this, all the columns that refer to the registries will 
# also have to be flipped.

# Columns that need to be flipped: 
# trn1, trn2, registry1, registry2, 
# completion_date_reg1, completion_date_type_reg1, completion_month_year_reg1, 
# completion_date_reg2, completion_date_type_reg2, completion_month_year_reg2,
# recruitment_status_reg1, overall_recruitment_status_reg1,
# recruitment_status_reg2, overall_recruitment_status_reg2, 
# has_summary_results_reg1_main, has_summary_results_reg1_sensitivity,
# has_summary_results_reg2_main, has_summary_results_reg2_sensitivity

# SPLIT THE DATASET
# Extract data to transform
drks_ctgov_data <- data |> filter(registry1 != "EUCTR")

# Keep rows in data where registry1 == EUCTR
euctr_data <- data |> filter(registry1 == "EUCTR") 

# CREATE FUNCTION FOR COLUMN FLIP
flip_columns <- function(data, col1, col2) {
  data |>
    mutate(
      temp = !!sym(col1),   # Temporarily store the value of col1
      !!col1 := !!sym(col2), # Assign col2 to col1
      !!col2 := temp         # Assign the temporary value to col2
    ) |>
    select(-temp)           # Remove the temporary column
}

# APPLYING FUNCTION
data_transformed <- drks_ctgov_data |>
  flip_columns("trn1", "trn2") |>
  flip_columns("registry1", "registry2") |>
  flip_columns("completion_date_reg1", "completion_date_reg2") |>
  flip_columns("completion_month_year_reg1", "completion_month_year_reg2") |>
  flip_columns("completion_date_type_reg1", "completion_date_type_reg2") |>
  flip_columns("recruitment_status_reg1", "recruitment_status_reg2") |>
  flip_columns("overall_recruitment_status_reg1", "overall_recruitment_status_reg2")

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
# Define column pairs
column_pairs <- list(
  c("trn1", "trn2"),
  c("registry1", "registry2"),
  c("completion_date_reg1", "completion_date_reg2"),
  c("completion_month_year_reg1", "completion_month_year_reg2"),
  c("completion_date_type_reg1", "completion_date_type_reg2"),
  c("recruitment_status_reg1", "recruitment_status_reg2"),
  c("overall_recruitment_status_reg1", "overall_recruitment_status_reg2"))

# Perform comparisons
for (pair in column_pairs) {
  compare_columns(data_transformed, pair[1], drks_ctgov_data, pair[2])
  compare_columns(data_transformed, pair[2], drks_ctgov_data, pair[1])
}

#### RE-JOINING DATASET ####

joined_data <- bind_rows(euctr_data, data_transformed)

#### CREATE COLUMNS FOR SUMMARY RESULTS ANALYSES ####

# Derive booleans for results reporting
sumres <- read_xlsx(here("data", "sumres.xlsx"),
                    col_types = c("text", "text", "text", "text",
                                  "logical", "text", "text", "logical",
                                  "logical", "logical", "text", "text",
                                  "text", "date"))

# Convert dates to Date format with lubridate()
sumres$date_of_check_sumres <- ymd(sumres$date_of_check_sumres)

sumres <- sumres |>
  mutate(
    # only TRUE if structured results
    has_summary_results_reg1_main = if_else(
      sumres_euctr == "structured_results", TRUE, FALSE
    ),
    has_summary_results_reg1_sensitivity = if_else(
      str_detect(sumres_euctr, "structured_results|synopsis_or_report|tabular_results_other_registry|statement_termination|uploaded_pub_citation|uploaded_pub_or_abstract"), 
      TRUE, FALSE
    ),
    # both sensitivity analyses are the same for EUCTR since we already include
    # links to publications in the summary results field
    has_summary_results_reg1_sensitivity_v2 = has_summary_results_reg1_sensitivity,
    # CTGOV main: TRUE if structured results
    # DRKS main: always FALSE as it doesn't offer structured results format
    has_summary_results_reg2_main = 
      if_else(
        registry2 == "ClinicalTrials.gov" & ctgov_results_overview_field == "structured_results",
        TRUE, FALSE
      ),
    has_summary_results_reg2_sensitivity = 
      # DRKS first sensitivity: TRUE if other format available including pub link in results field, otherwise default to main analysis (which is always FALSE)
      # CTGOV: main and first sensitivity analysis are the same since other formats can't be
      # uploaded in the results overview and we only consider ctgov pubs in second sensitivity analysis
      case_when(
        registry2 == "DRKS" & str_detect(drks_publication_of_study_results_field, "report|tabular_results_other_registry|publink_or_pubcitation") | 
          str_detect(drks_basic_reporting_field, "report|tabular_results_other_registry|publink_or_pubcitation") ~ TRUE, 
        .default = has_summary_results_reg2_main
      ),
    has_summary_results_reg2_sensitivity_v2 =
      # CTGOV: TRUE if there is a pub in the publications field, otherwise set it to
      # first sensitivity analysis (which is the same as main analysis)
      # DRKS: first and second sensitivity analysis are the same since we
      # already count publications in the first sensitivity analysis
      case_when(
        registry2 == "ClinicalTrials.gov" & ctgov_publications_field_pubmed | ctgov_publications_field_general | ctgov_publications_field_results ~ TRUE, 
        .default = has_summary_results_reg2_sensitivity
      )
  )

# Check assumptions
sumres_ctgov <- sumres |> filter(registry2 == "ClinicalTrials.gov")
sumres_drks <- sumres |> filter(registry2 == "DRKS")

# check that EUCTR first and second sensitivity analyses are always the same
if (FALSE %in% (sumres$has_summary_results_reg1_sensitivity == sumres$has_summary_results_reg1_sensitivity_v2)) {
  message("Something looks wrong, EUCTR first and second sensitivity analyses differ at points!")
} else {
  message("All good, EUCTR first and second sensitivity analyses are the same")
}

# check that DRKS first and second sensitivity analyses are the same
if (FALSE %in% (sumres_drks$has_summary_results_reg2_sensitivity == sumres_drks$has_summary_results_reg2_sensitivity_v2)) {
  message("Something looks wrong, DRKS first and second sensitivity analyses differ at points!")
} else {
  message("All good, DRKS first and second sensitivity analyses are the same")
}

# check that all DRKS entries for main analysis are FALSE
if (unique(sumres_drks$has_summary_results_reg2_main) != FALSE) {
  message("Something looks wrong, DRKS main analysis contains some TRUE values!")
} else {
  message("All good, DRKS main analysis is always FALSE")
}

# check that CTGOV main and first sensitivity analysis are the same
if (FALSE %in% (sumres_ctgov$has_summary_results_reg2_main == sumres_ctgov$has_summary_results_reg2_sensitivity)) {
  message("Something looks wrong, CTGOV main and first and sensitivity analyses differ at points!")
} else {
  message("All good, CTGOV main and first sensitivty analyses are the same")
}

# Add new columns to joined_data
joined_data <- joined_data |>
  left_join(
    sumres |> select(trn1, trn2, 
                     has_summary_results_reg1_main, has_summary_results_reg1_sensitivity, has_summary_results_reg1_sensitivity_v2,
                     has_summary_results_reg2_main, has_summary_results_reg2_sensitivity, has_summary_results_reg2_sensitivity_v2),
    by = c("trn1", "trn2")
  )

#### CREATE COLUMN FOR SENSITIVITY ANALYSIS COMPLETION DATE ####

# We process the data further to allow for a sensitivity analysis for discrepancies
# in completion date, where the completion date in EUCTR is only taken from the
# Germany (DE) country protocol in EUCTR (completion dates from results
# sections in EUCTR are disregarded; registrations with missing DE country protocols
# are excluded)

# Load data euctr protocol dataset
euctr_data <- read.csv(here::here("data", "euctr_dump.csv"), sep = ",")

# Check that all trn1 from joined_data are in euctr_data
missing_trn1 <- setdiff(joined_data$trn1, euctr_data$eudract_number)

if (length(missing_trn1) == 0) {
  cat("All trn1 values are included in eudract_number.\n")
} else {
  cat("The following trn1 values are missing from eudract_number:\n")
  print(missing_trn1)
}

# Filter variables and rows of interest from euctr_data
euctr_data_filtered <- euctr_data |>
  select(eudract_number_with_country, eudract_number, date_of_the_global_end_of_the_trial) |>
  filter(eudract_number %in% joined_data$trn1, 
         str_detect(eudract_number_with_country, "DE"))

# Convert dates to Date format with lubridate()
euctr_data_filtered$date_of_the_global_end_of_the_trial <- ymd(euctr_data_filtered$date_of_the_global_end_of_the_trial)

# Add completion date from EUCTR DE protocol (date_of_the_global_end_of_the_trial) to joined_data
joined_data <- joined_data |>
  left_join(euctr_data_filtered |>
              select(eudract_number, 
                     completion_date_protocol = date_of_the_global_end_of_the_trial),
            by = c("trn1" = "eudract_number"))

# Let's explore missing completion dates from the EUCTR dump for this discrepancy analysis
# The discrepancy analysis of completion date across registrations of the same trial is limited
# to manually validated cross-registrations where both registrations are "Completed"

discrepancy_data <- joined_data |>
  filter(
    is_true_crossreg == TRUE,
    overall_recruitment_status_reg1 == "Completed" & overall_recruitment_status_reg2 == "Completed"
  )

# There are 200 observations in the discrepancy analysis of completion date
# This includes 28 observations with a missing completion_date_protocol from the EUCTR dump
discrepancy_data |>
  count(is.na(completion_date_protocol))

# Let's explore missing the 28 completion_date_protocol data in the discrepancy data
#   - 6 of these do not have a DE protocol in the EUCTR dump
#   - 8 of these do not have a completion date on either the DE protocol or results (see `completion_date_type_reg1` == Missing)
#   - 14 of these do not have a completion date on the DE protocol

discrepancy_data |>
  select(trn1, 
         general_comment,
         completion_date_reg1,
         completion_date_type_reg1,
         completion_date_protocol) |>
  filter(is.na(completion_date_protocol)) |>
  arrange(general_comment, desc(completion_date_type_reg1)) |>
  print(n = Inf)

# Round the completion_date_protocol column:
joined_data <- joined_data |> 
  mutate(
    completion_month_year_protocol = lubridate::floor_date(completion_date_protocol, unit = "month"),
  ) |> 
  relocate(completion_month_year_protocol, .after = completion_month_year_reg1)

### EXPORT FINAL DATASET

#write_csv(joined_data, here::here("data", "manual_validation_processed.csv"))