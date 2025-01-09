#### MANUAL CHECKS DATA PROCESSING SCRIPT ####

library(readxl)
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

#### RENAMING COLUMNS ####

# Some column names in the Excel sheet have white spaces and need to be renamed
names(data) <- stringr::str_replace_all(names(data), c(" " = "."))

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

# For 2012-004555-36 - NCT01797861, information regarding “no DE protocol available” needs to be moved from the population_comment column to the general_comment column

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
  flip_columns("overall_recruitment_status_reg1", "overall_recruitment_status_reg2") |>
  flip_columns("has_summary_results_reg1_main", "has_summary_results_reg2_main") |>
  flip_columns("has_summary_results_reg1_sensitivity", "has_summary_results_reg2_sensitivity")

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
  c("overall_recruitment_status_reg1", "overall_recruitment_status_reg2"),
  c("has_summary_results_reg1_main", "has_summary_results_reg2_main"),
  c("has_summary_results_reg1_sensitivity", "has_summary_results_reg2_sensitivity"))

# Perform comparisons
for (pair in column_pairs) {
  compare_columns(data_transformed, pair[1], drks_ctgov_data, pair[2])
  compare_columns(data_transformed, pair[2], drks_ctgov_data, pair[1])
}

#### RE-JOINING DATASET ####

joined_data <- bind_rows(euctr_data, data_transformed)

#### CREATE COLUMN FOR SECOND SENSITIVITY ANALYSIS SUMMARY RESULTS ####

# Note: this analysis is currently under review: the comment_summary_results values are being double checked.

# The purpose is to add a second sensitivity analysis, where we included any document linked (including publications added either automatically 
# or manually) as TRUE for having summary results reported. The text in comment_summary_results is going to be used to identify all cases that would be 
# false in this second sensitivity analysis, and everything else will be considered TRUE for having summary results.

# Create new column

joined_data <- joined_data |>
  mutate(has_summary_results_reg1_sensitivity_v2 = if_else(
    str_detect(comment_summary_results, 
               "euctr no results|euctr: link to a summary that does not resolve|euctr link that does not resolve|euctr 
               has link that does not resolve|euctr link to report that does not resolve|euctr links a synopsis that does 
               not resolve|euctr link to synopsis that does not resolve|euctr link to summary trial results that does not resolve"),
    FALSE, TRUE),
    has_summary_results_reg2_sensitivity_v2 = if_else(
      str_detect(comment_summary_results, 
                 "drks no results|ctgov no results|ctgov results submitted to ClinicalTrials.gov but not posted|ctgov results submitted 
                 but not posted|drks two links to publication that do not resolve"),
      FALSE, TRUE)) |>
  relocate(has_summary_results_reg1_sensitivity_v2, 
           .after = has_summary_results_reg1_sensitivity) |>
  relocate(has_summary_results_reg2_sensitivity_v2, 
           .after = has_summary_results_reg2_sensitivity)

# Validate new column value: verifying all columns of summary results with specific values in comment_summary_results

spot_check_summary_results <- joined_data |>                         
  select(has_summary_results_reg1_main, has_summary_results_reg1_sensitivity, 
         has_summary_results_reg1_sensitivity_v2, has_summary_results_reg2_main,
         has_summary_results_reg2_sensitivity, has_summary_results_reg2_sensitivity_v2,
         comment_summary_results)

spot_check_summary_results |>
  mutate(
    reg1_condition = str_detect(
      comment_summary_results,
      str_c("euctr no results",
            "euctr: link to a summary that does not resolve",
            "euctr link that does not resolve",
            "euctr has link that does not resolve",
            "euctr link to report that does not resolve",
            "euctr links a synopsis that does not resolve",
            "euctr link to synopsis that does not resolve",
            "euctr link to summary trial results that does not resolve",
            collapse = "|")),
    reg2_condition = str_detect(
      comment_summary_results,
      str_c("ctgov results submitted to ClinicalTrials.gov but not posted",
            "ctgov results submitted but not posted",
            "drks two links to publication that do not resolve",
            "ctgov no results",
            "drks no results",
            collapse = "|"))) |>
  filter(
    (reg1_condition & 
       !(has_summary_results_reg1_main == FALSE &
           has_summary_results_reg1_sensitivity == FALSE &
           has_summary_results_reg1_sensitivity_v2 == FALSE)) |
      (reg2_condition & 
         !(has_summary_results_reg2_main == FALSE &
             has_summary_results_reg2_sensitivity == FALSE &
             has_summary_results_reg2_sensitivity_v2 == FALSE))) -> non_matching_rows

#print result
if (nrow(non_matching_rows) == 0) {
  print("All columns are FALSE for the relevant conditions.")
} else {
  print("Not all columns are FALSE for the relevant conditions. Failed rows:")
  print(non_matching_rows)
}

#### CREATE COLUMN FOR SENSITIVITY ANALYSIS COMPLETION DATE ####

# This is a sensitivity analysis of completion date of EUCTR trn's, based on the one that was present in the German member state protocol instead of the result section.

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

# Discrepancy analysis check
# In the manual_validation_results.qmd file we analyze discrepancies between registries only for manually validated cross-registrations (n = 233)
# Only 227 observations (out of the 233) are included in euctr_data_filtered: 

joined_data |>
  filter(is_true_crossreg == TRUE,
         trn1 %in% euctr_data_filtered$eudract_number) 

# These 6 observations did not have a DE protocol available:

joined_data |>
  filter(is_true_crossreg == TRUE,
         !trn1 %in% euctr_data_filtered$eudract_number) |>
  select(trn1, general_comment)

# Add data from date_of_the_global_end_of_the_trial to final_data
joined_data <- joined_data |>
  left_join(euctr_data_filtered |>
              select(eudract_number, 
                     completion_date_protocol = date_of_the_global_end_of_the_trial),
            by = c("trn1" = "eudract_number"))

# Analysis of missing data in completion_date_protocol column for cases with both "Completed" recruitment status

joined_data |>
  filter(overall_recruitment_status_reg1 == "Completed" & overall_recruitment_status_reg2 == "Completed") |>
  summarise(missing_count = sum(is.na(completion_date_protocol) | completion_date_protocol == ""))

# 28 rows have missing data:

joined_data |>
  select(trn1, 
         general_comment,
         overall_recruitment_status_reg1, 
         overall_recruitment_status_reg2,
         completion_date_protocol) |>
  filter(overall_recruitment_status_reg1 == "Completed" & overall_recruitment_status_reg2 == "Completed") |>
  filter(is.na(completion_date_protocol) | completion_date_protocol == "") |>
  print(n = Inf)

# The 6 NA in the completion_date_protocol column are the rows that do not have a DE protocol.
# The remaining 22 rows are blank. Upon inspecting them on EUCTR website I found that in protocol they have completed status and NO global end of trial. 
# If I clicked results, then there was always a date available there, which could explain why in main analysis we did not encounter this issue for 
# overall enrollment complete trials in more than 8 cases. 

# Review of completion_date_reg1 missing data for cases with both "Completed" recruitment status:

joined_data |>
  mutate(completion_date_reg1 = as.character(completion_date_reg1)) |>
  select(trn1, 
         general_comment,
         overall_recruitment_status_reg1, 
         overall_recruitment_status_reg2,
         completion_date_reg1) |>
  filter(overall_recruitment_status_reg1 == "Completed" & overall_recruitment_status_reg2 == "Completed") |>
  filter(is.na(completion_date_reg1) | completion_date_reg1 == "" | completion_date_reg1 == "N/A") |>
  print(n = Inf)

# There are 8 cases of missing completion date, but complete recruitment status.

# Upon manually checking the 28 missing from euctr_data, I verified that each euctr trn that had no date on both protocol and results, was also labelled 
# as N/A in the manually extracted completion date. Therefore, of the 28 missing from Nick's data, 6 are not DE protocol, 14 are missing completion date 
# on the protocol, and 8 are missing their completion date on both protocol and results section in EUCTR registry page.

# Create rounded completion date column for completion_date_protocol

class(joined_data$completion_date_protocol)   # Check type of data for completion_date_protocol - it is a character. It needs to be "POSIXct" "POSIXt" 

joined_data$completion_date_protocol <- as.POSIXct(joined_data$completion_date_protocol, format = "%Y-%m-%d") # Convert to desired class

# Now we can round the completion_date_protocol column to year-month:

joined_data <- joined_data |> 
  mutate(
    completion_date_protocol = lubridate::floor_date(completion_date_protocol, unit = "month"),
  ) |> 
  relocate(completion_date_protocol, .after = completion_month_year_reg1)

### EXPORT FINAL DATASET

#write.csv(joined_data, here::here("data", "manual_validation_processed.csv"), row.names = FALSE)