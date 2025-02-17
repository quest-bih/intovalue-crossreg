#### TABLE GENERATION CROSS-REG ####  
# This file contains the code to generate a table with an overview of the amount of suspected cross-registrations. 
# These are broken down by priority, and by the following levels: overall, manually reviewed and confirmed true cross-registrations.

#### Load packages and data ####

library(tidyverse)
library(gt)

final_data <- read_csv(here::here("data", "manual_validation_processed.csv"))
trn_trn_table <- readRDS(here::here("data", "crossreg_pipeline_output.rds"))

# Filter trn_trn_table
filtered_table <- trn_trn_table |>
  filter(priority >= 1 & priority <= 4, # Select priorities with over 50% precision in pilot
         drks_removed == FALSE,         # Remove TRNs from trials deleted from DRKS registry
         euctr_id_in_euctr == TRUE,     # Select TRNs that resolve in EUCTR registry
         trn2 != "2008-004408-29")  

#### Prepare data for table ####

## Flip columns from filtered_table to match structure of final_data:

# SPLIT THE DATASET
# Extract data to transform
drks_ctgov_data <- filtered_table |> 
  filter(str_starts(trn1, "NCT") | str_starts(trn1, "DRKS"))

# Keep rows in data where registry1 == EUCTR
euctr_data <- filtered_table |> 
  filter(!str_starts(trn1, "NCT") & !str_starts(trn1, "DRKS"))


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
  flip_columns("trn1", "trn2")

# RE JOIN DATA  
filtered_table_flipped <- bind_rows(euctr_data, data_transformed)

## Further organization of the data

# Keep variables of interest
filtered_table_flipped <- filtered_table_flipped |>
  select(trn1, trn2, priority)

# Create columns registry1 and registry2
filtered_table_flipped <- filtered_table_flipped |>
  mutate(registry1 = "EUCTR",
         .after = trn2) |>
  mutate(registry2 = case_when(
    str_starts(trn2, "NCT") ~ "ClinicalTrials.gov",
    str_starts(trn2, "DRKS") ~ "DRKS"),
    .after = registry1)

# Left join is_true_cross_reg from final_data
filtered_table_flipped <- filtered_table_flipped |>
  left_join(final_data %>% select(trn1, trn2, is_true_crossreg), by = c("trn1", "trn2"))

# Create manually reviewed column
filtered_table_flipped <- filtered_table_flipped |>
  mutate(manually_reviewed = !is.na(is_true_crossreg),
         .after = priority)

# Rename dataset
data_table <- filtered_table_flipped

# Remove objects and functions no longer in use
remove(data_transformed, drks_ctgov_data, euctr_data, filtered_table, 
       trn_trn_table, final_data, filtered_table_flipped, flip_columns)

#### Table development ####
# Columns should be 1) overall, 2) manually reviewed and 3) Confirmed cross-reg
# Rows should be 1) by priority and 2) within each priority EUCTR-DRKS and EUCTR-ClinicalTrials.gov pairs.

# Count by priority and registry2
registry_counts <- data_table |>
  group_by(priority, registry2) |>
  summarise(
    overall = n(),
    reviewed = sum(manually_reviewed, na.rm = TRUE),
    true_crossreg = sum(is_true_crossreg, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(registry2 = factor(registry2, levels = c("ClinicalTrials.gov", "DRKS")))  # Set order (CTgov first; DRKS second)

# Compute total per priority
priority_totals <- data_table |>
  group_by(priority) |>
  summarise(
    overall = n(),
    reviewed = sum(manually_reviewed, na.rm = TRUE),
    true_crossreg = sum(is_true_crossreg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(registry2 = "Total")  # Label total rows

# Combine totals and registry breakdown
summary_table <- bind_rows(priority_totals, registry_counts) |>
  arrange(priority, desc(registry2 == "Total"), registry2) |> 
  mutate(category = ifelse(registry2 == "Total", paste("Priority", priority), registry2)) |>
  select(category, overall, reviewed, true_crossreg)  # Keep only relevant columns

# Rename priority values for the table
summary_table <- summary_table |> 
  mutate(category = recode(category, 
                           "Priority 1" = "Cross-registrations with bidirectional registry links", 
                           "Priority 2" = "Remaining cross-registrations with unidirectional registry link", 
                           "Priority 3" = "Remaining cross-registrations with approximate title matching",
                           "Priority 4" = "Remaining cross-registrations with match only on TRN in publication"))

# Create table
final_table <- summary_table |> 
  gt() |> 
  tab_header(
    title = "Table proposal"
  ) |> 
  cols_label(
    category = "",
    overall = md("Overall <br> (n = 625)"),
    reviewed = md("Manually screened <br> (n = 242)"),
    true_crossreg = md("Confirmed <br> (n = 232)")
  ) |> 
  fmt_number(
    columns = c(overall, reviewed, true_crossreg),
    decimals = 0
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),  # Bold the priority rows
    locations = cells_body(
      columns = category,
      rows = grepl("cross", category, ignore.case = TRUE)  # Detects rows that contain "cross"
    )
  ) |> 
  tab_style(
    style = cell_text(indent = px(20)),  # Indent registry names
    locations = cells_body(
      columns = category,
      rows = category %in% c("DRKS", "ClinicalTrials.gov")
    )
  ) |> 
  cols_width(
    overall ~ px(170),         
    reviewed ~ px(170),
    true_crossreg ~ px(170),
    category ~ px(250)
  ) |> 
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16)
  )

print(final_table)

#### Export table as word document ####
#gtsave(final_table, filename = "final_table.docx")
