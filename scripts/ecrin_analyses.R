# A script to evaluate the degree of overlap between potential cross-registrations identified via our approach and 
# inferred cross-registrations based on data in the 
# Clinical Research Metadata Repository (MDR) from ECRIN (https://ecrin.org/clinical-research-metadata-repository)
library(tidyverse)
library(here)
library(ctregistries)
library(ggvenn)

# Function to standardize trial pairs, give them unique identifier
standardize_pairs <- function(df) {
  df |>
    rowwise() |>
    mutate(
      # Sort trial_id_1 and trial_id_2 alphabetically within each row to handle the order issue
      standardized_pair = paste(sort(c(trn1, trn2)), collapse = "_")
    ) |>
    ungroup()
}


# Load TRN manual checks first
potential_crossregs <- read_rds(here("data","crossreg_pipeline_output.rds"))

# Here, we are filtering for trials with a priority of 4 or lower (priority 4 = trial identifier mentioned in another trial's related publication), as our pilot manual review of trial pairs with priority > 4 revealed low precision for correctly identified true cross-registrations
# Unlike in other scripts, we do not filter out pairs that don't resolve in EUCTR or DRKS.
# We took this decision because the MDR does not make any claims about the validity of a cross registration 
# The MDR merely aggregates information across registries, including secondary identifiers. We then used that information to infer potential cross-registrations.
# Since the point of this analysis is not to screen and confirm cross-registrations, but 
# to identify how these two searching approaches overlap, it is appropriate to leave TRN pairs that may not resolve in a registry
potential_crossregs_filtered <- potential_crossregs |>
  filter(priority <= 4 )


# read in MDR information
ecrin <- read_csv(here("data", "20241025_mdr_identifiers.csv")) |>
  filter(identifier_type == "Trial registry ID") |>
  filter(identifier_source == "EU Clinical Trials Register")|>
  rename(trn1 = iv_id,
         trn2 = identifier) 

# num of distinct EUCTR trials
ecrin |>
  select(trn2) |>
  distinct() |>
  nrow()

# num of distinct IV trials
ecrin |>
  select(trn1) |>
  distinct() |>
  nrow()
  

# Apply the function to both datasets (do NOT filter for trials that resolve in TRN)
ecrin_standardized <- standardize_pairs(ecrin)
potential_crossregs_filtered_standardized <- standardize_pairs(potential_crossregs_filtered)

# Convert to vectors for easy comparison
ecrin_pairs_vector <- ecrin_standardized$standardized_pair
potential_crossregs_filtered_standardized_vector <- potential_crossregs_filtered_standardized$standardized_pair

#######################################################################################
# New, cleaner approach to Venn diagrams to visualize overlap between our approach and using data from the MDR

ecrin_venn_data <- list(
  "Approach of the present study" = potential_crossregs_filtered_standardized_vector,
  "MDR Data" = ecrin_pairs_vector
  )

base_ecrin_venn <- ggvenn(
 ecrin_venn_data,
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5,
  set_name_size = 0, # Turn off default labels for custom handling
 auto_scale = TRUE
)

base_ecrin_venn +
  annotate("label", x = -1.5, y = 0.3, label = "Approach of the present study",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.2, y = 0.3, label = "MDR Data",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines"))

#######################################################################################
# Here we identify the trial pairs unique to the MDR data and our approach, and where our approaches overlapped

common_pairs <- intersect(ecrin_pairs_vector, potential_crossregs_filtered_standardized_vector)
unique_to_ecrin <- setdiff(ecrin_pairs_vector, potential_crossregs_filtered_standardized_vector)
unique_to_crossreg_pipeline <- setdiff(potential_crossregs_filtered_standardized_vector, ecrin_pairs_vector)


# Summary counts
total_ecrin <- length(ecrin_pairs_vector)
total_trn <- length(potential_crossregs_filtered_standardized_vector)
common_count <- length(common_pairs)
unique_to_ecrin_count <- length(unique_to_ecrin)
unique_to_crossreg_pipeline_count <- length(unique_to_crossreg_pipeline)

###################################################################################################
# Data exploration section

# Including this table allows us to identify further overlap between MDR results, and trial pairs 
# identified by our crossreg pipeline approach. Some TRN pairs found "exclusively" by the MDR were also identified
# in our data, but excluded from `potential_crossregs_filtered` for having a Priority > 4
potential_crossregs_standardized <- standardize_pairs(potential_crossregs)


# Create data frames of the TRN pairs unique to the MDR, and to the crossreg pipeline approach, and of the overlap in these approaches
unique_to_ecrin_df <- ecrin_standardized[!ecrin_standardized$standardized_pair %in% potential_crossregs_filtered_standardized$standardized_pair, ]
unique_to_crossreg_pipeline_df <- potential_crossregs_filtered_standardized[!potential_crossregs_filtered_standardized$standardized_pair %in% ecrin_standardized$standardized_pair, ]

mdr_crossreg_pipeline_overlap <- intersect(potential_crossregs_standardized |> select(standardized_pair), 
                                           ecrin_standardized |> select(standardized_pair))

# Add unique flag to the three data frames to merge with larger tables
unique_to_crossreg_pipeline_df <- unique_to_crossreg_pipeline_df |>
  mutate(unique_to_trn = 1) |>
  select(standardized_pair, unique_to_trn)

unique_to_ecrin_df <- unique_to_ecrin_df |>
  mutate(unique_to_ecrin = 1)|>
  select(standardized_pair, unique_to_ecrin)

mdr_crossreg_pipeline_overlap <- mdr_crossreg_pipeline_overlap |>
  mutate(overlap = 1)

# Highlight TRN pairs unique to the crossreg pipeline approach in broader table, `potential_crossregs`
# QUESTION: What kind of trial pairs did our approach find, that could not be identified from MDR data?
potential_crossregs_unique_highlighted <- potential_crossregs_standardized |>
  left_join(unique_to_crossreg_pipeline_df, by = "standardized_pair") |>
  filter(unique_to_trn == 1)


# Highlight TRN pairs "unique" to the MDR data in broader table, `potential_crossregs`
# QUESTION: Are there any TRN pairs "unique" to the MDR data that we also identified, but excluded from `potential_crossregs_filtered`? 
# Check for overlap between `unique_to_ecrin_df` and `potential_crossregs_standardized`
potential_crossregs_mdr_highlighted <- potential_crossregs_standardized |>
  left_join(unique_to_ecrin_df, by = "standardized_pair") |>
  filter(unique_to_ecrin == 1)

# QUESTION: What kind of trial pairs did both the MDR data and the crossreg pipeline approach capture?
# Create data frame to dive into this
mdr_crossreg_pipeline_overlap_with_information <- potential_crossregs_standardized |>
  left_join(mdr_crossreg_pipeline_overlap, by = "standardized_pair") |>
  filter(overlap == 1)

# QUESTION: What kind of TRN pairs did the crossreg pipeline approach miss entirely, which can be identified in the MDR?
# `potential_crossregs_mdr_highlighted` contains only TRN pairs sorted into Priority 5
priority_5_isolated <- potential_crossregs_mdr_highlighted |>
  select(standardized_pair)

# Determine which pairs in `unique_to_ecrin_df` we missed entirely in our crossreg pipeline approach (not in priority 5)
difference <- setdiff(unique_to_ecrin_df$standardized_pair, priority_5_isolated$standardized_pair)

# Convert `difference` to data frame in order to inspect visually, left_join to larger MDR source table for more information on pairs
missed_in_crossreg_pipeline <- as.data.frame(difference) |>
  rename(standardized_pair = difference) |>
  left_join(ecrin_standardized, by = "standardized_pair")

# QUESTION: Of the pairs found exclusively from our crossreg pipeline, how many were screened and confirmed as true cross registrations? 

manual_screening <- read_csv(here("data", "manual_validation_processed.csv"))

manual_screening_standardized <- manual_screening |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)


# Left join manually validated pairs to unique_to_crossreg_pipeline_df
manual_screening_with_crossreg_pipeline_unique_flagged <- unique_to_crossreg_pipeline_df |>
  left_join(manual_screening_standardized, by = "standardized_pair") |>
  filter(!is.na(is_true_crossreg))

# Summary of true/ false positive proportions of trials exclusively found by crossreg pipeline and manually screened 
crossreg_pipeline_unique_screened_summary <- manual_screening_with_crossreg_pipeline_unique_flagged |>
  group_by(is_true_crossreg) |>
  summarise(
    total = n(),
    proportion = n()/ nrow(manual_screening_with_crossreg_pipeline_unique_flagged)
  )


