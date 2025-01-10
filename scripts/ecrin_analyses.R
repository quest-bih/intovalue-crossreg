# A script to evaluate the degree of overlap between ECRIN MDR cross registrations
# and the cross registrations we found

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
trn_manual_checks <- read_rds(here("data","crossreg_pipeline_output.rds"))

# Here, we are filtering for trials with a priority of 4 or lower (priority 4 = trial identifier mentioned in another trial's related publication), as our pilot manual review of trial pairs with priority > 4 revealed low precision for correctly identified true cross-registrations
# Unlike in other scripts, we do not filter out pairs that don't resolve in EUCTR or DRKS.
# We took this decision because ECRIN does not make any claims about the validity of a cross registration 
# Like our approach, it merely compiles potential cross-registrations. Since the point of this analysis is not to screen and confirm cross-registrations, but 
# to identify how these two searching approaches overlap, it is appropriate to leave TRN pairs that may not resolve in a registry
trn_filtered <- trn_manual_checks |>
  filter(priority <= 4 )

trn_manual_standardized <- standardize_pairs(trn_manual_checks)

# Filter TRN for EUCTR and DRKS that resolves
trn_resolves <- trn_filtered |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")

# read in ECRIN information
ecrin <- read_csv(here("data", "20241025_mdr_identifiers.csv")) |>
  rename(trn1 = iv_id,
         trn2 = identifier) |>
  filter(identifier_type == "Trial registry ID") |>
  filter( identifier_source == "EU Clinical Trials Register")

# num of distinct EUCTR trials
euctr_distinct <- ecrin |>
  select(trn2) |>
  distinct()

# num of distinct IV trials
iv_distinct <- ecrin |>
  select(trn1) |>
  distinct()
  

ecrin_errors <- read_csv("data/20241025_mdr_identifiers_errors.csv")



# Apply the function to both datasets (do NOT filter for trials that resolve in TRN)
ecrin_standardized <- standardize_pairs(ecrin)
trn_standardized <- standardize_pairs(trn_filtered)

# trn_resolves_standardized <- standardize_pairs(trn_resolves)

# Select only the relevant column containing the standardized pairs
ecrin_pairs <- ecrin_standardized |> select(standardized_pair)
trn_pairs <- trn_standardized |> select(standardized_pair)

# trn_resolves_pairs <- trn_resolves_standardized |> select(standardized_pair)

# Convert to vectors for easy comparison
ecrin_pairs_vector <- ecrin_pairs$standardized_pair
trn_pairs_vector <- trn_pairs$standardized_pair

#######################################################################################
# New, cleaner approach to Venn diagrams that incorporates Daniel's suggestions
# Replaces most of the code Venn diagram code past this section

ecrin_venn_data <- list(
  "Approach of the present study" = trn_pairs_vector,
  "ECRIN Approach" = ecrin_pairs_vector
  )

base_ecrin_venn <- ggvenn(
 ecrin_venn_data,
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5,
  set_name_size = 0, # Turn off default labels for custom handling
 auto_scale = TRUE
)

base_ecrin_venn +
  annotate("label", x = -1.5, y = 0, label = "Approach of the present study",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.2, y = 0, label = "ECRIN Approach",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines"))

#######################################################################################
# The below is no longer being used. It was the previous method for creating Ecrin Venn diagrams with 
# proportional area sizes
# Also includes code for making Ecrin Venn with only TRNs that resolve in the Ecrin database.
# It should be completely commmented out

# trn_resolves_pairs_vector <- trn_resolves_pairs$standardized_pair
# Create a Venn diagram for the pair overlaps
# overlap_plot <- venn.diagram(
#  x = list(
#    "ECRIN" = ecrin_pairs_vector,
#    "TRN Manual" = trn_pairs_vector
#  ),
#  category.names = c("ECRIN", "TRN Manual"),
#  filename = NULL,  # Don't save to file; display directly
#  output = TRUE,
#  fill = c("lightblue", "lightgreen"),
#  alpha = 0.5,
#  cex = 2,
#  fontfamily = "sans",
#  cat.fontfamily = "sans",
#  cat.cex = 2,
#  cat.pos = c(-20, 20)
# )

# Plot the Venn diagram
# grid.draw(overlap_plot)

# Create a Venn diagram for the pair overlaps for TRNs that resolve
# resolves_overlap_plot <- venn.diagram(
#  x = list(
#    "ECRIN" = ecrin_pairs_vector,
#    "TRN Manual" = trn_resolves_pairs_vector
#  ),
#  category.names = c("ECRIN", "TRN Manual"),
#  filename = NULL,  # Don't save to file; display directly
#  output = TRUE,
#  fill = c("lightblue", "lightgreen"),
#  alpha = 0.5,
#  cex = 2,
#  fontfamily = "sans",
#  cat.fontfamily = "sans",
# cat.cex = 2,
 # cat.pos = c(-20, 20)
#)

# Plot the Venn diagram
# grid.draw(resolves_overlap_plot)

#######################################################################################
# Here we identify the trial pairs unique to ECRIN's approach and our approach, and where our approaches overlapped

 common_pairs <- intersect(ecrin_pairs_vector, trn_pairs_vector)
 unique_to_ecrin <- setdiff(ecrin_pairs_vector, trn_pairs_vector)
 unique_to_trn <- setdiff(trn_pairs_vector, ecrin_pairs_vector)

# Pairs that resolve
# resolves_common_pairs <- intersect(ecrin_pairs_vector, trn_resolves_pairs_vector)
# resolves_unique_to_ecrin <- setdiff(ecrin_pairs_vector, trn_resolves_pairs_vector)
# resolves_unique_to_trn <- setdiff(trn_resolves_pairs_vector, ecrin_pairs_vector)


# Summary counts
total_ecrin <- length(ecrin_pairs_vector)
total_trn <- length(trn_pairs_vector)
common_count <- length(common_pairs)
unique_to_ecrin_count <- length(unique_to_ecrin)
unique_to_trn_count <- length(unique_to_trn)

# Summary counts for resolving pairs
# resolves_total_ecrin <- length(ecrin_pairs_vector)
# resolves_total_trn <- length(trn_resolves_pairs_vector)
# resolves_common_count <- length(resolves_common_pairs)
# resolves_unique_to_ecrin_count <- length(resolves_unique_to_ecrin)
# resolves_unique_to_trn_count <- length(resolves_unique_to_trn)

# tables of trial pairs unique to ecrin or TRN
unique_to_ecrin_df <- ecrin_pairs[!ecrin_pairs$standardized_pair %in% trn_pairs$standardized_pair, ]
unique_to_trn_df <- trn_pairs[!trn_pairs$standardized_pair %in% ecrin_pairs$standardized_pair, ]

# add unique flag
unique_to_trn_df <- unique_to_trn_df |>
  mutate(unique_to_trn = 1)

unique_to_ecrin_df <- unique_to_ecrin_df |>
  mutate(unique_to_ecrin = 1)

# identify unique to trn pairs in broader table
trn_unique_flagged <- trn_standardized |>
  left_join(unique_to_trn_df, by = "standardized_pair") |>
  filter(unique_to_trn == 1)

# identify unique to ECRIN pairs in broader table
trn_manual_standardized <- trn_manual_standardized |>
  left_join(unique_to_ecrin_df, by = "standardized_pair") |>
  filter(unique_to_ecrin == 1)

# Isolate TRN pairs we found in category 5 to see which ones we missed entirely 
priority_5_isolated <- trn_manual_standardized |>
  select(standardized_pair)

# determine which pairs in unique to ecrin we missed entirely (not in priority 5)
difference <- setdiff(unique_to_ecrin_df$standardized_pair, priority_5_isolated$standardized_pair)

###################################################################################################
# Check how many of the pairs found exclusively by us were checked by us, and how many of those were actually positive

manual_validation <- read.csv("data/manual_validation_processed.csv")
unique_to_trn <- setdiff(trn_pairs_vector, ecrin_pairs_vector)
unique_to_trn_df <- trn_pairs[!trn_pairs$standardized_pair %in% ecrin_pairs$standardized_pair, ]

# Add in unique standardized_pair so we can compare with unique_to_trn_df
manual_validation_standard = standardize_pairs(manual_validation)

# Left join manually validated pairs to unique_to_trn_df
trn_unique_with_manual <- unique_to_trn_df |>
  left_join(manual_validation_standard, by = "standardized_pair") |>
  filter(!is.na(is_true_crossreg))

trn_unique_with_manual_summary <- trn_unique_with_manual |>
  group_by(is_true_crossreg) |>
  summarise(
    total = n(),
    proportion = n()/ nrow(trn_unique_with_manual)
  )




# Filter by first two priorities
# trn_unique_priority_1_2 <- trn_unique_flagged |>
#  filter(priority <= 2)


# Find the rows in `trn_pairs` that are unique to trn (not present in ecrin)
# resolves_unique_to_trn_df <- trn_resolves_pairs[!trn_resolves_pairs$standardized_pair %in% ecrin_pairs$standardized_pair, ]


# Adding back registry columns to filtered table
# trn_filtered <- trn_filtered |>
#  rowwise()|>
#  mutate(registry1 = which_registry(trn1),
#         registry2 = which_registry(trn2))

# trn_resolves <- trn_resolves |>
#  rowwise()|>
#  mutate(registry1 = which_registry(trn1),
#         registry2 = which_registry(trn2))

# trn_resolves <- trn_resolves |>
#  filter(trn2 != "2008-004408-29")

