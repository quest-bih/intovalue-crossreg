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
potential_crossregs <- read_rds(here("data","crossreg_pipeline_output.rds"))

# Here, we are filtering for trials with a priority of 4 or lower (priority 4 = trial identifier mentioned in another trial's related publication), as our pilot manual review of trial pairs with priority > 4 revealed low precision for correctly identified true cross-registrations
# Unlike in other scripts, we do not filter out pairs that don't resolve in EUCTR or DRKS.
# We took this decision because ECRIN does not make any claims about the validity of a cross registration 
# Like our approach, it merely compiles potential cross-registrations. Since the point of this analysis is not to screen and confirm cross-registrations, but 
# to identify how these two searching approaches overlap, it is appropriate to leave TRN pairs that may not resolve in a registry
trn_filtered <- potential_crossregs |>
  filter(priority <= 4 )

# Including this table allows us to identify further overlap between ECRIN results, and trial pairs 
# identified by our approach, but excluded from trn_filtered for their Priorities
potential_crossregs_standardized <- standardize_pairs(potential_crossregs)



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
  

# Apply the function to both datasets (do NOT filter for trials that resolve in TRN)
ecrin_standardized <- standardize_pairs(ecrin)
crossreg_pipeline_pairs_standardized <- standardize_pairs(trn_filtered)

# Convert to vectors for easy comparison
ecrin_pairs_vector <- ecrin_standardized$standardized_pair
crossreg_pipeline_pairs_vector <- crossreg_pipeline_pairs_standardized$standardized_pair

#######################################################################################
# New, cleaner approach to Venn diagrams to visualize overlap between our approach and the ECRIN approach

ecrin_venn_data <- list(
  "Approach of the present study" = crossreg_pipeline_pairs_vector,
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
  annotate("label", x = -1.5, y = 0.3, label = "Approach of the present study",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.2, y = 0.3, label = "ECRIN Approach",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines"))

#######################################################################################
# Here we identify the trial pairs unique to ECRIN's approach and our approach, and where our approaches overlapped

 common_pairs <- intersect(ecrin_pairs_vector, crossreg_pipeline_pairs_vector)
 unique_to_ecrin <- setdiff(ecrin_pairs_vector, crossreg_pipeline_pairs_vector)
 unique_to_trn <- setdiff(crossreg_pipeline_pairs_vector, ecrin_pairs_vector)


# Summary counts
total_ecrin <- length(ecrin_pairs_vector)
total_trn <- length(crossreg_pipeline_pairs_vector)
common_count <- length(common_pairs)
unique_to_ecrin_count <- length(unique_to_ecrin)
unique_to_trn_count <- length(unique_to_trn)

###################################################################################################

