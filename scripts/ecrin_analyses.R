# A script to evaluate the degree of overlap between potential cross-registrations identified via our approach and 
# inferred cross-registrations based on data in the 
# Clinical Research Metadata Repository (MDR) from ECRIN (https://ecrin.org/clinical-research-metadata-repository)
library(tidyverse)
library(here)
library(ggvenn)
library(ggupset)
library(ctregistries)

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
  fill_color = c("steelblue", "#F7C72F"),
  stroke_size = 0.5,
  text_size = 4,
  set_name_size = 0, # Turn off default labels for custom handling
  auto_scale = TRUE
)

p <- base_ecrin_venn +
  annotate("label", x = -0.84, y = 0.4, label = "Approach of study",
           fill = "white", color = "black", size = 4, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 0.7, y = 0.4, label = "MDR Data",
           fill = "white", color = "black", size = 4, label.padding = unit(0.2, "lines")) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

#######################################################################################
# Here we identify the trial pairs unique to the MDR data and our approach, and where our approaches overlapped

common_pairs <- intersect(ecrin_pairs_vector, potential_crossregs_filtered_standardized_vector)
unique_to_ecrin <- setdiff(ecrin_pairs_vector, potential_crossregs_filtered_standardized_vector)
unique_to_crossreg_pipeline <- setdiff(potential_crossregs_filtered_standardized_vector, ecrin_pairs_vector)


# Summary counts
total_ecrin <- length(ecrin_pairs_vector)
total_potential_crossregs_filtered <- length(potential_crossregs_filtered_standardized_vector)
common_count <- length(common_pairs)
unique_to_ecrin_count <- length(unique_to_ecrin)
unique_to_crossreg_pipeline_count <- length(unique_to_crossreg_pipeline)

###################################################################################################

# Data exploration section

# Including this table allows us to identify further overlap between MDR results, and trial pairs 
# identified by our crossreg pipeline approach. Some TRN pairs found "exclusively" by the MDR were also identified
# in our data, but excluded from `potential_crossregs_filtered` for having a Priority > 4

potential_crossregs_standardized <- potential_crossregs |>
  filter(priority >= 1 & priority <= 5) |>
  standardize_pairs()

#######################################################################################
# Highlight TRN pairs "unique" to the MDR data 
# QUESTION: What TRN pairs did the MDR find that are not present in the filtered results of the crossreg pipeline approach? 
# Check for overlap between `unique_to_ecrin_df` and `potential_crossregs_filtered_standardized`

unique_to_ecrin_df <- ecrin_standardized |> 
  filter(!standardized_pair %in% potential_crossregs_filtered_standardized$standardized_pair)

#######################################################################################
# Highlight TRN pairs unique to the crossreg pipeline approach in filtered table, `potential_crossregs_filtered_standardized`
# QUESTION: What kind of trial pairs did our approach find, that could not be identified from MDR data?

unique_to_crossreg_pipeline_df <- potential_crossregs_filtered_standardized |>
  filter(!standardized_pair %in% ecrin_standardized$standardized_pair)

#######################################################################################
# Dive deeper into the TRN pairs unique to the crossreg pipeline approach and create an upset plot to visualize the connections
# QUESTION: Of the TRN pairs only we found, how are they connected?

# Add registry information back in, in case we want to divide analysis by registry
unique_to_crossreg_pipeline_upset <- unique_to_crossreg_pipeline_df |>
  rowwise()|>
  mutate(registry1 = ctregistries::which_registry(trn1),
         registry2 = ctregistries::which_registry(trn2))

# Edit layout to make table easier for upset package to read
unique_to_crossreg_pipeline_upset <- unique_to_crossreg_pipeline_upset |>
  mutate(
    bidirectional = if_else(trn1inreg2 & trn2inreg1, TRUE, FALSE),
    is_title_matched = if_else(is.na(is_title_matched), FALSE, is_title_matched),
    non_euctr_registry = if_else(registry1 == "EudraCT", registry2, registry1) ,
    unidirectional = if_else((trn1inreg2 | trn2inreg1) & !bidirectional, TRUE, FALSE),
    # unidirectional = if_else((trn1inreg2 | trn2inreg1), TRUE, FALSE)
    
    trn1_in_pub_abs = replace_na(trn1_in_pub_abs, FALSE),
    trn2_in_pub_abs = replace_na(trn2_in_pub_abs, FALSE),
    trn_in_pub_abs = trn1_in_pub_abs | trn2_in_pub_abs,
    
    trn1_in_pub_si = replace_na(trn1_in_pub_si, FALSE),
    trn2_in_pub_si = replace_na(trn2_in_pub_si, FALSE),
    trn_in_pub_si = trn1_in_pub_si | trn2_in_pub_si,
    
    trn1_in_pub_ft = replace_na(trn1_in_pub_ft, FALSE),
    trn2_in_pub_ft = replace_na(trn2_in_pub_ft, FALSE),
    trn_in_pub_ft = trn1_in_pub_ft | trn2_in_pub_ft
  ) 

# Keep editing table layout to make easier for upset plot package to read
unique_to_crossreg_pipeline_upset <-
  unique_to_crossreg_pipeline_upset |>
  select(trn1,
         trn2,
         non_euctr_registry,
         is_title_matched,
         at_least_one_pub,
         bidirectional,
         unidirectional, 
         standardized_pair
  ) |>
  rename(
    "Title matched" = is_title_matched,
    "Publication link" = at_least_one_pub,
    "Bidirectional link" = bidirectional,
    "Unidirectional link" = unidirectional
  ) |>
  pivot_longer(cols = -c(trn1, trn2, non_euctr_registry, standardized_pair), names_to = "link") |>
  filter(value == TRUE) |>
  group_by(trn1, trn2) |>
  mutate(links = list(link)) |>
  ungroup() |>
  select(-value, -link) |>
  distinct()


# Upset plot showing connections between TRN pairs found exclusively by the crossreg pipeline approach
unique_crossreg_pipeline_upset_plot <- unique_to_crossreg_pipeline_upset |>
  ggplot(aes(x=links)) +
  geom_bar() +
  geom_text(stat='count', 
            size = 6,
            aes(label=after_stat(count)), 
            vjust=-1) +
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(limits = c(0, 100)) +
  ylab("Number of pairs") +
  xlab("Linking combinations") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16)
  )
#######################################################################################
# QUESTION: What kind of trial pairs did both the MDR data and the crossreg pipeline approach capture?
# Use of broader table is intentional in order to capture full degree of overlap between MDR and the crossreg pipeline. 
# If interested in the overlap between potential_crossregs_filtered and MDR, please filter the resulting table for priority <= 4
# Create data frame to dive into this

# Here, we compare the MDR data with the full, unfiltered set of potential cross registrations found through the crossreg pipeline
# This is because many of the TRN pairs found by MDR were also found in the crossreg pipeline, but filtered out of potential_crossregs_filtered
# Using the full, unfiltered data set gives a clearer picture of the overlap between MDR and crossreg pipeline
# Add overlap flag so that overlapping TRN pairs can be identified in `potential_crossregs`

mdr_crossreg_pipeline_overlap <- semi_join(potential_crossregs_standardized,
                                            ecrin_standardized,
                                            by = "standardized_pair") |> 
  transmute(standardized_pair, overlap = 1) 

mdr_crossreg_pipeline_overlap_with_information <- potential_crossregs_standardized |>
  left_join(mdr_crossreg_pipeline_overlap, by = "standardized_pair") |>
  filter(overlap == 1)

#######################################################################################
# QUESTION: What kind of TRN pairs did the crossreg pipeline approach miss entirely, which can be identified in the MDR?

missed_in_crossreg_pipeline <- unique_to_ecrin_df |> 
  anti_join(mdr_crossreg_pipeline_overlap_with_information, by = "standardized_pair") |> 
  select(standardized_pair, everything())

#######################################################################################
# QUESTION: Of the pairs found exclusively from our crossreg pipeline, how many were screened and confirmed as true cross registrations? 

manual_screening <- read_csv(here("data", "manual_validation_processed.csv"))

manual_screening_standardized <- manual_screening |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

# Left join manually validated pairs to unique_to_crossreg_pipeline_df
crossreg_pipeline_unique_flagged <- unique_to_crossreg_pipeline_df |>
  left_join(manual_screening_standardized, by = "standardized_pair") |>
  filter(!is.na(is_true_crossreg))

# Summary of true/ false positive proportions of trials exclusively found by crossreg pipeline and manually screened 
pipeline_screened_summary <- crossreg_pipeline_unique_flagged |>
  group_by(is_true_crossreg) |>
  summarise(
    total = n(),
    proportion = n()/ nrow(crossreg_pipeline_unique_flagged)
  )


