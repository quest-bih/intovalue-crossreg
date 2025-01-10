# A script to explore cross registrations that were screened as true/false cross-registrations
# previously identified potential cross registration pairs were classified as either true cross registrations or false positive pairs

library(tidyverse)
library(here)
library(ctregistries)
library(ggupset)

################################################################################################
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
################################################################################################

# Load data
# `manual_validation_processed.csv` contains all screened cross-registrations, and has a variable `is_true_crossreg` that labels those that were confirmed as true cross-registrations
manual_screening <- read.csv(here("data", "manual_validation_processed.csv"))
potential_crossreg<- read_rds(here("data", "crossreg_pipeline_output.rds"))

# Standardize list of manually screened pairs
manual_screening_standardized <- manual_screening |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

# Prepare/process tables for use
# Here, we are filtering for trials with a priority of 4 or lower (priority 4 = trial identifier mentioned in another trial's related publication), as our pilot manual review of trial pairs with priority > 4 revealed low precision for correctly identified true cross-registrations
# Then, we filter for TRNs that have not been removed from the DRKS registry. We also filter for EUCTR TRNs that still resolve in the EUCTR registry. These steps ensure that all remaining TRN pairs can be looked up in their respective registries and screened
# Finally, we filter out one specific row, in which the TRN '2008-004408-29' is incorrectly marked as a trial that resolves in the EUCTR database.
trn_filtered <- potential_crossreg|>
  filter(priority <= 4) |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")

# Add registries back in
trn_filtered <- trn_filtered |>
  rowwise()|>
  mutate(registry1 = ctregistries::which_registry(trn1),
         registry2 = ctregistries::which_registry(trn2))

trn_filtered <- standardize_pairs(trn_filtered)

# Add columns to simplify identifying bidirectional/unidirectional linking, make upset plot easier to build
# Add column to identify non-EUCTR registry
# This version of the table separates bidirectional and unidirectional trial pairs
# ie. no pair marked as bidirectional will also be marked as unidirectional

# in order to obtain a table with all bidirectional pairs also marked as unidirectional:
# uncomment the line with `unidirectional = if_else((trn1inreg2 | trn2inreg1), TRUE, FALSE)`
# comment out the line with `unidirectional = if_else((trn1inreg2 | trn2inreg1) & !bidirectional, TRUE, FALSE)`

trn_filtered <- trn_filtered |>
  mutate(
    bidirectional = if_else(trn1inreg2 & trn2inreg1, TRUE, FALSE),
    is_title_matched = if_else(is.na(is_title_matched), FALSE, is_title_matched),
    non_euctr_registry = if_else(registry1 == "EudraCT", registry2, registry1) ,
    unidirectional = if_else((trn1inreg2 | trn2inreg1) & !bidirectional, TRUE, FALSE)
    # unidirectional = if_else((trn1inreg2 | trn2inreg1), TRUE, FALSE)
  ) 

############################################################################
# Upset plot for screened TRN pairs
# Will show true positives/false positives in each category


# Make trn_filtered readable for ggupset package, 
# Retain standardized_pair so we can match it to manual_validation_upset
trn_combos_potential_crossreg <-
  trn_filtered |>
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

# Join information on precision from the manual check with upset-friendly format of `trn_combos_potential_crossreg` to make upset w/ false positivity information
# NOTE: The editing of 2010-023688-16_NCT01326767 will no longer be necessary after receiving updated manual_validation_processed.csv file without white spaces
manual_screening_upset <- manual_screening_standardized |>
  left_join(trn_combos_potential_crossreg, by = "standardized_pair") |>
  mutate(links = ifelse(standardized_pair == " 2010-023688-16_NCT01326767", "Bidirectional link", links)) # Manually change `links` back to `bidirectional` for row "2010-023688-16_NCT01326767", not sure why it changes at all

# Upset plot showing screened TRN pairs, with false positivity displayed
manual_screening_plot <- manual_screening_upset |> 
  ggplot(aes(x = links, fill = is_true_crossreg)) + 
  geom_bar(position = "stack") + 
  geom_text(
    stat = "count", 
    aes(label = after_stat(count), group = is_true_crossreg), 
    position = position_stack(vjust = 0.5) # Place labels in the middle of each section
  ) + 
  scale_x_upset(n_intersections = 20) + 
  scale_fill_manual(values = c("TRUE" = "#B3EBF2", "FALSE" = "#009E73"),
                    name = "Confirmed as a true cross-registration") +
  ylab("Number of pairs") + 
  xlab("Linking combinations") + 
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"), 
    legend.position = c(.85, .9), 
    axis.title.y = element_text(size = 11)
  )


############################################################################
# Upset plot for screened trials linked by publication, with false positive count

# Add information about true/false positivity on cross registration status to `trn_filtered`
trn_filtered_with_screening_info<- manual_screening_standardized |>
  left_join(trn_filtered, by = "standardized_pair") |>
  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg)) # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all

# Filter for trials linked by publication in any way
pub_crossregs_manual_screened <- trn_filtered_with_screening_info |>
  filter(at_least_one_pub) |>
  mutate(
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


#Change format to make friendlier for ggupset
upset_manual_screening <-
  pub_crossregs_manual_screened |>
  select(trn1,
         trn2,
         trn_in_pub_si,
         trn_in_pub_abs,
         trn_in_pub_ft,
         standardized_pair
  ) |>
  rename(
    "trn in SI" = trn_in_pub_si,
    "trn in abstract" = trn_in_pub_abs,
    "trn in full text" = trn_in_pub_ft,
  ) |>
  pivot_longer(cols = -c(trn1, trn2, standardized_pair), names_to = "link") |>
  filter(value == TRUE) |>
  group_by(trn1, trn2) |>
  mutate(links = list(link)) |>
  ungroup() |>
  select(-value, -link) |>
  distinct()

upset_manual_false_positive <- upset_manual_screening |>
  left_join(manual_screening_standardized, by = "standardized_pair")

# Plot false positives in pub linkages

pub_linking_combinations_false_positive <- upset_manual_false_positive |> 
  ggplot(aes(x = links, fill = is_true_crossreg)) + 
  geom_bar(position = "stack") + 
  geom_text(
    stat = "count", 
    aes(label = after_stat(count), group = is_true_crossreg), 
    position = position_stack(vjust = 0.5) # Place labels in the middle of each section
  ) + 
  scale_x_upset(n_intersections = 20) + 
  ylab("Number of pairs") + 
  xlab("Linking combinations") + 
  scale_fill_manual(values = c("TRUE" = "#B3EBF2", "FALSE" = "#009E73"),
                    name = "Confirmed as a true cross-registration") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"), 
    legend.position = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

################################################################################

# Investigate how false positive (screened, but not confirmed) cross-registrations are connected ( 8/9 false positives are Category 4, so this will be investigated here)

# Filter for false cross-registrations
false_crossreg_standardized <- manual_screening_standardized |>
  filter(!is_true_crossreg) |>
  select(standardized_pair, is_true_crossreg)

false_crossreg <- false_crossreg_standardized |>
  left_join(trn_filtered, by = "standardized_pair")

# Visual inspection of table shows that all 8 are only connected by trn2_in_pub_ft
# In this case, 8 EUCTR numbers were mentioned in the full text of publications associated with 8 IntoValue TRNs

################################################################################
# Upset plot for all trials that were screened and CONFIRMED, across all links (not just publications)
# This code will likely not be used for any table in the paper and is thus deprecated

# Full table containing only screened and confirmed crossregs
#validated_crossreg <- trn_filtered |>
#  left_join(manual_validated_standardized, by = "standardized_pair") |> 
#  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg))|> # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all
#  filter(is_true_crossreg)

# Make validated_crossreg readable for ggupset package 
#upset_validated_crossreg <- validated_crossreg |>
#  select(trn1,
#         trn2,
#         non_euctr_registry,
#         is_title_matched,
#         at_least_one_pub,
#         bidirectional,
#         unidirectional
#  ) |>
#  rename(
#    "Title matched" = is_title_matched,
#    "Publication link" = at_least_one_pub,
#    "Bidirectional link" = bidirectional,
#    "Unidirectional link" = unidirectional
#  ) |>
#  pivot_longer(cols = -c(trn1, trn2, non_euctr_registry), names_to = "link") |>
#  filter(value == TRUE) |>
#  group_by(trn1, trn2) |>
#  mutate(links = list(link)) |>
#  ungroup() |>
#  select(-value, -link) |>
#  distinct()

# Upset plot showing all screened and CONFIRMED TRN pairs as proportions
#validated_crossreg_combinations_proportions <- upset_validated_crossreg |>
#  ggplot(aes(x = links)) +
#  geom_bar(aes(y = after_stat(count / nrow(upset_validated_crossreg) * 100))) +  # Set y as proportion for correct scaling
#  ggtitle("Confirmed Crossreg Combinations Proportions") +
#  geom_text(stat = 'count', aes(y = after_stat(count / nrow(upset_validated_crossreg) * 100), label = sprintf("%.1f%%", after_stat(count / nrow(upset_validated_crossreg) * 100))), vjust = -1) + # Display as percentages
# scale_x_upset(n_intersections = 20) +
#  scale_y_continuous(limits = c(0, 30), expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis limits and add small padding
#  ylab("Proportion of pairs (%)") +  
#  xlab("Linking combinations") +
#  theme(
   # legend.background = element_rect(color = "transparent", fill = "transparent"),
  #  legend.position = c(.85, .9),
 #   axis.title.y = element_text(size = 11)
#  )


################################################################################
# Upset plot for trials linked by publication
# The plot here is for trial pairs linked by publications that were screened and CONFIRMED as true cross-registrations
# The other publication upset also includes pairs that were not true cross-registrations
# This plot is deprecated and has been commented out here


#Change format to make friendlier for ggupset
#upset_pub_crossregs <-
#  pub_crossregs |>
#  select(trn1,
#         trn2,
#         trn_in_pub_si,
#         trn_in_pub_abs,
#         trn_in_pub_ft,
#  ) |>
#  rename(
#    "trn in SI" = trn_in_pub_si,
#    "trn in abstract" = trn_in_pub_abs,
#    "trn in full text" = trn_in_pub_ft,
#  ) |>
#  pivot_longer(cols = -c(trn1, trn2), names_to = "link") |>
#  filter(value == TRUE) |>
#  group_by(trn1, trn2) |>
#  mutate(links = list(link)) |>
#  ungroup() |>
#  select(-value, -link) |>
#  distinct()

# Plot upset
#pub_linking_combinations <- upset_pub_crossregs |>
#  ggplot(aes(x=links)) +
#  geom_bar() +
#  ggtitle("Overall Combinations") +
#  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
#  scale_x_upset(n_intersections = 20) +
#  ylab("Number of pairs") +
#  xlab("Linking combinations") +
#  theme(
#    legend.background = element_rect(color = "transparent", fill = "transparent"),
#    legend.position.inside = c(.85, .9),
#    axis.title.y = element_text(size = 11)
#  )


#################################################################################
