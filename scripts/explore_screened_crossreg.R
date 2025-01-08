# A script to explore cross registrations that were further screened as true or false cross-registrations
# previously identified potential cross registration pairs were classified as either true cross registrations or false positive pairs

library(tidyverse)
library(VennDiagram)
library(lubridate)
library(here)
library(fs)
library(stringr)
library(ctregistries)
library(cli)
library(ggupset)
library(ggplot2)

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
manual_validation <- read.csv("data/manual_validation_processed.csv")
trn_manual_checks <- read_rds("data/crossreg_pipeline_output.rds")

# Prepare/process tables for use

trn_filtered <- trn_manual_checks |>
  filter(priority <= 4) |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")

# Add registries back in
trn_filtered <- trn_filtered |>
  rowwise()|>
  mutate(registry1 = which_registry(trn1),
         registry2 = which_registry(trn2))

trn_filtered <- standardize_pairs(trn_filtered)
trn_filtered <- trn_filtered |>
  mutate(
    bidirectional = if_else(trn1inreg2 & trn2inreg1, TRUE, FALSE),
    is_title_matched = if_else(is.na(is_title_matched), FALSE, is_title_matched),
    non_euctr_registry = ifelse(registry1 == "EudraCT", registry2, registry1) #,
    # unidirectional = if_else((trn1inreg2 | trn2inreg1), TRUE, FALSE)
  ) |>
  mutate(
    unidirectional = if_else((trn1inreg2 | trn2inreg1) & !bidirectional, TRUE, FALSE),
  )


# Filter for true cross-registrations
#confirmed_crossreg_standardized <- manual_validation |>
#  filter(is_true_crossreg) |>
#  standardize_pairs() |>
#  select(standardized_pair, is_true_crossreg)

trn_manual_checks_standardized <- standardize_pairs(trn_manual_checks)

# Join true crossreg with larger table by standardized_pair
#confirmed_crossreg <- confirmed_crossreg_standardized |>
#  left_join(trn_manual_checks_standardized, by = "standardized_pair") |>
#  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg))|> # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all
#  filter(is_true_crossreg)

# Filter for trials linked by publication in any way
#pub_crossregs <- confirmed_crossreg |>
#  filter(at_least_one_pub) |>
#  mutate(
#    trn1_in_pub_abs = replace_na(trn1_in_pub_abs, FALSE),
#    trn2_in_pub_abs = replace_na(trn2_in_pub_abs, FALSE),
#    trn_in_pub_abs = trn1_in_pub_abs | trn2_in_pub_abs,
    
#    trn1_in_pub_si = replace_na(trn1_in_pub_si, FALSE),
#    trn2_in_pub_si = replace_na(trn2_in_pub_si, FALSE),
#    trn_in_pub_si = trn1_in_pub_si | trn2_in_pub_si,
    
#    trn1_in_pub_ft = replace_na(trn1_in_pub_ft, FALSE),
#    trn2_in_pub_ft = replace_na(trn2_in_pub_ft, FALSE),
#    trn_in_pub_ft = trn1_in_pub_ft | trn2_in_pub_ft
#  )

# Filter for trials linked by FT mention
#pub_ft <- pub_crossregs |>
#  filter(trn1_in_pub_ft | trn2_in_pub_ft)

# Filter for trials linked by SI mention
#pub_si <- pub_crossregs |>
#  filter(trn1_in_pub_si | trn2_in_pub_si)

# Filter for trials linked by abstract mention
#pub_abs <- pub_crossregs |>
#  filter(trn1_in_pub_abs | trn2_in_pub_abs)
################################################################################
# Investigate how false positive cross-registrations are connected ( 8/9 false positives are Category 4, so this will be investigated here)

# Filter for false cross-registrations
false_crossreg_standardized <- manual_validation |>
  filter(!is_true_crossreg) |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

# Join true crossreg with larger table by standardized_pair
false_crossreg <- false_crossreg_standardized |>
  left_join(trn_manual_checks_standardized, by = "standardized_pair") |>
  filter(!is_true_crossreg & priority == 4)

# Visual inspection of table shows that all 8 are only connected by trn2_in_pub_ft
# In this case, 8 EUCTR numbers were mentioned in the full text of publications associated with 8 IntoValue TRNs

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

################################################################################
# Upset plot for all trials that were screened and CONFIRMED, across all links (not just publications)

# Full table containing only screened and confirmed crossregs
validated_crossreg <- trn_filtered |>
  left_join(manual_validation |> standardize_pairs() |> select(standardized_pair, is_true_crossreg), by = "standardized_pair") |> 
  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg))|> # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all
  filter(is_true_crossreg)

# Make validated_crossreg readable for ggupset package 
upset_validated_crossreg <- validated_crossreg |>
  select(trn1,
         trn2,
         non_euctr_registry,
         is_title_matched,
         at_least_one_pub,
         bidirectional,
         unidirectional
  ) |>
  rename(
    "Title matched" = is_title_matched,
    "Publication link" = at_least_one_pub,
    "Bidirectional link" = bidirectional,
    "Undirectional link" = unidirectional
  ) |>
  pivot_longer(cols = -c(trn1, trn2, non_euctr_registry), names_to = "link") |>
  filter(value == TRUE) |>
  group_by(trn1, trn2) |>
  mutate(links = list(link)) |>
  ungroup() |>
  select(-value, -link) |>
  distinct()

# Upset plot showing all screened and CONFIRMED TRN pairs as proportions
validated_crossreg_combinations_proportions <- upset_validated_crossreg |>
  ggplot(aes(x = links)) +
  geom_bar(aes(y = after_stat(count / 233 * 100))) +  # Set y as proportion for correct scaling
  ggtitle("Validated and True Crossreg Combinations Proportions") +
  geom_text(stat = 'count', aes(y = after_stat(count / 233 * 100), label = sprintf("%.1f%%", after_stat(count / 233 * 100))), vjust = -1) + # Display as percentages
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(limits = c(0, 30), expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis limits and add small padding
  ylab("Proportion of pairs (%)") +  
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )


################################################################################
# Upset plot for trials linked by publication, with false positive count

manual_validated_standardized <- manual_validation |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

# Filter for true cross-registrations
confirmed_crossreg_standardized <- manual_validation |>
  filter(is_true_crossreg) |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

manual_validated <- manual_validated_standardized |>
  left_join(trn_manual_checks_standardized, by = "standardized_pair") |>
  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg)) # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all

# Filter for trials linked by publication in any way
pub_crossregs_manual_validated <- manual_validated |>
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
upset_manual_validation <-
  pub_crossregs_manual_validated |>
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

upset_manual_false_positive <- upset_manual_validation |>
  left_join(manual_validated_standardized, by = "standardized_pair")

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

############################################################################
# Upset plot for manually validated TRN pairs (with all categories, not just publication linkages)
# Will show false positivity rate per category

manual_validation_upset <- manual_validation |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

# Make trn_filtered readable for ggupset package, 
# Retain standardized_pair so we can match it to manual_validation_upset
trn_combos_validated <-
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
    "Undirectional link" = unidirectional
  ) |>
  pivot_longer(cols = -c(trn1, trn2, non_euctr_registry, standardized_pair), names_to = "link") |>
  filter(value == TRUE) |>
  group_by(trn1, trn2) |>
  mutate(links = list(link)) |>
  ungroup() |>
  select(-value, -link) |>
  distinct()

# Join false positivity information to create new upset
manual_validation_upset <- manual_validation_upset |>
  left_join(trn_combos_validated, by = "standardized_pair") |>
  mutate(links = ifelse(standardized_pair == " 2010-023688-16_NCT01326767", "Bidirectional link", links)) # Manually change `links` back to `bidirectional` for row "2010-023688-16_NCT01326767", not sure why it changes at all

# Upset plot showing manually validated TRN pairs, with false positivity displayed

manual_validation_plot <- manual_validation_upset |> 
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
