# Script to make upset plots of different ways we identified crossregs
library(tidyverse)
library(here)
library(lubridate)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(ctregistries)
library(cli)
library(ggupset)
library(ggplot2)
library(VennDiagram)
library(ggvenn)


# Read in manual checks table, filter out everything w/ priority more than 4 and non-resolving or removed TRNs
trn_manual_checks = read_rds("data/crossreg_pipeline_output.rds")

trn_filtered <- trn_manual_checks |>
  filter(priority <= 4) |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")

# Add registries back in
trn_filtered <- trn_filtered |>
  rowwise()|>
  mutate(registry1 = which_registry(trn1),
         registry2 = which_registry(trn2))

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

# Read in confirmed crossregs, standardize pairs so we can merge with larger table
manual_validation <- read.csv("data/manual_validation_processed.csv") |>
  filter(is_true_crossreg) |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

trn_filtered <- standardize_pairs(trn_filtered)

# Add columns to simplify identifying bidirectional/unidirectional linking, make upset plot easier to build
# Add column to identify non-EUCTR registry
trn_filtered <- trn_filtered |>
  mutate(
    bidirectional = if_else(trn1inreg2 & trn2inreg1, TRUE, FALSE),
    non_euctr_registry = ifelse(registry1 == "EudraCT", registry2, registry1)
  ) |>
  mutate(
    unidirectional = if_else((trn1inreg2 | trn2inreg1) & !bidirectional, TRUE, FALSE),
  )

# Full table containing only true, validated crossregs for another upset plot
validated_crossreg <- trn_filtered |>
  left_join(manual_validation, by = "standardized_pair") |> 
  mutate(is_true_crossreg = ifelse(standardized_pair == "2010-023688-16_NCT01326767", TRUE, is_true_crossreg))|> # Manually change is_true_crossreg back to TRUE for row "2010-023688-16_NCT01326767", not sure why it changes at all
  filter(is_true_crossreg)

#########################################################################################################################
# Count and analyze how many cross-registrations are in all 3 registrations (informal) analysis

# Assign euctr_trn and non_euctr_trn based on the registry
trn_filtered <- trn_filtered |>
  mutate(
    euctr_trn = ifelse(registry1 == "EudraCT", trn1, ifelse(registry2 == "EudraCT", trn2, NA)),
    non_euctr_trn = ifelse(registry1 != "EudraCT", trn1, trn2),
    non_euctr_registry = ifelse(registry1 != "EudraCT", registry1, registry2)
  )

# Find EUCTR trials linked to multiple non-EUCTR TRNs
duplicates <- trn_filtered |>
  group_by(euctr_trn) |>
  filter(n_distinct(non_euctr_trn) > 1) |>  # Keep only EUCTR IDs linked to more than one non-EUCTR ID
  ungroup()

# Returns 5 pairs
# DRKS00000582 - NCT00989352 (linked by 2008-007645-31) - does appear like a valid crossreg
# NCT02035709 - NCT01490268 (found by us in Priority 7, linked by 2011-003648-31, also NCT02035709 is also linked to 2013-002875-16 in Priority 2) - UNSURE if valid crossreg
# NCT01703832 - NCT01703819 ( NCT01703832 also connected to 2012-002359-40 in Priority 1, these two connected by 2012-002358-22 ) - These look like identical trials
# NCT02309918 - DRKS00006863 (not seen previously, connected by 2013-001081-42) - Looks VALID
# DRKS00003170 - NCT01387048 (not seen previously, connected by 2011-000152-42) - looks VALID

# also potential linkages within EUCTR?
# 2011-003648-31 - 2013-002875-16 (2013 link no longer resolves)
# 2012-002359-40 - 2012-002358-22 (first one is open label, second is double-blind, otherwise the same)

# More in depth description in Paper Outline document

############################################################################
# Code for Table 1
# Table 1. Characteristics indicating potential cross-registrations, 
# overall and by registry (prior to manual validation).  

# Overall
trn_overall_bidirectional <- trn_filtered |>
  filter(bidirectional) |>
  nrow()

trn_overall_unidirectional <- trn_filtered |>
  filter(unidirectional) |>
  nrow()

trn_overall_title_match <- trn_filtered |>
  filter(is_title_matched) |>
  nrow()

trn_overall_pub <- trn_filtered |>
  filter(at_least_one_pub) |>
  nrow()

# ClinicalTrials.gov
trn_ct_bidirectional <- trn_filtered |>
  filter(non_euctr_registry == "ClinicalTrials.gov") |>
  filter(bidirectional) |>
  nrow()

trn_ct_unidirectional <- trn_filtered |>
  filter(non_euctr_registry == "ClinicalTrials.gov") |>
  filter(unidirectional) |>
  nrow()

trn_ct_title_matched <- trn_filtered |>
  filter(non_euctr_registry == "ClinicalTrials.gov") |>
  filter(is_title_matched) |>
  nrow()

trn_ct_pub <- trn_filtered |>
  filter(non_euctr_registry == "ClinicalTrials.gov") |>
  filter(at_least_one_pub) |>
  nrow()

# DRKS
trn_drks_bidirectional <- trn_filtered |>
  filter(non_euctr_registry == "DRKS") |>
  filter(bidirectional) |>
  nrow()

trn_drks_unidirectional <- trn_filtered |>
  filter(non_euctr_registry == "DRKS") |>
  filter(unidirectional) |>
  nrow()

trn_drks_title_matched <- trn_filtered |>
  filter(non_euctr_registry == "DRKS") |>
  filter(is_title_matched) |>
  nrow()

trn_drks_pub <- trn_filtered |>
  filter(non_euctr_registry == "DRKS") |>
  filter(at_least_one_pub) |>
  nrow()

#########################################################################################################################
# Code to determine % of cross-reg matches that provide information on the other registry in one registry 

# Filter for crossregs between CT and EUCTR
ct_euctr_reg_linked <- trn_filtered |>
  #standardize_pairs() |>
  filter(non_euctr_registry == "ClinicalTrials.gov") |>
  filter(unidirectional | bidirectional)
#  left_join(manual_validation, by = "standardized_pair") |>
  #filter(is_true_crossreg)

# Count how many EUCTR trials mention the corresponding CT number in their registry
ct_euctr_mention <- ct_euctr_reg_linked |>
  filter((registry1 == "EudraCT" & trn2inreg1 == TRUE) | (registry2 == "EudraCT" & trn1inreg2 == TRUE)) |>
  select(standardized_pair)

# Count how many CT trials mention the corresponding EUCTR number in their registry

euctr_ct_mention <- ct_euctr_reg_linked |>
  filter((registry1 == "ClinicalTrials.gov" & trn2inreg1 == TRUE) | (registry2 == "ClinicalTrials.gov" & trn1inreg2 == TRUE)) |>
  select(standardized_pair)

# Count how many match on title
ct_euctr_title_match_count <- sum(replace_na(ct_euctr_reg_linked$is_title_matched, FALSE))

ct_euctr_title_match_percentage <- (ct_euctr_title_match_count/ nrow(ct_euctr_reg_linked) ) * 100


# Create the Venn Diagram
ct_euctr_venn <- list(
  "CT Number mentioned in EUCTR" = ct_euctr_mention$standardized_pair,
  "EUCTR Number mentioned in CT" = euctr_ct_mention$standardized_pair
)

base_ct_euctr_venn <- ggvenn(
  ct_euctr_venn,
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5,
  set_name_size = 0 # Turn off default labels for custom handling
)

# Add custom labels with text boxes
base_ct_euctr_venn +
  annotate("label", x = -1.2, y = 1.1, label = "CT Number mentioned in EUCTR",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.2, y = 1.1, label = "EUCTR Number mentioned in CT",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines"))



# Filter for crossregs between DRKS and EUCTR
drks_euctr_reg_linked <- trn_filtered |>
#  standardize_pairs() |>
  filter(non_euctr_registry == "DRKS") |>
  filter(unidirectional | bidirectional)
#  left_join(manual_validation, by = "standardized_pair") |>
#  filter(is_true_crossreg)

# Count how many EUCTR trials mention the corresponding DRKS number in their registry
drks_euctr_mention <- drks_euctr_reg_linked |>
  filter((registry1 == "EudraCT" & trn2inreg1 == TRUE) | (registry2 == "EudraCT" & trn1inreg2 == TRUE)) |>
  select(standardized_pair)


# Count how many DRKS trials mention the corresponding EUCTR number in their registry
euctr_drks_mention <- drks_euctr_reg_linked |>
  filter((registry1 == "DRKS" & trn2inreg1 == TRUE) | (registry2 == "DRKS" & trn1inreg2 == TRUE)) |>
  select(standardized_pair)

# Count how many match on title
drks_euctr_title_match_count <- sum(replace_na(drks_euctr_reg_linked$is_title_matched, FALSE))

drks_euctr_title_match_percentage <- (drks_euctr_title_match_count/ nrow(drks_euctr_reg_linked) ) * 100

# Create the Venn Diagram
drks_euctr_venn <- list(
  "DRKS Number mentioned in EUCTR" = euctr_drks_mention$standardized_pair,
  "EUCTR Number mentioned in DRKS" = drks_euctr_mention$standardized_pair
)

base_drks_euctr_venn <- ggvenn(
  drks_euctr_venn,
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5,
  set_name_size = 0 # Turn off default labels for custom handling
)

# Add custom labels with text boxes
base_drks_euctr_venn +
  annotate("label", x = -1.2, y = 1.1, label = "DRKS Number mentioned in EUCTR",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.2, y = 1.1, label = "EUCTR Number mentioned in DRKS",
           fill = "white", color = "black", size = 3, label.padding = unit(0.2, "lines"))

#########################################################################################################################
# Continue making upset plot:

# Make trn_filtered readable for ggupset package
trn_combos <-
  trn_filtered |>
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

# Filter for only DRKS analysis
trn_combos_drks <- trn_combos |>
  filter(non_euctr_registry == "DRKS")

# Filter for only CTgov analysis
trn_combos_ctgov <- trn_combos |>
  filter(non_euctr_registry == "ClinicalTrials.gov")

############################################################################
# Upset plot for manually validated TRN pairs
# Will show false positivity rate per category


manual_validation_upset <- read.csv("data/manual_validation_processed.csv") |>
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
  ggtitle("Manually validated pairs combinations, with false positive count shown ") + 
  geom_text(
    stat = "count", 
    aes(label = after_stat(count), group = is_true_crossreg), 
    position = position_stack(vjust = 0.5) # Place labels in the middle of each section
  ) + 
  scale_x_upset(n_intersections = 20) + 
  ylab("Number of pairs") + 
  xlab("Linking combinations") + 
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"), 
    legend.position = c(.85, .9), 
    axis.title.y = element_text(size = 11)
  )


############################################################################

# Upset plot showing all TRN pairs in analysis set
overall_crossreg_combinations <- trn_combos |>
  ggplot(aes(x=links)) +
  geom_bar() +
  ggtitle("Overall Combinations") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
  scale_x_upset(n_intersections = 20) +
  ylab("Number of pairs") +
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

# Upset plot showing all TRN pairs as proportions
overall_crossreg_combinations_proportions <- trn_combos |>
  ggplot(aes(x = links)) +
  geom_bar(aes(y = after_stat(count / 625 * 100))) +  # Set y as proportion for correct scaling
  ggtitle("Overall Combinations Proportions") +
  geom_text(stat = 'count', aes(y = after_stat(count / 625 * 100), label = sprintf("%.1f%%", after_stat(count / 625 * 100))), vjust = -1) + # Display as percentages
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(limits = c(0, 30), expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis limits and add small padding
  ylab("Proportion of pairs (%)") +  
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

# Upset plot showing all validated, TRUE TRN pairs as proportions
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

# Upset plot showing all TRN pairs in analysis set, divided by registry (not super readable yet, too much text)
registry_divided_combinations <- trn_combos |>
  ggplot(aes(x=links)) +
  facet_wrap(~non_euctr_registry) +
  geom_bar() +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
  scale_x_upset(n_intersections = 20) +
  ylab("Number of pairs") +
  xlab("Linking combinations") +
  ggupset::theme_combmatrix(
    combmatrix.panel.line.size = 0,
    combmatrix.label.text = element_text(family = "Roboto", size = 11)
  ) +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside  = c(.85, .9),
    axis.title.y = element_text(size = 11),
    # Remove all x-axis text
    axis.text.x = element_blank(),
    # Remove x-axis ticks
    axis.ticks.x = element_blank()
  )

# Upset plot of TRN pairs between DRKS and EUCTR
drks_crossreg_combinations <- trn_combos_drks |>
  ggplot(aes(x=links)) +
  ggtitle("DRKS Combinations") +
  geom_bar() +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
  scale_x_upset(n_intersections = 20) +
  ylab("Number of pairs") +
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

#  Upset plot of TRN pairs between DRKS and EUCTR (Proportions instead of counts)
drks_crossreg_combinations_proportions <- trn_combos_drks |>
  ggplot(aes(x = links)) +
  geom_bar(aes(y = after_stat(count / 62 * 100))) +  # Set y as proportion for correct scaling
  ggtitle("DRKS Combinations Proportions") +
  geom_text(stat = 'count', aes(y = after_stat(count / 62 * 100), label = sprintf("%.1f%%", after_stat(count / 62 * 100))), vjust = -1) + # Display as percentages
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis limits and add small padding
  ylab("Proportion of pairs (%)") +  
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = c(.85, .9),
    axis.title.y = element_text(size = 11)
  ) 

# Upset plot of TRN pairs between CTgov and EUCTR
ctgov_crossreg_combinations <- trn_combos_ctgov |>
  ggplot(aes(x=links)) +
  geom_bar() +
  ggtitle("ClinicalTrials.gov Combinations") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
  scale_x_upset(n_intersections = 20) +
  ylab("Number of pairs") +
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

# Upset plot of TRN pairs between CT and EUCTR (Proportions instead of counts)
ctgov_crossreg_combinations_proportions <- trn_combos_ctgov |>
  ggplot(aes(x = links)) +
  geom_bar(aes(y = after_stat(count / 563 * 100))) +  # Set y as proportion for correct scaling
  ggtitle("ClinicalTrials.gov Combinations Proportions") +
  geom_text(stat = 'count', aes(y = after_stat(count / 563 * 100), label = sprintf("%.1f%%", after_stat(count / 563 * 100))), vjust = -1) + # Display as percentages
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(limits = c(0, 40), expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis limits and add small padding
  ylab("Proportion of pairs (%)") +  
  xlab("Linking combinations") +
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = c(.85, .9),
    axis.title.y = element_text(size = 11)
  ) 

# # Move y axis label closer to plot
# # Thanks to https://stackoverflow.com/questions/68593982
# plot_upset_links_reg_pub_registry +
#   theme(axis.title.y=element_blank()) +
#   annotate(geom = "text", x = -0.2, y = 6500, label = "count", angle = 90, size=4) +
#   coord_cartesian(xlim = c(1, 8), clip = "off")

# `ggupset` doesn't currently allow to change label order
# https://github.com/const-ae/ggupset/issues/20

# SAVE UPSET PLOTS AS PDFs
# For now, not sure what we will need later

ggsave(
  "overall_crossreg_combinations.pdf",
  overall_crossreg_combinations,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "overall_crossreg_combinations_proportions.pdf",
  overall_crossreg_combinations_proportions,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "registry_divided_combinations.pdf",
  registry_divided_combinations,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "drks_crossreg_combinations.pdf",
  drks_crossreg_combinations,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "drks_crossreg_combinations_proportions.pdf",
  drks_crossreg_combinations_proportions,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "ctgov_crossreg_combinations.pdf",
  ctgov_crossreg_combinations,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  "ctgov_crossreg_combinations_proportions.pdf",
  ctgov_crossreg_combinations_proportions,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

