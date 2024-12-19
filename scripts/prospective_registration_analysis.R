# Script to analyze prospective vs retrospective registrations in cross-registrations


library(tidyverse)
library(readr)
library("readxl")
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

# Identify non-EUCTR TRN
trn_filtered <- trn_filtered |>
  mutate(
    euctr_trn = ifelse(registry1 == "EudraCT", trn1, ifelse(registry2 == "EudraCT", trn2, NA)),
    non_euctr_trn = ifelse(registry1 != "EudraCT", trn1, trn2),
    non_euctr_registry = ifelse(registry1 != "EudraCT", registry1, registry2)
  )

# Read in tables with prospective/retrospective classifications
drks_registration_type <- read.csv("data/iv_trials_drks_26_11_2024.csv") #|>
  select(drks_id, registration_type) |>
  rename(non_euctr_trn = drks_id) |>
  mutate(
    registration_type = ifelse(registration_type == "Prospective", "PROSPECTIVE", "RETROSPECTIVE")
  )


extra_drks_registration_type <- read_excel("data/trial_data_01-characteristics.xlsx") |>
  select(TRL_DRKS_ID, TRL_RegistrationType) |>
  rename(non_euctr_trn = TRL_DRKS_ID, registration_type = TRL_RegistrationType) |>
  filter(!is.na(registration_type))
  

ct_registration_type <- read.csv("data/iv_trials_ctgov_25_11_2024.csv") |>
  select(nct_id, has_prospective_registration) |>
  rename(non_euctr_trn = nct_id, registration_type = has_prospective_registration) |>
  filter(!is.na(registration_type)) |>
  mutate(
    registration_type = ifelse(registration_type, "PROSPECTIVE", "RETROSPECTIVE")
  )

# Combine all registration type tables
combined_registration_types <- rbind(drks_registration_type, extra_drks_registration_type, ct_registration_type)

# Join in registration_type column to large table
trn_registration_type <- trn_filtered |>
  left_join(combined_registration_types, by = "non_euctr_trn")

# Plot number/proportion of trials in each registry prospectively/retrospectively registered
# Summarize data
summary <- trn_registration_type |> 
  group_by(non_euctr_registry, registration_type) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(non_euctr_registry) |> 
  mutate(proportion = count / sum(count))

# Plot grouped bar chart with counts and proportions
ggplot(summary, aes(x = non_euctr_registry, y = count, fill = registration_type)) +
  geom_bar(stat = "identity", position = "dodge") + # Grouped bars
  geom_text(aes(label = sprintf("%.1f%%", proportion * 100)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) + # Add proportion labels
  labs(
    title = "Trial Registration Type by Registry",
    x = "Registry",
    y = "Number of Trials",
    fill = "Registration Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("PROSPECTIVE" = "steelblue", "RETROSPECTIVE" = "orange")) 

  
