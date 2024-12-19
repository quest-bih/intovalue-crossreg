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

  
