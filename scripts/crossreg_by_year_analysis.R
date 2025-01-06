# Script to Plot of the number of potential cross-registrations over time 

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
library(readr)

trn_manual_checks <- read_rds("data/crossreg_pipeline_output.rds")

url <- "https://osf.io/mkgux/download"
intovalue <- read_csv(url)

intovalue <- intovalue |>
  select(id, completion_year)|>
  filter(!is.na(completion_year))|>
  distinct() 
  
trn_filtered <- trn_manual_checks |>
  filter(priority <= 4) |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")


# Add flag to intovalue to identify potential crossregs
# Add a new column to `intovalue` checking if `id` is in `trn1` or `trn2` of `trn_filtered`
intovalue <- intovalue |> 
  mutate(
    potential_crossreg = if_else(
      id %in% trn_filtered$trn1 | id %in% trn_filtered$trn2,
      1, 0
    )
  )

# Summarize data by year
summary_by_year <- intovalue |> 
  group_by(completion_year) |> 
  summarise(
    total_trials = n(),
    crossreg_trials = sum(potential_crossreg, na.rm = TRUE)
  ) |> 
  mutate(
    non_crossreg_trials = total_trials - crossreg_trials,
    crossreg_percentage = round((crossreg_trials / total_trials) * 100, 1)
  ) |> 
  pivot_longer(
    cols = c(crossreg_trials, non_crossreg_trials),
    names_to = "crossreg_status",
    values_to = "count"
  ) |> 
  mutate(
    crossreg_status = recode(crossreg_status,
                             crossreg_trials = "TRUE",
                             non_crossreg_trials = "FALSE")
  )

# Plot the data
ggplot(summary_by_year, aes(x = completion_year, y = count, fill = crossreg_status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(
      label = ifelse(crossreg_status == "TRUE", 
                     paste0(crossreg_percentage, "%"), "")
    ), 
    position = position_stack(vjust = 0.5), 
    color = "black", size = 4
  ) +
  scale_fill_manual(values = c("TRUE" = "#f0e442", "FALSE" = "#009e73")) +
  scale_x_continuous(
    breaks = 2009:2017,  # Explicitly set discrete years as ticks
    labels = as.character(2009:2017) # Convert breaks to discrete labels
  ) +
  labs(
    x = "Completion Year",
    y = "Number of Trials",
    fill = "Potential EUCTR cross-registration"
  ) +
  theme_minimal()



