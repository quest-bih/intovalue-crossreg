# Script to plot the number of potential cross-registrations over time 

library(tidyverse)
library(here)

potential_crossreg <- read_rds(here("data", "crossreg_pipeline_output.rds"))

# Prepare/process tables for use
# Here, we are filtering for trials with a priority of 4 or lower (priority 4 = trial identifier mentioned in another trial's related publication), as our pilot manual review of trial pairs with priority > 4 revealed low precision for correctly identified true cross-registrations
# Then, we filter for TRNs that have not been removed from the DRKS registry. We also filter for EUCTR TRNs that still resolve in the EUCTR registry. These steps ensure that all remaining TRN pairs can be looked up in their respective registries and screened
# Finally, we filter out one specific row, in which the TRN '2008-004408-29' is incorrectly marked as a trial that resolves in the EUCTR database.
trn_filtered <- potential_crossreg |>
  filter(priority <= 4) |>
  filter(drks_removed == FALSE & euctr_id_in_euctr == TRUE) |>
  filter(trn2 != "2008-004408-29")

url <- "https://osf.io/mkgux/download"
intovalue <- read_csv(url)

intovalue <- intovalue |>
  select(id, completion_year)
  
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
# NOTE: trn_filtered identifies 625 distinct pairs of TRNs that are potential cross-registrations
# However, in the `intovalue` data frame used above, only 614 trials are marked as potential cross-registrations
# This discrepancy is due to the fact that 11 intovalue TRNs (in ClinicalTrials.gov or DRKS) are connected to more than one EUCTR trial. 
# Thus, while these connections are captured in trn_filtered, each intovalue TRN is only listed once in the `intovalue` data frame.
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
                     paste0(round(crossreg_percentage), "%"), "")
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
    y = "Number of IntoValue trials (DRKS or ClinicalTrials.gov)",
    fill = "Potential EUCTR cross-registration"
  ) +
  theme_minimal()



