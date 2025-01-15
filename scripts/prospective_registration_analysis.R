# Analyze prospective registrations in IntoValue trials (ClinicalTrials.gov/DRKS)
# Adapted from Vladislav Nachev and Delwen Franzen
# Download date DRKS: 20 December 2024; AACT: 27 September 2024

library(readr)
library(dplyr)
library(purrr)
library(here)
library(stringr)
library(ggplot2)
library(dRks)
library(lubridate)

# Load in list of screened trial pairs to limit analysis to manually confirmed crossreg
manual_screening <- read_csv(here("data", "manual_validation_processed.csv"))
manual_confirmed <- manual_screening |>
  filter(is_true_crossreg)

# DRKS
##### load IntoValue dataset from OSF and pull DRKS trial IDs

drks_ids <- readr::read_csv("https://osf.io/download/mkgux") |>
  filter(registry == "DRKS") |>
  
  # filter out invalid DRKS IDs that no longer resolve in the registry (n=3)
  filter(id != "DRKS00004097", id != "DRKS00003807", id != "DRKS00004108") |>
  pull(id)

slow_download_drks <- slowly(download_drks, rate = rate_delay(1)) # this delay worked for me without issues

##### assuming you want to download the html to a "raw" subfolder, change path if needed

drks_ids |>
  walk(\(x) slow_download_drks(x, dir = here("data", "prereg_raw")))

drks_prereg_info <- map(drks_ids, \(x) parse_drks_study(filepath = file.path(here("data", "prereg_raw"), x)), .progress = TRUE)

drks_prereg_info <- drks_prereg_info |>
  list_rbind() |>
  mutate(start_date = ymd(start_date),
         registration_date = ymd(registration_date),
         has_prospective_registration = floor_date(start_date, unit = "month") >=
           floor_date(registration_date, unit = "month")) |>
  rename(id = drks_id) |>
  select(id, registration_date, start_date, has_prospective_registration)

drks_prereg_info |>
  write_csv(here("data", "prereg_processed", "iv_drks_prereg.csv"))

# CLinicalTrials.gov
# Data was downloaded from AACT (20240927) and processed to obtain prospective registration
# has_prospective_registration = floor_date(start_date, unit = "month") >=
# floor_date(study_first_submitted_date, unit = "month")

ctgov_prereg_info <- read_csv(here("data", "prereg_processed", "iv_ctgov_prereg.csv")) |>
  rename(id = nct_id, registration_date = study_first_submitted_date) |>
  select(id, registration_date, start_date, has_prospective_registration)

# Combine DRKS and ctgov data
prereg_combined <- bind_rows(drks_prereg_info, ctgov_prereg_info) |>
  mutate(registry = if_else(str_detect(id, "NCT"), "ClinicalTrials.gov", "DRKS")) |>
  relocate(registry, .after = id)

# Explore data
prereg_combined |> 
  filter(is.na(has_prospective_registration)) |>
  count(registration_date, start_date)

# Exclude trials with no (actual) start date in the registry (n=5)
prereg_combined <- prereg_combined |>
  filter(!is.na(has_prospective_registration))

write_csv(prereg_combined, here("data", "prereg_processed", "iv_prereg_combined.csv"))

# Filter prereg_combined so that it only includes TRNs that have been confirmed as crossregs
prereg_combined_confirmed <- prereg_combined |>
  filter(id %in% manual_confirmed$trn1 | id %in% manual_confirmed$trn2)

# Plot number/proportion of trials in each registry prospectively/retrospectively registered
# Summarize data
summary <- prereg_combined_confirmed |> 
  group_by(registry, has_prospective_registration) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(registry) |> 
  mutate(proportion = count / sum(count))

# Plot grouped bar chart with counts and proportions
ggplot(summary, aes(x = registry, y = proportion * 100, fill = has_prospective_registration)) +
  geom_bar(stat = "identity", position = "dodge") + # Grouped bars
  geom_text(
    aes(label = sprintf("%.1f%% (%d)", round(proportion * 100, 1), count)), # Percentage with count
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    x = "Registry",
    y = "Percentage of Trials",
    fill = "Prospective registration"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "grey"),
    labels = c("TRUE" = "Registered prospectively", "FALSE" = "Registered retrospectively")
  ) +
  theme_minimal()

