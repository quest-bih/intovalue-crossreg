# Analyze prospective registrations in IntoValue trials (ClinicalTrials.gov/DRKS)
# Adapted from Vladislav Nachev and Delwen Franzen
# Download date DRKS: 20 December 2024

library(readr)
library(dplyr)
library(here)
library(dRks)
library(purrr)
library(lubridate)


# Download and process DRKS data ------------------------------------------

##### load IntoValue dataset from OSF and pull DRKS trial IDs

drks_ids <- readr::read_csv("https://osf.io/download/mkgux") |>
  filter(registry == "DRKS") |>
  
  # filter out invalid DRKS IDs that no longer resolve in the registry (n=3)
  filter(id != "DRKS00004097", id != "DRKS00003807", id != "DRKS00004108") |>
  pull(id) |> head(5)

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
  write_csv(here("data", "prereg_processed", "iv_drks_prereg_test.csv"))
