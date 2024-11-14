# A script to explore confirmed cross registrations that were identified through a publication 
library(tidyverse)
library(VennDiagram)
library(lubridate)
library(here)
library(fs)
library(stringr)
library(ctregistries)
library(cli)

# Load data 
manual_validation <- read.csv("data/manual_validation_processed.csv")
trn_manual_checks <- read_rds("data/crossreg_pipeline_output.rds")

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

# Filter for true cross-registrations
confirmed_crossreg_standardized <- manual_validation |>
  filter(is_true_crossreg) |>
  standardize_pairs() |>
  select(standardized_pair, is_true_crossreg)

trn_manual_checks_standardized <- standardize_pairs(trn_manual_checks)

# Join true crossreg with larger table by standardized_pair
confirmed_crossreg <- confirmed_crossreg_standardized |>
  left_join(trn_manual_checks_standardized, by = "standardized_pair") |>
  filter(is_true_crossreg)

# Filter for trials linked by publication in any way
pub_crossregs <- confirmed_crossreg |>
  filter(at_least_one_pub)

# Filter for trials linked by FT mention
pub_ft <- pub_crossregs |>
  filter(trn1_in_pub_ft | trn2_in_pub_ft)

# Filter for trials linked by SI mention
pub_si <- pub_crossregs |>
  filter(trn1_in_pub_si | trn2_in_pub_si)

# Filter for trials linked by abstract mention
pub_abs <- pub_crossregs |>
  filter(trn1_in_pub_abs | trn2_in_pub_abs)
