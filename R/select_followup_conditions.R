# Freeze the follow-up-eligible conditions — Trust-In Megastudy
# -----------------------------------------------------------------------------
# Run ONCE during wave-1 fieldwork to lock the conditions that receive a
# follow-up invitation: the 5 most effective interventions plus control.
#
# Interventions are ranked by the preregistered main treatment-effect model on
# the primary outcome (OLS, HC2 robust SEs, covariates age/gender/race,
# Benjamini-Hochberg adjustment), fit on the preregistered dataset
# (cleaned.rds). The top 5 by point estimate, plus "control", are written to
# followup_eligible_conditions.csv; the full 20-condition ranking is written to
# followup_top5_lock_log.csv for transparency.
#
# Because wave-1 collection spans weeks and follow-up invitations are rolling,
# this set must be frozen early, from a PARTIAL sample, before the first invites
# are due. That is a documented deviation from the preregistration — see
# cloudresearch/generate_followup_invitations.qmd.
#
# Usage:  Rscript R/select_followup_conditions.R
# -----------------------------------------------------------------------------

library(tidyverse)
library(here)

# Reused, not re-derived: the exact preregistered main treatment-effect model.
source(here("R/functions/statistics.R"))

PRIMARY_OUTCOME <- "trust_multidimensional"
COVARIATES      <- c("age", "gender", "race") # mandatory covariates (no missingness)
N_TOP           <- 5

OUT_FILE <- here("cloudresearch/followup_eligible_conditions.csv")
LOG_FILE <- here("cloudresearch/followup_top5_lock_log.csv")

# The eligible set must not change mid-fieldwork. Re-locking is a deliberate
# act: delete both files by hand, then re-run.
if (file.exists(OUT_FILE)) {
  stop(
    "Eligible conditions are already locked: ", OUT_FILE, "\n",
    "To deliberately re-lock, delete that file (and ", basename(LOG_FILE),
    ") by hand, then re-run."
  )
}

cleaned <- readRDS(here("data/experiment/cleaned.rds"))

ranking <- run_main_treatment_model(
  data          = cleaned,
  outcome       = PRIMARY_OUTCOME,
  condition_var = "condition",
  covariates    = COVARIATES
) |>
  filter(outcome == PRIMARY_OUTCOME) |>
  arrange(desc(estimate)) |>
  mutate(rank = row_number())

eligible <- c(slice_head(ranking, n = N_TOP)$condition, "control")

# Eligible set used by generate_followup_invitations.qmd. NOTE: a few condition
# labels carry a trailing newline (from cleaning.qmd); write_csv/read_csv
# round-trip these via quoting — never hand-edit this file.
write_csv(tibble(condition = eligible), OUT_FILE)

# Full ranking log: lock date, sample size at lock, and all 20 estimates.
ranking |>
  transmute(
    lock_date            = format(Sys.Date()),
    n_at_lock            = sum(!is.na(cleaned[[PRIMARY_OUTCOME]])),
    rank,
    condition,
    estimate,
    p.value,
    p.value_adjusted,
    significant_adjusted,
    locked_eligible      = condition %in% eligible
  ) |>
  write_csv(LOG_FILE)

message(sprintf(
  "Locked %d eligible conditions (top %d + control) -> %s",
  length(eligible), N_TOP, OUT_FILE
))
