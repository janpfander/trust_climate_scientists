############################################################
# Simulate follow-up sample for preregistration
# Climate trust megastudy
#
# Design:
#   - ~1 week after the main experiment
#   - Participants from the 5 strongest intervention conditions
#     + the control condition are re-invited
#   - Only outcome measures are re-assessed (no pre-treatment
#     variables repeated)
#   - ~30% attrition on top of the main study's 10%
#   - Linked to main data via shared participant `id`
############################################################

library(tidyverse)

set.seed(456)

############################################################
# Helper functions (same as main simulation)
############################################################

r_slider <- function(n) pmin(pmax(rnorm(n, 50, 30), 0), 100)

############################################################
# Step 1: Load main dataset & identify eligible participants
############################################################

main <- read_csv("data/simulation/main_sample_preregistration.csv",
                 show_col_types = FALSE)

# In the real study, the 5 strongest conditions will be
# identified empirically. Here we randomly designate 5
# intervention conditions as "top 5" for simulation purposes.
top5_conditions <- sample(paste0("intervention_", 1:20), 5)

eligible_conditions <- c(top5_conditions, "control")

cat("Follow-up eligible conditions:\n")
print(eligible_conditions)

# Eligible pool: participants who completed the main study
# (non-missing on primary outcome) and are in an eligible condition
eligible <- main |>
  filter(
    condition %in% eligible_conditions,
    !is.na(trust_multidimensional)   # completed main study
  )

cat("\nEligible participants (completed main study, in eligible condition):",
    nrow(eligible), "\n")

############################################################
# Step 2: Simulate follow-up attrition (~30%)
############################################################

followup <- eligible |>
  group_by(condition) |>
  slice_sample(prop = 0.70) |>    # 30% attrition
  ungroup()

cat("Retained after follow-up attrition (30%):", nrow(followup), "\n\n")
cat("N per condition:\n")
print(count(followup, condition))

############################################################
# Step 3: Simulate follow-up outcome measures
#
# Only the id (linking key) and condition are carried over
# from the main dataset. All outcome measures are re-assessed.
############################################################

n <- nrow(followup)

followup_outcomes <- followup |>
  select(id) |>   # linking key only; no pre-treatment vars
  
  # ----------------------------------------------------------
# Primary outcome: multidimensional trust (12 items + composites)
# ----------------------------------------------------------
bind_cols(
  map_dfc(1:3, ~ r_slider(n)) |> set_names(paste0("trust_competence_",  1:3)),
  map_dfc(1:3, ~ r_slider(n)) |> set_names(paste0("trust_integrity_",   1:3)),
  map_dfc(1:3, ~ r_slider(n)) |> set_names(paste0("trust_benevolence_", 1:3)),
  map_dfc(1:3, ~ r_slider(n)) |> set_names(paste0("trust_openness_",    1:3))
) |>
  mutate(
    trust_competence       = rowMeans(pick(starts_with("trust_competence_"))),
    trust_integrity        = rowMeans(pick(starts_with("trust_integrity_"))),
    trust_benevolence      = rowMeans(pick(starts_with("trust_benevolence_"))),
    trust_openness         = rowMeans(pick(starts_with("trust_openness_"))),
    trust_multidimensional = rowMeans(pick(
      trust_competence, trust_integrity, trust_benevolence, trust_openness
    ))
  ) |>
  
  # ----------------------------------------------------------
# Secondary outcomes
# ----------------------------------------------------------
mutate(
  trust_post    = r_slider(n),
  distrust_post = r_slider(n),
  
  # Donation (two boxes summing to 10)
  donation_ams  = round(runif(n, 0, 10), 1),
  donation_self = 10 - donation_ams,
  
  # Newsletter (binary)
  newsletter_signup = sample(c(TRUE, FALSE), n,
                             replace = TRUE, prob = c(0.2, 0.8)),
  
  # Funding perceptions
  funding_perceptions = r_slider(n),
  
  # Scientists' role in policymaking (4 items)
  policy_role_1 = r_slider(n),
  policy_role_2 = r_slider(n),
  policy_role_3 = r_slider(n),
  policy_role_4 = r_slider(n),
  policy_role_mean = rowMeans(pick(starts_with("policy_role_")
                                   & !ends_with("mean"))),
  
  # Institutional trust (5 institutions)
  inst_trust_epa          = r_slider(n),
  inst_trust_nasa         = r_slider(n),
  inst_trust_noaa         = r_slider(n),
  inst_trust_universities = r_slider(n),
  inst_trust_federal_gov  = r_slider(n),
  inst_trust_mean         = rowMeans(pick(starts_with("inst_trust_")
                                          & !ends_with("mean")))
) |>
  
  # ----------------------------------------------------------
# Tertiary outcomes
# ----------------------------------------------------------
mutate(
  belief_post = r_slider(n),
  
  # Climate concern (3 items)
  concern_1    = r_slider(n),
  concern_2    = r_slider(n),
  concern_3    = r_slider(n),
  concern_mean = rowMeans(pick(concern_1, concern_2, concern_3)),
  
  # Policy support
  policy_general = r_slider(n),
  
  policy_specific_1 = r_slider(n),
  policy_specific_2 = r_slider(n),
  policy_specific_3 = r_slider(n),
  policy_specific_4 = r_slider(n),
  policy_specific_5 = r_slider(n),
  policy_specific_6 = r_slider(n),
  policy_specific_7 = r_slider(n),
  policy_specific_mean = rowMeans(pick(starts_with("policy_specific_")
                                       & !ends_with("mean"))),
  
  # Individual mitigation behaviors (4 with NA escape, 2 plain)
  behavior_meat      = if_else(runif(n) < 0.08, NA_real_, r_slider(n)),
  behavior_transport = if_else(runif(n) < 0.12, NA_real_, r_slider(n)),
  behavior_solar     = if_else(runif(n) < 0.15, NA_real_, r_slider(n)),
  behavior_fly       = if_else(runif(n) < 0.10, NA_real_, r_slider(n)),
  behavior_talk      = r_slider(n),
  behavior_donate    = r_slider(n),
  behavior_mean      = rowMeans(
    pick(behavior_meat, behavior_transport, behavior_solar,
         behavior_fly, behavior_talk, behavior_donate),
    na.rm = TRUE
  )
)

############################################################
# Step 4: Save
############################################################

write_csv(followup_outcomes,
          "data/simulation/followup_sample_preregistration.csv")

cat("\nFollow-up dataset saved.\n")
cat("Rows:", nrow(followup_outcomes), "\n")
cat("Columns:", ncol(followup_outcomes), "\n")