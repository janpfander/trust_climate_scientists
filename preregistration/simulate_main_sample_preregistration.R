############################################################
# Simulate main sample for preregistration
# Climate trust megastudy
# Updated to match current questionnaire
############################################################

library(tidyverse)

set.seed(123)

############################################################
# Global parameters
############################################################

N_per_condition         <- 1000
N_per_control_condition <- 2000
N_interventions         <- 20
N                       <- N_per_condition * N_interventions + N_per_control_condition

conditions <- c(
  paste0("intervention_", 1:N_interventions),
  "control"
)

# Control sub-texts (assigned only to control condition participants)
control_subtexts <- c("neckties", "baseball", "dances")

############################################################
# Helper functions
############################################################

# Slider: bounded [0, 100]
r_slider <- function(n) pmin(pmax(rnorm(n, 50, 30), 0), 100)

# Likert: integer scale from `lo` to `hi`
r_likert <- function(n, lo = 1, hi = 7) sample(lo:hi, n, replace = TRUE)

# Generic categorical draw
r_cat <- function(n, levels) factor(sample(levels, n, replace = TRUE), levels = levels)

############################################################
# Core dataset
############################################################

dat <- tibble(
  id        = 1:N,
  condition = sample(c(
    rep(paste0("intervention_", 1:N_interventions), each = N_per_condition),
    rep("control", N_per_control_condition)
  ))
)

############################################################
# Control sub-text assignment
# (NA for all intervention participants)
############################################################

dat <- dat |>
  mutate(
    control_subtext = if_else(
      condition == "control",
      sample(control_subtexts, N, replace = TRUE),
      NA_character_
    ),
    control_subtext = factor(control_subtext, levels = control_subtexts)
  )

############################################################
# Demographics
############################################################

dat <- dat |>
  mutate(
    
    # Gender [Male; Female; Other]
    gender = r_cat(N, c("Male", "Female", "Other")),
    
    # Year of birth (text box) → age derived
    year_birth = round(pmin(pmax(rnorm(N, 1979, 18), 1934), 2006)),
    age        = 2025L - year_birth,
    
    # Race/ethnicity — multi-select; simplified to single dominant category
    # In real data this will be a comma-separated multi-select string
    race = r_cat(N, c(
      "White / Caucasian",
      "Black / African American",
      "Hispanic / Latino",
      "Asian / Asian American",
      "Other"
    )),
    
    # Education — 6 levels (updated from old 5-level version)
    education = r_cat(N, c(
      "Less than high school",
      "High school diploma / GED",
      "Some college or Associate's degree",
      "Bachelor's degree",
      "Master's degree / Professional degree",
      "Doctorate degree / Ph.D."
    )),
    
    # Education in climate science — 3 items, each Yes / No / Not applicable
    education_climate_primary    = r_cat(N, c("Yes", "No", "Not applicable")),
    education_climate_highschool = r_cat(N, c("Yes", "No", "Not applicable")),
    education_climate_university = r_cat(N, c("Yes", "No", "Not applicable")),
    
    # Income — 5 brackets anchored to Pew thresholds
    # (based on 2024 US Census median household income of $83,730)
    income = r_cat(N, c(
      "Less than $30,000",
      "$30,000 to $55,999",
      "$56,000 to $99,999",
      "$100,000 to $167,999",
      "$168,000 or more"
    )),

    # Household size — 6 levels
    household_size = r_cat(N, c("1", "2", "3", "4", "5", "6 or more")),

    # Social class — 4 levels
    social_class = r_cat(N, c(
      "Lower class",
      "Working class",
      "Middle class",
      "Upper class"
    )),

    # Urban / rural — 4 levels (Pew wording)
    urban_rural = r_cat(N, c(
      "A large city",
      "A suburb near a large city",
      "A small city or town",
      "A rural area"
    )),

    # Zip code
    zip_code = sprintf("%05d", sample(10000:99999, N, replace = TRUE))
  )

############################################################
# Partisan identity
############################################################
dat <- dat |>
  mutate(
    party = r_cat(N, c("Republican", "Democrat", "Independent", "Other")),
    
    # Party importance — slider, only asked of Republicans and Democrats
    # NA for Independents / Other
    party_importance = if_else(
      party %in% c("Republican", "Democrat"),
      r_slider(N),
      NA_real_
    )
  )

############################################################
# Religion
############################################################

dat <- dat |>
  mutate(
    religion = r_cat(N, c(
      "I am not religious",
      "Protestant",
      "Catholic",
      "Orthodox Christian",
      "Mormon",
      "Muslim",
      "Jewish",
      "Hindu",
      "Buddhist",
      "Other religion"
    )),

    # Born-again — only asked of Protestant, Catholic, Orthodox Christian, Mormon
    born_again = if_else(
      religion %in% c("Protestant", "Catholic", "Orthodox Christian", "Mormon"),
      r_cat(N, c("Yes", "No")),
      NA_character_
    ),
    born_again = factor(born_again, levels = c("Yes", "No")),

    # Religiosity slider — only asked if not "I am not religious"
    religiosity = if_else(
      religion != "I am not religious",
      r_slider(N),
      NA_real_
    )
  )

############################################################
# Need for epistemic autonomy (6 items, 1–7 Likert)
# Item 6 is reverse-scored
############################################################

epist_auton_items <- map_dfc(1:6, ~ r_likert(N, 1, 7)) |>
  set_names(paste0("epist_auton_", 1:6))

dat <- bind_cols(dat, epist_auton_items) |>
  mutate(
    # Reverse-score item 6 before computing mean
    epist_auton_6r  = 8L - epist_auton_6,
    epist_auton_mean = rowMeans(cbind(
      epist_auton_1, epist_auton_2, epist_auton_3,
      epist_auton_4, epist_auton_5, epist_auton_6r
    ))
  )

############################################################
# Pre-treatment measures
############################################################

dat <- dat |>
  mutate(
    # Belief in climate change (slider 0–100)
    belief_pre = r_slider(N),
    
    # Single-item trust in climate scientists (slider 0–100)
    trust_pre  = r_slider(N)
  )

# ----------------------------------------------------------
# Alienation from climate science
# ----------------------------------------------------------
# Institutional alienation (2 items, 1–7 Likert)
alien_inst_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_inst_1", "alien_inst_2"))

# Social distance (2 items, 1–7 Likert)
alien_social_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_social_1", "alien_social_2"))

# Spatial distance (2 items, 1–7 Likert)
alien_spatial_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_spatial_1", "alien_spatial_2"))

# Informational distance (10 items, 7-category ordinal)
alien_info_levels <- c(
  "Never",
  "Once or twice during the year",
  "Several times during the year",
  "Once or twice a month",
  "Once or twice a week",
  "Almost every day",
  "Once or more per day"
)

alien_info_items <- map_dfc(1:10, ~ r_cat(N, alien_info_levels)) |>
  set_names(paste0("alien_info_", 1:10))

dat <- bind_cols(dat, alien_inst_items, alien_social_items,
                 alien_spatial_items, alien_info_items) |>
  mutate(
    alien_inst_mean    = rowMeans(cbind(alien_inst_1, alien_inst_2)),
    alien_social_mean  = rowMeans(cbind(alien_social_1, alien_social_2)),
    alien_spatial_mean = rowMeans(cbind(alien_spatial_1, alien_spatial_2)),
    # Recode info items to numeric (1–7) for mean
    across(starts_with("alien_info_"),
           ~ as.integer(factor(.x, levels = alien_info_levels)),
           .names = "{.col}_num"),
    alien_info_mean = rowMeans(pick(ends_with("_num") & starts_with("alien_info_")))
  )

############################################################
# Primary outcome: Multidimensional trust in climate
# scientists (12 items, sliders 0–100)
############################################################

trust_competence_items  <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_competence_",  1:3))
trust_integrity_items   <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_integrity_",   1:3))
trust_benevolence_items <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_benevolence_", 1:3))
trust_openness_items    <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_openness_",    1:3))

dat <- bind_cols(dat, trust_competence_items, trust_integrity_items,
                 trust_benevolence_items, trust_openness_items) |>
  mutate(
    trust_competence       = rowMeans(trust_competence_items),
    trust_integrity        = rowMeans(trust_integrity_items),
    trust_benevolence      = rowMeans(trust_benevolence_items),
    trust_openness         = rowMeans(trust_openness_items),
    trust_multidimensional = rowMeans(cbind(
      trust_competence, trust_integrity, trust_benevolence, trust_openness
    ))
  )

############################################################
# Secondary outcomes
############################################################

# ----------------------------------------------------------
# Single-item trust & distrust (sliders 0–100)
# ----------------------------------------------------------
dat <- dat |>
  mutate(
    trust_post   = r_slider(N),
    distrust_post = r_slider(N)
  )

# ----------------------------------------------------------
# Donation to ams (two boxes summing to 10)
# ----------------------------------------------------------
dat <- dat |>
  mutate(
    donation_ams  = round(runif(N, 0, 10), 1),
    donation_self = 10 - donation_ams
  )

# ----------------------------------------------------------
# Newsletter subscription (binary)
# ----------------------------------------------------------
dat <- dat |>
  mutate(
    newsletter_signup = sample(c(TRUE, FALSE), N, replace = TRUE, prob = c(0.2, 0.8))
  )

# ----------------------------------------------------------
# Funding perceptions (1 slider, 0–100)
# 0 = far too little, 50 = about right, 100 = far too much
# ----------------------------------------------------------
dat <- dat |>
  mutate(funding_perceptions = r_slider(N))

# ----------------------------------------------------------
# Scientists' role in policymaking (4 items, sliders 0–100)
# ----------------------------------------------------------
policy_role_items <- map_dfc(1:4, ~ r_slider(N)) |>
  set_names(paste0("policy_role_", 1:4))

dat <- bind_cols(dat, policy_role_items) |>
  mutate(policy_role_mean = rowMeans(policy_role_items))

# ----------------------------------------------------------
# Institutional trust — 5 institutions (sliders 0–100)
# NOTE: asked post-treatment only; pre-treatment inst. trust
# is not in the current questionnaire
# ----------------------------------------------------------
institutions <- c("epa", "nasa", "noaa", "universities", "federal_gov")

inst_trust_items <- map_dfc(institutions, ~ r_slider(N)) |>
  set_names(paste0("inst_trust_", institutions))

dat <- bind_cols(dat, inst_trust_items) |>
  mutate(inst_trust_mean = rowMeans(inst_trust_items))

############################################################
# Tertiary outcomes
############################################################

# ----------------------------------------------------------
# Belief in climate change — post (slider 0–100)
# ----------------------------------------------------------
dat <- dat |>
  mutate(belief_post = r_slider(N))

# ----------------------------------------------------------
# Climate change concern (3 items, sliders 0–100)
# ----------------------------------------------------------
concern_items <- map_dfc(1:3, ~ r_slider(N)) |>
  set_names(paste0("concern_", 1:3))

dat <- bind_cols(dat, concern_items) |>
  mutate(concern_mean = rowMeans(concern_items))

# ----------------------------------------------------------
# General climate policy support (1 slider, 0–100)
# ----------------------------------------------------------
dat <- dat |>
  mutate(policy_general = r_slider(N))

# ----------------------------------------------------------
# Specific climate policies (7 items, sliders 0–100)
# ----------------------------------------------------------
policy_specific_items <- map_dfc(1:7, ~ r_slider(N)) |>
  set_names(paste0("policy_specific_", 1:7))

dat <- bind_cols(dat, policy_specific_items) |>
  mutate(policy_specific_mean = rowMeans(policy_specific_items))

# ----------------------------------------------------------
# Individual-level climate mitigation behaviors
# 6 items. Four items have a "not applicable" escape option:
#   item 1 (meat):      "I am a vegetarian"
#   item 2 (transport): "I never drive by myself"
#   item 3 (solar):     "I already have enough solar panels installed"
#   item 4 (flying):    "I never fly"
# Items 5 (talk) and 6 (donate) are plain sliders.
#
# We simulate each item as either a slider value (0–100) or
# its NA string, then store binary NA flags separately.
# ----------------------------------------------------------

# Items with a not-applicable escape option
na_labels <- c(
  "I am a vegetarian",
  "I never drive by myself",
  "I already have enough solar panels installed",
  "I never fly"
)

simulate_behavior_item <- function(n, na_label, p_na = 0.1) {
  is_na <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(p_na, 1 - p_na))
  slider_val <- r_slider(n)
  list(
    value  = if_else(is_na, NA_real_,      slider_val),
    not_applicable = if_else(is_na, na_label, NA_character_)
  )
}

item_names  <- c("meat", "transport", "solar", "fly")
p_na_values <- c(0.08, 0.12, 0.15, 0.10)   # realistic NA rates per item

behavior_na_items <- map2(na_labels, p_na_values, ~ simulate_behavior_item(N, .x, .y))

behavior_cols <- map_dfc(seq_along(item_names), function(i) {
  tibble(
    !!paste0("behavior_", item_names[i])    := behavior_na_items[[i]]$value,
    !!paste0("behavior_", item_names[i], "_na") := behavior_na_items[[i]]$not_applicable
  )
})

# Items 5 and 6: plain sliders, no escape option
behavior_cols <- behavior_cols |>
  mutate(
    behavior_talk   = r_slider(N),
    behavior_donate = r_slider(N)
  )

dat <- bind_cols(dat, behavior_cols) |>
  mutate(
    # Mean across slider items only (excluding not-applicable responses)
    behavior_mean = rowMeans(
      cbind(behavior_meat, behavior_transport, behavior_solar,
            behavior_fly, behavior_talk, behavior_donate),
      na.rm = TRUE
    )
  )

############################################################
# Attrition
############################################################

# --- Tier 1: survey non-completion (10% per condition) ---
# Affects all outcomes simultaneously
dat <- dat |>
  group_by(condition) |>
  mutate(
    dropout = row_number() %in% sample(n(), size = ceiling(0.10 * n()))
  ) |>
  ungroup()

# --- Tier 2: item-level skipping (rare, outcome-specific) ---
# Applied only to participants who completed the survey
# Rates reflect realistic skip rates for each outcome type

item_skip_rates <- list(
  # Primary: low skip rate (prominent, early in block)
  trust_multidimensional   = 0.01,
  trust_competence         = 0.01,
  trust_integrity          = 0.01,
  trust_benevolence        = 0.01,
  trust_openness           = 0.01,
  
  # Secondary: slightly higher for sensitive/effortful items
  trust_post               = 0.01,
  distrust_post            = 0.01,
  donation_ams             = 0.03,  # real money = more skips
  donation_self            = 0.03,
  newsletter_signup        = 0.05,  # optional by design
  funding_perceptions      = 0.02,
  policy_role_mean         = 0.02,
  inst_trust_mean          = 0.02,
  
  # Tertiary: further in survey, slightly more fatigue
  belief_post              = 0.02,
  concern_mean             = 0.02,
  policy_general           = 0.02,
  policy_specific_mean     = 0.03,
  behavior_mean            = 0.03
)

# define outcome variables
outcome_vars <- c(
  # Primary
  paste0("trust_competence_",  1:3),
  paste0("trust_integrity_",   1:3),
  paste0("trust_benevolence_", 1:3),
  paste0("trust_openness_",    1:3),
  "trust_competence", "trust_integrity", "trust_benevolence",
  "trust_openness",   "trust_multidimensional",
  
  # Secondary
  "trust_post", "distrust_post",
  "donation_ams", "donation_self",
  "funding_perceptions",
  paste0("policy_role_", 1:4), "policy_role_mean",
  paste0("inst_trust_", institutions), "inst_trust_mean",
  
  # Tertiary
  "belief_post",
  paste0("concern_", 1:3), "concern_mean",
  "policy_general",
  paste0("policy_specific_", 1:7), "policy_specific_mean",
  "behavior_meat", "behavior_transport", "behavior_solar", "behavior_fly",
  "behavior_talk", "behavior_donate", "behavior_mean"
)

# apply dropout to all outcomes first
dat <- dat |>
  mutate(across(all_of(outcome_vars),
                ~ if_else(dropout, NA_real_, .x))) |>
  mutate(newsletter_signup = if_else(dropout, NA, newsletter_signup))

# then apply item-level skipping on top, only for completers
for (var in names(item_skip_rates)) {
  rate <- item_skip_rates[[var]]
  dat <- dat |>
    mutate(
      !!var := if_else(
        !dropout & runif(n()) < rate,
        if (is.logical(.data[[var]])) NA else NA_real_,
        .data[[var]]
      )
    )
}

dat <- dat |> select(-dropout)


############################################################
# Build factors
############################################################

# make condition a factor and set main control as baseline
dat <- dat |> 
  # make condition a factor and set main control as baseline
  mutate(
    condition = relevel(factor(condition), ref = "control")
  )

# check
# levels(data$condition)

# make all pre-treatment categorical variables factors
dat <- dat |> 
  mutate(
    # --- Demographics ---
    gender = factor(gender, levels = c(
      "Male", "Female", "Other"
    )),
    race = factor(race, levels = c(
      "White / Caucasian",
      "Black / African American",
      "Hispanic / Latino",
      "Asian / Asian American",
      "Other"
    )),
    education = factor(education, levels = c(
      "Less than high school",
      "High school diploma / GED",
      "Some college or Associate's degree",
      "Bachelor's degree",
      "Master's degree / Professional degree",
      "Doctorate degree / Ph.D."
    )),
    education_climate_primary    = factor(education_climate_primary,
                                          levels = c("Yes", "No", "Not applicable")),
    education_climate_highschool = factor(education_climate_highschool,
                                          levels = c("Yes", "No", "Not applicable")),
    education_climate_university = factor(education_climate_university,
                                          levels = c("Yes", "No", "Not applicable")),
    income = factor(income, levels = c(
      "Less than $30,000",
      "$30,000 to $55,999",
      "$56,000 to $99,999",
      "$100,000 to $167,999",
      "$168,000 or more"
    )),
    household_size = factor(household_size, levels = c(
      "1", "2", "3", "4", "5", "6 or more"
    )),
    social_class = factor(social_class, levels = c(
      "Lower class", "Working class", "Middle class", "Upper class"
    )),
    urban_rural = factor(urban_rural, levels = c(
      "A large city",
      "A suburb near a large city",
      "A small city or town",
      "A rural area"
    )),
    
    # --- Partisan identity ---
    party = factor(party, levels = c(
      "Republican", "Democrat", "Independent", "Other"
    )),
    
    # --- Religion ---
    religion = factor(religion, levels = c(
      "I am not religious",
      "Protestant",
      "Catholic",
      "Orthodox Christian",
      "Mormon",
      "Muslim",
      "Jewish",
      "Hindu",
      "Buddhist",
      "Other religion"
    ))
  )

############################################################
# Final dataset
############################################################

write_csv(dat, "data/simulation/main_sample_preregistration.csv")
saveRDS(dat, "data/simulation/main_sample_preregistration.rds")
