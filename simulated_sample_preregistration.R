############################################################
# Simulated dataset for preregistration
# Climate trust megastudy
############################################################

library(tidyverse)

set.seed(123)

############################################################
# Global parameters
############################################################

N_per_condition <- 1000
N_interventions <- 20
N_control <- 2
N_conditions <- N_interventions + N_control
N <- N_per_condition*N_conditions

conditions <- c(
  paste0("intervention_", 1:N_interventions),
  "control_main",
  "control_interactive"
)

############################################################
# Helper functions
############################################################

# Generic slider: bounded [0, 100], identical everywhere
r_slider <- function(n) {
  pmin(pmax(rnorm(n, 50, 30), 0), 100)
}

# Generic categorical draw
r_cat <- function(n, levels) {
  factor(sample(levels, n, replace = TRUE), levels = levels)
}

############################################################
# Core dataset
############################################################

dat <- tibble(
  id = 1:N,
  condition = sample(conditions, N, replace = TRUE)
)

############################################################
# Demographics
############################################################

dat <- dat %>%
  mutate(
    gender = r_cat(N, c("Male", "Female", "Other")),
    age = round(pmin(pmax(rnorm(N, 45, 18), 18), 90)),
    education = r_cat(
      N,
      c(
        "Less than high school",
        "High school diploma / GED",
        "Some college or Associate's degree",
        "Bachelor's degree",
        "Postgraduate"
      )
    ),
    religion = r_cat(
      N,
      c(
        "No religion",
        "Christian",
        "Muslim",
        "Jewish",
        "Hindu",
        "Buddhist",
        "Other"
      )
    ),
    religiosity = r_slider(N),
    income_bracket = r_cat(
      N,
      c(
        "<25k", "25–35k", "35–50k", "50–75k", "75–100k",
        "100–125k", "125–150k", "150–175k", "175–200k", "200k+"
      )
    ),
    urban_rural = r_cat(N, c("Urban", "Rural")),
    race = r_cat(N, c("White", "Black", "Asian", "Hispanic")),
    zip_code = sprintf("%05d", sample(10000:99999, N, replace = TRUE))
  )

############################################################
# Political & ideological moderators
############################################################

dat <- dat %>%
  mutate(
    conservatism = r_cat(
      N,
      c(
        "Extremely liberal", "Liberal", "Slightly liberal",
        "Moderate",
        "Slightly conservative", "Conservative", "Extremely conservative"
      )
    ),
    party = r_cat(N, c("Democrat", "Republican", "Independent", "Other")),
    party_importance = r_slider(N)
  )

############################################################
# Pre-treatment beliefs
############################################################

dat <- dat %>%
  mutate(
    climate_belief_pre = r_slider(N),
    trust_scientists_single_pre = r_slider(N)
  )

############################################################
# Social Dominance Orientation (SDO)
############################################################

sdo_items <- replicate(4, r_slider(N)) %>% as_tibble()
colnames(sdo_items) <- paste0("sdo_item_", 1:4)

dat <- bind_cols(dat, sdo_items) %>%
  mutate(
    sdo_mean = rowMeans(select(., starts_with("sdo_item_")))
  )

############################################################
# Subjective inequality
############################################################

ineq_items <- replicate(4, r_slider(N)) %>% as_tibble()
colnames(ineq_items) <- paste0("ineq_item_", 1:4)

dat <- bind_cols(dat, ineq_items) %>%
  mutate(
    subjective_inequality_mean =
      rowMeans(select(., starts_with("ineq_item_")))
  )

############################################################
# Institutional trust (pre-treatment)
############################################################

institutions <- c(
  "epa", "nasa", "noaa", "universities",
  "federal_gov", "supreme_court", "congress"
)

inst_items <- map_dfc(
  institutions,
  ~ r_slider(N)
)

colnames(inst_items) <- paste0("trust_", institutions, "_pre")

dat <- bind_cols(dat, inst_items) %>%
  mutate(
    institutional_trust_pre =
      rowMeans(select(., starts_with("trust_") & ends_with("_pre")))
  )

############################################################
# Psychological distance to science (aggregated)
############################################################

psych_items <- replicate(16, r_slider(N)) %>% as_tibble()
colnames(psych_items) <- paste0("psych_item_", 1:16)

dat <- bind_cols(dat, psych_items) %>%
  mutate(
    psych_distance_mean =
      rowMeans(select(., starts_with("psych_item_")))
  )

############################################################
# Main outcome: Trust in climate scientists (post)
############################################################

# 3 items per dimension
trust_competence_items <- replicate(3, r_slider(N)) %>% as_tibble()
trust_integrity_items  <- replicate(3, r_slider(N)) %>% as_tibble()
trust_benevolence_items <- replicate(3, r_slider(N)) %>% as_tibble()
trust_openness_items <- replicate(3, r_slider(N)) %>% as_tibble()

colnames(trust_competence_items) <- paste0("trust_competence_", 1:3)
colnames(trust_integrity_items)  <- paste0("trust_integrity_", 1:3)
colnames(trust_benevolence_items) <- paste0("trust_benevolence_", 1:3)
colnames(trust_openness_items) <- paste0("trust_openness_", 1:3)

dat <- bind_cols(
  dat,
  trust_competence_items,
  trust_integrity_items,
  trust_benevolence_items,
  trust_openness_items
) %>%
  mutate(
    trust_competence  = rowMeans(trust_competence_items),
    trust_integrity   = rowMeans(trust_integrity_items),
    trust_benevolence = rowMeans(trust_benevolence_items),
    trust_openness    = rowMeans(trust_openness_items),
    trust_multidimensional = rowMeans(cbind(
      trust_competence,
      trust_integrity,
      trust_benevolence,
      trust_openness
    ))
  )

############################################################
# Secondary outcomes
############################################################

dat <- dat %>%
  mutate(
    donation = pmin(pmax(rnorm(N, 5, 3), 0), 10),
    
    behavior_mean = rowMeans(cbind(
      r_slider(N),
      r_slider(N),
      r_slider(N),
      r_slider(N),
      r_slider(N),
      r_slider(N)
    )),
    
    scientists_policy_role = r_slider(N),
    rely_on_scientists = r_slider(N),
    
    trust_scientists_single_post = r_slider(N),
    distrust_scientists_single_post = r_slider(N),
    
    general_climate_policy_support = r_slider(N),
    specific_climate_policy_support =
      rowMeans(replicate(7, r_slider(N))),
    
    climate_belief_post = r_slider(N),
    
    climate_concern =
      rowMeans(replicate(3, r_slider(N))),
    
    institutional_trust_post =
      rowMeans(replicate(7, r_slider(N))),
    
    science_personal_benefit = r_slider(N)
  )

############################################################
# Attrition
############################################################

# simulate some attrition for outcome variables
outcomes <- c(
  names(trust_competence_items), names(trust_integrity_items),
  names(trust_benevolence_items), names(trust_openness_items),
  "trust_competence", "trust_integrity", "trust_benevolence", "trust_openness",
  "trust_multidimensional",
  "donation"
)


dat <- dat %>%
  group_by(condition) %>%
  mutate(
    # randomly select 10% of rows per condition
    attrition_flag = if_else(row_number() %in% sample(n(), size = ceiling(0.1 * n())), TRUE, FALSE)
  ) %>%
  mutate(
    across(all_of(outcomes), ~ if_else(attrition_flag, NA_real_, .x))
  ) %>%
  select(-attrition_flag) %>%
  ungroup()

############################################################
# Final dataset
############################################################

write_csv(dat, "data/simulation/sample_preregistration.csv")







