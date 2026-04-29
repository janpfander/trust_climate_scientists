# attrition simulation

library(tidyverse)
library(broom)
set.seed(123)


generate_sample <- function(seed = NULL,
                            N_per_condition = 1000,
                            N_interventions = 20,
                            N_control = 2) {
  
  if (!is.null(seed)) set.seed(seed)
  
  N_conditions <- N_interventions + N_control
  N <- N_per_condition * N_conditions
  
  conditions <- c(
    paste0("intervention_", 1:N_interventions),
    "control_main",
    "control_interactive"
  )
  
  r_slider <- function(n) pmin(pmax(rnorm(n, 50, 30), 0), 100)
  r_cat <- function(n, levels)
    factor(sample(levels, n, replace = TRUE), levels = levels)
  
  dat <- tibble(
    id = 1:N,
    condition = sample(conditions, N, replace = TRUE)
  )
  
  # --- demographics ---
  dat <- dat %>%
    mutate(
      gender = r_cat(N, c("Male", "Female", "Other")),
      age = round(pmin(pmax(rnorm(N, 45, 18), 18), 90)),
      education = r_cat(N, c("Low", "Mid", "High")),
      religion = r_cat(N, c("None", "Christian", "Other")),
      religiosity = r_slider(N),
      income = r_cat(N, c("Low", "Mid", "High")),
      urban_rural = r_cat(N, c("Urban", "Rural")),
      race = r_cat(N, c("White", "Black", "Other"))
    )
  
  # --- main outcome ---
  dat <- dat %>%
    mutate(
      trust_multidimensional = r_slider(N),
      donation = pmin(pmax(rnorm(N, 5, 3), 0), 10)
    )
  
  # --- attrition (equal across arms) ---
  dat <- dat %>%
    group_by(condition) %>%
    mutate(
      attrition_flag =
        row_number() %in%
        sample(n(), size = ceiling(0.1 * n()))
    ) %>%
    mutate(
      across(c(trust_multidimensional, donation),
             ~ if_else(attrition_flag, NA_real_, .x))
    ) %>%
    ungroup() %>%
    select(-attrition_flag)
  
  dat
}

# test 
test <- generate_sample(N_per_condition = 1000,
                            N_interventions = 20,
                            N_control = 2) 

run_attrition_tests <- function(dat,
                                outcome = "trust_multidimensional",
                                covariates,
                                alpha = 0.05) {
  
  dat <- dat %>%
    mutate(completed = !is.na(.data[[outcome]]))
  
  # 1️⃣ overall attrition difference
  overall_fit <- glm(completed ~ condition,
                     data = dat,
                     family = binomial)
  
  overall_p <- car::Anova(overall_fit, type = 3)$`Pr(>Chisq)`[1]
  
  # 2️⃣ interaction tests
  interaction_p <- covariates %>%
    map_dbl(function(cov) {
      fit <- glm(
        as.formula(paste("completed ~ condition *", cov)),
        data = dat,
        family = binomial
      )
      car::Anova(fit, type = 3)$`Pr(>Chisq)`[grep(":", rownames(car::Anova(fit, type = 3)))]
    })
  
  # multiple testing correction
  interaction_p_adj <- p.adjust(interaction_p, method = "holm")
  
  tibble(
    overall_sig = overall_p < alpha,
    any_interaction_sig =
      any(interaction_p_adj < alpha)
  )
}

# test
run_attrition_tests(dat = test,
                    outcome = "trust_multidimensional",
                    covariates = c("gender", "age", "religion", "age"),
                    alpha = 0.05) 


simulate_false_positive <- function(n_sims = 500,
                                    covariates,
                                    alpha = 0.05) {
  
  map_df(1:n_sims, function(i) {
    
    print(paste0("iteration number: ", i))
    
    dat <- generate_sample(seed = i)
    
    res <- run_attrition_tests(
      dat,
      outcome = "trust_multidimensional",
      covariates = covariates,
      alpha = alpha
    )
    
    res
  }) %>%
    summarise(
      fpr_overall =
        mean(overall_sig),
      fpr_interactions =
        mean(any_interaction_sig)
    )
}

# test
test_sensitivity <- simulate_false_positive(n_sims = 20,
                        c("gender", "age", "religion", "age"),
                        alpha = 0.05) 



