# power simulation

# load packages
library(tidyverse)
library(broom)


# read pilot data
pilot_data <- read_csv("data/pilot/cleaned.csv") |> 
  filter(condition == "control")

# pilot_data must contain a variable called `trust`
trust_pilot <- pilot_data$trust

pilot_sd   <- sd(trust_pilot, na.rm = TRUE)
pilot_mean <- mean(trust_pilot, na.rm = TRUE)


simulate_one_sample <- function(sim_id,
                                effect_size,
                                n_per_arm,
                                trust_pilot,
                                pilot_sd,
                                alpha = 0.05) {
  
  # Generate design
  dat <- tibble(
    sim = sim_id,
    arm = c(
      rep(0, n_per_arm * 2),        # control: twice as large
      rep(1:20, each = n_per_arm)   # 20 interventions
  )
  ) %>%
    mutate(
      arm = factor(arm),
      
      # resample from empirical pilot distribution
      y_base = sample(trust_pilot, size = n(), replace = TRUE),
      
      # compute sample-specific SD (per simulated sample, before adding effect)
      sample_sd = sd(y_base),
      
      # calculate delta in raw points
      delta_global = effect_size * pilot_sd,
      delta_sample = effect_size * sample_sd,
      
      # apply treatment effect
      true_mean = if_else(arm == 0, 0, delta_sample),
      y = pmin(pmax(y_base + true_mean, 0), 100)
    )
  
  # Fit model
  fit <- lm(y ~ arm, data = dat)
  
  # Robust variance-covariance matrix
  vcov_robust <- sandwich::vcovHC(fit, type = "HC2")
  
  # Extract results with robust SEs
  lmtest::coeftest(fit, vcov = vcov_robust) %>%
    broom::tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(
      significant = p.value < alpha,
      adjusted_p.value = p.adjust(p.value, method = "BH"),
      adjusted_significant = adjusted_p.value < alpha,
      sim_id = sim_id,
      effect_size = effect_size,
      arm = as.integer(str_remove(term, "arm")),
      delta_global = dat$delta_global[1],
      delta_sample = dat$delta_sample[1],
      sample_sd = dat$sample_sd[1]
    )
}


calculate_power <- function(n_sims, ...) {
  
  power_data <- 1:n_sims %>%
    purrr::map_df(function(x) {
      
      results <- simulate_one_sample(sim_id = x, ...)
      
      if (x %% 50 == 0) {
        cat(format(Sys.time(), "%H:%M:%S"), "— iteration", x, "\n")
      }
      
      results
    })
  
  power_data %>%
    group_by(sim_id) %>%
    summarize(
      power_within_sample = mean(significant),
      power_within_sample_adjusted = mean(adjusted_significant),
      .groups = "drop"
    ) %>%
    summarize(
      power = mean(power_within_sample),
      power_adjusted = mean(power_within_sample_adjusted),
      n_sims = n_sims
    )
}

simulate <- function(file_name, sample_sizes, effect_sizes, ...) {
  
  if (!file.exists(file_name)) {
    
    design_grid <- expand_grid(
      n_per_arm = sample_sizes,
      effect_size = effect_sizes
    )
    
    power_results <- purrr::map_df(
      seq_len(nrow(design_grid)),
      function(i) {
        
        current <- design_grid[i, ]
        
        print(
          paste(
            "tested sample size =", current$n_per_arm,
            "| effect size =", current$effect_size
          )
        )
        
        result <- calculate_power(
          n_per_arm = current$n_per_arm,
          effect_size = current$effect_size,
          ...
        )
        
        result$n_per_arm   <- current$n_per_arm
        result$effect_size <- current$effect_size
        
        result
      }
    )
    
    write_csv(power_results, file_name)
  }
}

system.time({
  simulate(
    file_name    = "data/simulation/power_simulation.csv",
    sample_sizes = c(400, 600, 800, 1000),
    effect_sizes = c(0, 0.10, 0.15, 0.2),
    n_sims       = 1000,
    trust_pilot  = trust_pilot,
    pilot_sd     = pilot_sd
  )
})

# activate amphetamine (the program)
# then run this from terminal to have it run in the background
# in my case:
# cd ~/Desktop/trust_climate_scientists
# Rscript R/power_simulation.R










