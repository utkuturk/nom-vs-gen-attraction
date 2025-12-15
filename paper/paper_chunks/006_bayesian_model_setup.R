if (nrow(clean_data) > 0) {
    clean_data$Condition2 <- factor(
        clean_data$Condition2,
        levels = c("targetMatch", "attractorMatch", "noMatch")
    )

    c_mat <- matrix(
        c(2, -1, -1, 0, 1, -1),
        ncol = 2
    )

    colnames(c_mat) <- c("Grammaticality", "Attraction")
    contrasts(clean_data$Condition2) <- c_mat
    contrasts(clean_data$Condition1) <- contr.sum(2)
    colnames(contrasts(clean_data$Condition1)) <- c("Case")
}

# Capture random seed for replicability
run_seed <- randnum
# seed is used in brm call below

# Define variables for reporting
est_attraction <- 0
prob_attraction <- 0
est_interaction <- 0
prob_interaction <- 0
ci_att_lower <- 0
ci_att_upper <- 0
ci_int_lower <- 0
ci_int_upper <- 0

# Define Model Formula and Priors
bf_mod <- bf(
  Response_Binary ~ Condition1 * Condition2 +
    (1 + Condition1 * Condition2 | Part_ID) +
    (1 + Condition1 * Condition2 | Item_Set)
)

priors <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor")
)

# Fit or Load Model
model_file <- "../data/models/int.model.rds"
if (file.exists(model_file)) {
    fit <- readRDS(model_file)
} else {
    fit <- brm(
      formula = bf_mod,
      data = clean_data,
      family = bernoulli(link = "logit"),
      prior = priors,
      chains = 4,
      iter = 2000,
      warmup = 1000,
      cores = 4,
      seed = run_seed,
      file = model_file
    )
}

# Define variables for reporting
est_attraction <- 0
prob_attraction <- 0
est_interaction <- 0
prob_interaction <- 0
ci_att_lower <- 0
ci_att_upper <- 0
ci_int_lower <- 0
ci_int_upper <- 0

if (exists("fit")) {
    # Extract estimates
    summ <- posterior_summary(fit, pars = "^b_") %>%
        as.data.frame() %>%
        rownames_to_column("Term")

    # Attraction effect (b_Condition2Attraction)
    att_row <- summ %>% filter(Term == "b_Condition2Attraction")
    if(nrow(att_row) > 0){
        est_attraction <- round(att_row$Estimate, 2)
        ci_att_lower <- round(att_row$Q2.5, 2)
        ci_att_upper <- round(att_row$Q97.5, 2)
    }

    draws <- as_draws_df(fit)
    if("b_Condition2Attraction" %in% names(draws)){
         prob_attraction <- round(mean(draws$b_Condition2Attraction > 0), 2)
    }

    # Interaction effect
    int_row <- summ %>% filter(Term == "b_Condition1Case:Condition2Attraction")
    if (nrow(int_row) > 0) {
        est_interaction <- round(int_row$Estimate, 2)
        ci_int_lower <- round(int_row$Q2.5, 2)
        ci_int_upper <- round(int_row$Q97.5, 2)
        
        if("b_Condition1Case:Condition2Attraction" %in% names(draws)){
             prob_interaction <- round(
                mean(draws$`b_Condition1Case:Condition2Attraction` > 0),
                2
             )
        }
    }
}
