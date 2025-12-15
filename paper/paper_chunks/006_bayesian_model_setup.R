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

# Define variables for reporting
est_attraction <- 0
prob_attraction <- 0
est_interaction <- 0
prob_interaction <- 0
ci_att_lower <- 0
ci_att_upper <- 0
ci_int_lower <- 0
ci_int_upper <- 0

# Load existing model
if (file.exists("../data/models/int.model.rds")) {
    fit <- readRDS("../data/models/int.model.rds")

    # Extract estimates
    summ <- posterior_summary(fit, pars = "^b_") %>%
        as.data.frame() %>%
        rownames_to_column("Term")

    # Attraction effect (b_Condition2Attraction)
    att_row <- summ %>% filter(Term == "b_Condition2Attraction")
    est_attraction <- round(att_row$Estimate, 2)
    ci_att_lower <- round(att_row$Q2.5, 2)
    ci_att_upper <- round(att_row$Q97.5, 2)

    draws <- as_draws_df(fit)
    prob_attraction <- round(mean(draws$b_Condition2Attraction > 0), 2)

    # Interaction effect (b_Condition1Case:Condition2Attraction) - assumes Case contrast matches sum coding in script
    # Need to verify term name from model or script. Script uses `Condition1Case:Condition2Attraction` or similar.
    # Checking previous script content: `Term == "b_Condition1Case:Condition2Attraction"`

    int_row <- summ %>% filter(Term == "b_Condition1Case:Condition2Attraction")
    if (nrow(int_row) > 0) {
        est_interaction <- round(int_row$Estimate, 2)
        ci_int_lower <- round(int_row$Q2.5, 2)
        ci_int_upper <- round(int_row$Q97.5, 2)
        prob_interaction <- round(
            mean(draws$`b_Condition1Case:Condition2Attraction` > 0),
            2
        )
    }
}
