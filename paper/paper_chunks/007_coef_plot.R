if (exists("fit")) {
    post_summary <- posterior_summary(fit, pars = "^b_") %>%
        as.data.frame() %>%
        rownames_to_column("Term") %>%
        filter(Term != "b_Intercept")

    draws_df <- as_draws_df(fit) %>%
        as_tibble() %>%
        select(starts_with("b_"))

    p_greater_than_zero <- draws_df %>%
        pivot_longer(
            cols = starts_with("b_"),
            names_to = "Term",
            values_to = "Value"
        ) %>%
        filter(Term != "b_Intercept") %>%
        group_by(Term) %>%
        summarise(
            P = mean(Value > 0)
        ) %>%
        ungroup()

    post_summary_final <- post_summary %>%
        left_join(p_greater_than_zero, by = "Term")

    post_summary_final %<>%
        mutate(
            Term = case_when(
                Term == "b_Condition1Case" ~ "Case",
                Term == "b_Condition2Grammaticality" ~ "Grammaticality",
                Term == "b_Condition2Attraction" ~ "Attraction",
                Term ==
                    "b_Condition1Case:Condition2Grammaticality" ~ "Case X \n Grammaticality",
                Term ==
                    "b_Condition1Case:Condition2Attraction" ~ "Case X \n Attraction",
            )
        )

    post_summary_final$Term <- factor(
        post_summary_final$Term,
        levels = rev(c(
            "Case",
            "Grammaticality",
            "Attraction",
            "Case X \n Grammaticality",
            "Case X \n Attraction"
        ))
    )

    ggplot(post_summary_final, aes(y = Term, x = Estimate)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
        geom_point(size = 3) +
        geom_text(
            aes(x = 1.2, label = paste0("=", round(P, 2))),
            hjust = 0,
            size = 3.5,
            color = "black"
        ) +
        labs(x = "Estimate (Log Odds)", y = "") +
        coord_cartesian(xlim = c(-0.5, 1.5), clip = "off") +
        theme_minimal()
}
