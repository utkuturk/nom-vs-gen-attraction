if (nrow(clean_data) > 0) {
    desc_stats <- clean_data %>%
        group_by(Part_ID, Condition1, Condition2) %>%
        summarise(meanSubj = mean(Response_Binary)) %>%
        group_by(Condition1, Condition2) %>%
        summarise(
            Mean = mean(meanSubj) * 100,
            SE = (sd(meanSubj) / sqrt(n())) * 100,
            .groups = "drop"
        ) %>%
        mutate(
            Condition1 = case_when(
                Condition1 == "nom" ~ "Nominative",
                Condition1 == "gen" ~ "Genitive"
            )
        )

    # Extract specific means for reporting
    mean_gen_att <- desc_stats %>%
        filter(Condition1 == "Genitive", Condition2 == "attractorMatch") %>%
        pull(Mean) %>%
        round(1)
    mean_nom_att <- desc_stats %>%
        filter(Condition1 == "Nominative", Condition2 == "attractorMatch") %>%
        pull(Mean) %>%
        round(1)
    mean_gen_no <- desc_stats %>%
        filter(Condition1 == "Genitive", Condition2 == "noMatch") %>%
        pull(Mean) %>%
        round(1)
    mean_nom_no <- desc_stats %>%
        filter(Condition1 == "Nominative", Condition2 == "noMatch") %>%
        pull(Mean) %>%
        round(1)
} else {
    mean_gen_att <- 0
    mean_nom_att <- 0
    mean_gen_no <- 0
    mean_nom_no <- 0
}
