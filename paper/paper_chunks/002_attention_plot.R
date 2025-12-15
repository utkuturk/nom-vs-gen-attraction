att_file <- "../data/attention/turkish_bert_attention_verb_to_nouns.csv"
if (file.exists(att_file)) {
    df_att <- read.csv(att_file)

    stats_att <- df_att %>%
        group_by(case, number) %>%
        summarise(
            mean = mean(attention_diff),
            sem = sd(attention_diff) / sqrt(n()),
            .groups = 'drop'
        )

    dodge_width <- 0.8
    y_max <- max(stats_att$mean + stats_att$sem)
    y_min <- min(stats_att$mean - stats_att$sem)
    y_limit_upper <- y_max + 0.015
    y_limit_lower <- y_min - 0.015

    ggplot(stats_att, aes(x = case, y = mean, group = number, shape = number)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_errorbar(
            aes(ymin = mean - sem, ymax = mean + sem),
            width = 0.2,
            position = position_dodge(dodge_width),
            linewidth = 0.5
        ) +
        geom_point(
            size = 4,
            position = position_dodge(dodge_width)
        ) +
        scale_x_discrete(
            labels = c("Genitive", "Nominative")
        ) +
        labs(
            x = NULL,
            y = "Attention Difference\n(Attractor - Target)",
            shape = "Number"
        ) +
        theme_classic() +
        theme(
            legend.position = "top"
        )
}
