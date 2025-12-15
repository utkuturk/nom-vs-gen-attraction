if (nrow(clean_data) > 0) {
    desc_stats$Condition2 <- factor(
        desc_stats$Condition2,
        levels = c("targetMatch", "attractorMatch", "noMatch")
    )

    ggplot(
        desc_stats,
        aes(
            x = Condition1,
            y = Mean,
            color = Condition2,
            group = Condition2,
            shape = Condition2
        )
    ) +
        geom_errorbar(
            aes(ymin = Mean - SE, ymax = Mean + SE),
            width = 0.2,
            linewidth = 1,
            position = position_dodge(0.5)
        ) +
        geom_point(
            size = 2.5,
            position = position_dodge(0.5)
        ) +
        scale_color_brewer(palette = "Dark2") +
        scale_y_continuous(
            breaks = seq(10, 90, 20),
            labels = function(x) paste0(x, "%")
        ) +
        labs(
            y = "Acceptance Rate ('Yes')",
            color = "Condition",
            shape = "Condition",
            x = NULL
        ) +
        theme(
            legend.position = "top"
        )
}
