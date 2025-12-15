library(knitr)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)
options(brms.backend = "cmdstanr")
library(posterior)
library(glue)
library(magrittr)
library(dplyr)
library(ggplot2)

## Load and Clean Data

file_path <- "Data59participants.csv"
raw_lines <- readLines(file_path)
data_lines <- raw_lines[!grepl("^#", raw_lines)]
con <- textConnection(data_lines)
data <- read.csv(
  con,
  header = FALSE,
  fill = TRUE,
  col.names = paste0("V", 1:30),
  stringsAsFactors = FALSE
)
close(con)

### accuracy in fillers
filler_data <- data %>%
  filter(V16 == "filler") %>%
  select(
    Part_ID = V13,
    Item_Set = V14,
    Trial_Order = V15,
    Condition1 = V17,
    Condition2 = V18,
    Correct_Resp = V19,
    Participant_Response = V20,
    RT = V21
  ) %>%
  distinct(Part_ID, Item_Set, Trial_Order, .keep_all = TRUE) %>%
  mutate(
    RT = as.numeric(as.character(RT)),
    Response_Binary = ifelse(Participant_Response == "Yes", 1, 0),
    Is_Correct = ifelse(Participant_Response == Correct_Resp, 1, 0),
    Condition1 = as.factor(Condition1),
    Condition2 = as.factor(Condition2)
  )

subj_accuracy <- filler_data %>%
  group_by(Part_ID) %>%
  summarize(meanAcc = mean(Is_Correct))

### experimental trials
clean_data <- data %>%
  filter(V16 == "experimental") %>%
  select(
    Part_ID = V13,
    Item_Set = V14,
    Trial_Order = V15,
    Condition1 = V17,
    Condition2 = V18,
    Correct_Resp = V19,
    Participant_Response = V20,
    RT = V21
  ) %>%
  distinct(Part_ID, Item_Set, Trial_Order, .keep_all = TRUE) %>%
  mutate(
    RT = as.numeric(as.character(RT)),
    Response_Binary = ifelse(Participant_Response == "Yes", 1, 0),
    Is_Correct = ifelse(Participant_Response == Correct_Resp, 1, 0),
    Condition1 = as.factor(Condition1),
    Condition2 = as.factor(Condition2)
  )
table(clean_data$Condition1, clean_data$Condition2)

length(unique(clean_data$Part_ID)) #59

# removing bad participants

#clean_data %<>% filter(!Part_ID %in% c("0V7zN7w0lRatEtx", "2mFGPZNrdQiXzTB", "32HAsoV5nuu38SW"))
#length(unique(clean_data$Part_ID)) #56

## Descriptive Plots (Dots and Error Bars)

desc_stats <- clean_data %>%
  group_by(Part_ID, Condition1, Condition2) %>%
  summarise(meanSubj = mean(Response_Binary)) %>%
  group_by(Condition1, Condition2) %>%
  summarise(Mean = mean(meanSubj) *100,
            SE =(sd(meanSubj) / sqrt(n())) *100,
            .groups = "drop") %>%
  mutate(Condition1 = case_when(
    Condition1 == "nom" ~ "Nominative",
    Condition1 == "gen" ~ "Genitive"
    ))

desc_stats$Condition2 <- factor(desc_stats$Condition2, levels = c("targetMatch", "attractorMatch", "noMatch"))

dodge_width <- 0.5
y_limit_lower <- 10
y_limit_upper <- 90
right_margin_size <- 5.0 # Retaining this variable for the custom margin

descriptive <- ggplot(
  desc_stats,
  aes(x = Condition1, y = Mean, color = Condition2, group = Condition2, shape = Condition2)
) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.2,
    linewidth = 1,
    position = position_dodge(dodge_width)
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(dodge_width)
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(
    limits = c(y_limit_lower, y_limit_upper),
    breaks = seq(10, 90, 20),
    labels = function(x) paste0(x, "%") # Adds % to Y-axis
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Response 'Yes'",
    color = NULL,
    shape = NULL
  ) +
  theme_classic() + 
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "gray90"), 
    panel.grid.major.x = element_blank(),                
    panel.grid.minor = element_blank(),                  
    
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 18, margin = margin(r = 10, unit = "pt")),
    plot.margin = unit(c(0.5, right_margin_size, 0.5, 0.5), "lines")
  ) +
  coord_cartesian(clip = "off")

ggsave("descriptive.png", plot = descriptive, width = 7, height = 4)

## Bayesian Analysis

### Contrast Setup

clean_data$Condition2 <- factor(
  clean_data$Condition2,
  levels = c("targetMatch", "attractorMatch", "noMatch"))

c_mat <- matrix(
  c(2, -1, -1, 0, 1, -1),
  ncol = 2)

colnames(c_mat) <- c("Grammaticality", "Attraction")
contrasts(clean_data$Condition2) <- c_mat
contrasts(clean_data$Condition2)

contrasts(clean_data$Condition1) <- contr.sum(2)
colnames(contrasts(clean_data$Condition1)) <- c("Case")
contrasts(clean_data$Condition1)


### Model Fitting
bf_mod <- bf(
  Response_Binary ~ Condition1 *
    Condition2 +
    (1 + Condition1 * Condition2 | Part_ID) +
    (1 + Condition1 * Condition2 | Item_Set))

priors <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"))

fit <- brm(
  formula = bf_mod,
  data = clean_data,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  file = "int.model"
  )
pp_check(fit, ndraws = 100)


bf_nested <- bf(
  Response_Binary ~ Condition1 + Condition1/Condition2 +
    (1 + Condition1 + Condition1/Condition2 | Part_ID) +
    (1 + Condition1 + Condition1/Condition2 | Item_Set))


fit_nested <- brm(
  formula = bf_nested,
  data = clean_data,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  file = "nested.model")

pp_check(fit_nested, ndraws = 100)


## plotting

fixef(fit)

draws <- as_draws_df(fit)

cond_effects <- conditional_effects(fit, effects = "Condition1:Condition2")
plot(cond_effects, points = TRUE)

## Coefficient Plot (interaction model)
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
  mutate(Term = case_when(
   Term == "b_Condition1Case" ~ "Case",
   Term == "b_Condition2Grammaticality" ~ "Grammaticality",
   Term == "b_Condition2Attraction" ~ "Attraction",
   Term == "b_Condition1Case:Condition2Grammaticality" ~ "Case X \n Grammaticality",
   Term == "b_Condition1Case:Condition2Attraction" ~ "Case X \n Attraction",
  )
  )

post_summary_final$Term <- factor(post_summary_final$Term, levels = rev(c("Case", "Grammaticality", 
                                                          "Attraction",
                                                          "Case X \n Grammaticality",
                                                          "Case X \n Attraction")))

int.plot <- ggplot(post_summary_final, aes(y = Term, x = Estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  geom_point(size = 3) +
  geom_text(
    aes(x = 1.2, label = paste0("=", round(P,2))),
    hjust = 0,
    size = 5,
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.2,
    y = length(unique(post_summary_final$Term)) + 0.6,
    label = "P(beta > 0)",
    parse = TRUE,
    fontface = "bold",
    hjust = 0,
    size = 6
  ) +
  coord_cartesian(xlim = c(-0.5, 1), clip = "off") +
  labs(
    title = "",
    x = "Estimate (Log Odds)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    plot.margin = margin(t = 10, r = 80, b = 10, l = 10, unit = "pt"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 1),
    strip.text.y = element_text(angle = 270),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y.left = element_text(hjust = 1),
    axis.ticks.y.left = element_blank()
  )

ggsave("intPlot.png", plot = int.plot, width = 7, height = 4)


## Coefficient Plot (nested model)

post_summary_nested <- posterior_summary(fit_nested, pars = "^b_") %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  filter(Term != "b_Intercept")

draws_df_nested <- as_draws_df(fit_nested) %>%
  as_tibble() %>%
  select(starts_with("b_"))

p_greater_than_zero_nested <- draws_df_nested %>%
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

post_summary_final_nested <- post_summary_nested %>%
  left_join(p_greater_than_zero_nested, by = "Term")

post_summary_final_nested %<>%
  mutate(Term = case_when(
    Term == "b_Condition1Case" ~ "Case",
    Term == "b_Condition1gen:Condition2Grammaticality" ~ "GEN: \n Grammaticality",
    Term == "b_Condition1nom:Condition2Grammaticality" ~ "NOM: \n Grammaticality",
    Term == "b_Condition1gen:Condition2Attraction" ~ "GEN: \n Attraction",
    Term == "b_Condition1nom:Condition2Attraction" ~ "NOM: \n Attraction",
  )
  )

post_summary_final_nested$Term <- factor(post_summary_final_nested$Term, levels = rev(c("Case", 
                                                                                        "GEN: \n Grammaticality", 
                                                                                        "NOM: \n Grammaticality",
                                                                                        "GEN: \n Attraction",
                                                                                        "NOM: \n Attraction")))

nested.plot <- ggplot(post_summary_final_nested, aes(y = Term, x = Estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  geom_point(size = 3) +
  geom_text(
    aes(x = 1.6, label = paste0("=", round(P,2))),
    hjust = 0,
    size = 5,
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.6,
    y = length(unique(post_summary_final$Term)) + 0.6,
    label = "P(beta > 0)",
    parse = TRUE,
    fontface = "bold",
    hjust = 0,
    size = 6
  ) +
  coord_cartesian(xlim = c(-0.5, 1.5), clip = "off") +
  labs(
    title = "",
    x = "Estimate (Log Odds)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    plot.margin = margin(t = 10, r = 80, b = 10, l = 10, unit = "pt"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 1),
    strip.text.y = element_text(angle = 270),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y.left = element_text(hjust = 1),
    axis.ticks.y.left = element_blank()
  )

## combined plot
post_summary_final %<>% mutate(model = "Crossed")
post_summary_final_nested %<>% mutate(model = "Nested")
post_summary_combined <- rbind(post_summary_final, post_summary_final_nested)

text_x_position <- 0.7 
x_axis_min <- -1
x_axis_max <- 3

get_p <- function(p) {
  label = ifelse(p==0, "<0.01", paste0("=",round(p,2)))
  label
}

ggplot(post_summary_combined, aes(y = Term, x = Estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  geom_point(size = 3) +
  geom_text(
    aes(
      x = 2,
      label =get_p(P)
    ),
    hjust = 0,
    size = 3.5,
    color = "black"
  ) +
  coord_cartesian(xlim = c(x_axis_min, x_axis_max)) +
  labs(
    title = "",
    x = "Estimate (Log Odds)",
    y = ""
  ) +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  facet_wrap(
    model~.,
    scales = "free_y",
    nrow = 2,
    strip.position = "right" # Keeps the strips on the left
  ) +
  geom_text(
    data = NULL,
    aes(x = 2, y = 5.4, label = "P(>0)"),
    hjust = 0,
    size = 3.5,
    color = "black"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    strip.background = element_rect(
      fill = "gray90",      # Light gray fill color for the box
      color = "black",       # Black border around the box
      linewidth = 1          # Thickness of the border
    ),
    strip.text.y = element_text(angle = 270), 
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y.left = element_text(hjust = 1),
    axis.ticks.y.left = element_blank()
  )





