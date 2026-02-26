# =============================================================================
# Bayesian Power Analysis for Agreement Attraction (Accuracy / response_yes)
# =============================================================================
# This script follows the previous brms-style workflow:
#   1) Estimate pilot-aligned effect centers from existing data
#   2) Define explicit priors (means + SDs)
#   3) Simulate new datasets from those priors (prior predictive)
#   4) Fit/update a brms Bernoulli model
#   5) Compute power as P(95% CrI excludes 0)
#
# Design simulated:
#   - Case: gen vs nom
#   - Match: noMatch vs targetMatch vs attractorMatch
#   - Position: subject vs object
#
# Attraction is the attractorMatch - noMatch contrast on response_yes.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(brms)
  library(cmdstanr)
  library(posterior)
})

set.seed(26022026)
options(contrasts = c("contr.treatment", "contr.poly"))
options(brms.backend = "cmdstanr")

# ------------------------- Assumptions / Priors -------------------------------
# NOTE: These are explicit Bayesian priors used by brms (plus simulation assumptions).
#
# Meanings:
#   - prior means for fixed effects are centered on pilot estimates from
#     data/experiment/Data59participants.csv (or fallback cell means).
#   - attraction is defined as: AttractorMatch - NoMatch (Yes-response scale).
#   - object_attraction_scale = 0.60 means attraction in object position is 40%
#     smaller than in subject position (on log-odds scale).
#   - implementation:
#       b_match_pos = (scale - 1) * b_match_attr
#       b_threeway = (scale - 1) * b_case_nom_match
#     therefore:
#       object_gen_attraction = scale * subject_gen_attraction
#   - target/noMatch are assumed unchanged across Subject Gen vs Object Gen:
#       b_pos_obj = 0, b_match_target_pos = 0 (by default assumptions below).
#   - power contrasts in this script focus on object GEN attraction only.
#   - prior_sd_fixed controls uncertainty for each fixed-effect coefficient.
#   - prior_sd_intercept controls uncertainty for the baseline log-odds.
#   - prior_sd_sd_subject/item control uncertainty for RE SD priors.
# Current implied Yes-rates (from pilot-centered means, with scale 0.60):

# Subject Gen (baseline reference):
#   noMatch = 25.68%
#   targetMatch = 74.90%
#   attractorMatch = 32.64%
#   attraction effect (AttractorMatch - NoMatch) = +6.96 percentage points
# Object Gen (assumed smaller attraction):
#   noMatch = 25.68%
#   targetMatch = 74.90%
#   attractorMatch = 29.73%
#   attraction effect (AttractorMatch - NoMatch) = +4.06 percentage points

assumptions <- list(
  baseline_cell = "case=gen, match=targetMatch, position=subject",
  object_attraction_scale = 0.60,
  object_attraction_scale_grid = c(0.40, 0.50, 0.60, 0.70, 0.80),
  object_main_effect_logodds = 0.00,
  case_by_position_logodds = 0.00,
  prior_sd_intercept = 0.50,
  prior_sd_fixed = 0.30,
  prior_sd_sd_subject = 0.20,
  prior_sd_sd_item = 0.10,
  fallback_sd_subject = 0.70,
  fallback_sd_item = 0.35,
  alpha = 0.05
)

# ---------------------------- Runtime config ----------------------------------
config <- list(
  sample_sizes = c(80, 120, 160, 200),
  n_simulations = 1000,
  n_item_sets = 24,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  cores = 4,
  threads = 8,
  alpha = assumptions$alpha,
  pilot_data_path = Sys.getenv("EXP2_PILOT_DATA", unset = ""),
  output_base_dir = Sys.getenv("EXP2_OUTPUT_DIR", unset = ""),
  output_csv = "scripts/power/exp2_power_results_accuracy_brms.csv",
  output_plot = "scripts/power/exp2_power_curve_accuracy_brms.png",
  output_assumptions_txt = "scripts/power/exp2_power_assumptions_report.txt",
  output_pilot_accuracy_csv = "scripts/power/exp2_power_pilot_accuracy.csv",
  output_assumed_cells_csv = "scripts/power/exp2_power_assumed_cells.csv",
  output_attraction_effects_csv = "scripts/power/exp2_power_attraction_effects.csv",
  output_scale_sensitivity_csv = "scripts/power/exp2_power_scale_sensitivity.csv",
  output_prior_params_csv = "scripts/power/exp2_power_prior_parameters.csv",
  output_min_n_csv = "scripts/power/exp2_power_min_n_80.csv"
)

# ---------------------------- Helpers -----------------------------------------
`%||%` <- function(x, y) {
  if (length(x) == 0 || is.na(x)) y else x
}

safe_prob <- function(x, eps = 1e-4) {
  pmin(pmax(x, eps), 1 - eps)
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[\\\\/])", path)
}

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    return(NA_character_)
  }

  normalizePath(
    sub("^--file=", "", file_arg[1]),
    winslash = "/",
    mustWork = FALSE
  )
}

infer_project_root <- function(script_path) {
  if (!is.na(script_path) && nzchar(script_path)) {
    root_from_script <- normalizePath(
      file.path(dirname(script_path), "..", ".."),
      winslash = "/",
      mustWork = FALSE
    )
    if (dir.exists(root_from_script)) {
      return(root_from_script)
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_data_path <- function(project_root, explicit_path = "", script_path = NA_character_) {
  explicit_candidate <- explicit_path
  if (nzchar(explicit_candidate) && !is_absolute_path(explicit_candidate)) {
    explicit_candidate <- file.path(project_root, explicit_candidate)
  }

  candidates <- c(
    explicit_candidate,
    file.path(project_root, "data", "experiment", "Data59participants.csv"),
    file.path(project_root, "..", "data", "experiment", "Data59participants.csv")
  )
  candidates <- unique(candidates[nzchar(candidates)])

  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(
      paste0(
        "Could not find Data59participants.csv.\n",
        "Tried these paths:\n - ",
        paste(candidates, collapse = "\n - "),
        "\nWorking directory: ",
        normalizePath(getwd(), winslash = "/", mustWork = FALSE),
        "\nScript path: ",
        ifelse(is.na(script_path), "<unknown>", script_path),
        "\nProject root used: ",
        project_root,
        "\nTip: set EXP2_PILOT_DATA to an absolute CSV path."
      )
    )
  }

  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

resolve_output_path <- function(path, project_root, output_base_dir = "") {
  out <- path
  if (!is_absolute_path(path)) {
    if (nzchar(output_base_dir)) {
      base_dir <- output_base_dir
      if (!is_absolute_path(base_dir)) {
        base_dir <- file.path(project_root, base_dir)
      }
      out <- file.path(base_dir, basename(path))
    } else {
      out <- file.path(project_root, path)
    }
  }

  out_dir <- dirname(out)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  normalizePath(out, winslash = "/", mustWork = FALSE)
}

print_assumptions <- function(assumptions, config) {
  cat("\n================ ASSUMPTIONS / PRIORS ================\n")
  cat("Model fitting:", "Bayesian GLMM via brms (Bernoulli-logit)\n")
  cat("Baseline cell:", assumptions$baseline_cell, "\n")
  cat("Attraction contrast:", "AttractorMatch - NoMatch\n")
  cat("Object attraction scale:", assumptions$object_attraction_scale, "\n")
  cat(
    "Object attraction scale grid (sensitivity):",
    paste(assumptions$object_attraction_scale_grid, collapse = ", "),
    "\n"
  )
  cat(
    "Object main effect (log-odds):",
    assumptions$object_main_effect_logodds,
    "\n"
  )
  cat("Assumed position effect on TargetMatch (log-odds): 0.00\n")
  cat("Assumed position effect on NoMatch (log-odds): 0.00\n")
  cat(
    "Case x Position effect (log-odds):",
    assumptions$case_by_position_logodds,
    "\n"
  )
  cat("Prior SD intercept:", assumptions$prior_sd_intercept, "\n")
  cat("Prior SD fixed effects:", assumptions$prior_sd_fixed, "\n")
  cat(
    "Prior SDs for RE-SD means (subject/item):",
    paste0(
      assumptions$prior_sd_sd_subject,
      " / ",
      assumptions$prior_sd_sd_item
    ),
    "\n"
  )
  cat(
    "Fallback RE SD centers (subject/item):",
    paste0(
      assumptions$fallback_sd_subject,
      " / ",
      assumptions$fallback_sd_item
    ),
    "\n"
  )
  cat("Alpha:", config$alpha, "\n")
  cat("Sample sizes:", paste(config$sample_sizes, collapse = ", "), "\n")
  cat("Simulations per N:", config$n_simulations, "\n")
  cat("Item sets:", config$n_item_sets, "\n")
  cat(
    "Pilot data override (EXP2_PILOT_DATA):",
    ifelse(nzchar(config$pilot_data_path), config$pilot_data_path, "<auto>"),
    "\n"
  )
  cat(
    "Output base override (EXP2_OUTPUT_DIR):",
    ifelse(nzchar(config$output_base_dir), config$output_base_dir, "<project root>"),
    "\n"
  )
  cat("Assumptions text export:", config$output_assumptions_txt, "\n")
  cat("Pilot accuracy export:", config$output_pilot_accuracy_csv, "\n")
  cat("Assumed cells export:", config$output_assumed_cells_csv, "\n")
  cat("Attraction effects export:", config$output_attraction_effects_csv, "\n")
  cat("Scale sensitivity export:", config$output_scale_sensitivity_csv, "\n")
  cat("Prior parameters export:", config$output_prior_params_csv, "\n")
  cat("Minimum-N export:", config$output_min_n_csv, "\n")
  cat(
    "MCMC: chains=",
    config$chains,
    ", iter=",
    config$iter,
    ", warmup=",
    config$warmup,
    ", threads=",
    config$threads,
    "\n",
    sep = ""
  )
  cat("=====================================================\n\n")
}

load_pilot_data <- function(data_path) {
  raw_lines <- readLines(data_path, warn = FALSE)
  data_lines <- raw_lines[!grepl("^#", raw_lines)]

  con <- textConnection(data_lines)
  on.exit(close(con), add = TRUE)

  raw <- read.csv(
    con,
    header = FALSE,
    fill = TRUE,
    col.names = paste0("V", 1:30),
    stringsAsFactors = FALSE
  )

  raw %>%
    filter(V16 == "experimental") %>%
    transmute(
      participant = as.character(V13),
      item_set = as.character(V14),
      trial_order = as.character(V15),
      case = as.character(V17),
      match = as.character(V18),
      participant_response = as.character(V20)
    ) %>%
    distinct(participant, item_set, trial_order, .keep_all = TRUE) %>%
    filter(match %in% c("noMatch", "targetMatch", "attractorMatch")) %>%
    mutate(
      response_yes = as.integer(participant_response == "Yes"),
      case = factor(case, levels = c("gen", "nom")),
      match = factor(
        match,
        levels = c("noMatch", "targetMatch", "attractorMatch")
      ),
      participant = factor(participant),
      item_set = factor(item_set)
    ) %>%
    select(participant, item_set, case, match, response_yes)
}

fallback_pilot_effects <- function(dat, assumptions) {
  means <- dat %>%
    group_by(case, match) %>%
    summarize(p_yes = mean(response_yes), .groups = "drop")

  get_cell <- function(case_level, match_level) {
    val <- means %>%
      filter(case == case_level, match == match_level) %>%
      pull(p_yes)
    if (length(val) == 0) NA_real_ else val[[1]]
  }

  p_gn <- safe_prob(get_cell("gen", "noMatch"))
  p_gt <- safe_prob(get_cell("gen", "targetMatch"))
  p_ga <- safe_prob(get_cell("gen", "attractorMatch"))
  p_nn <- safe_prob(get_cell("nom", "noMatch"))
  p_nt <- safe_prob(get_cell("nom", "targetMatch"))
  p_na <- safe_prob(get_cell("nom", "attractorMatch"))

  if (any(is.na(c(p_gn, p_gt, p_ga, p_nn, p_nt, p_na)))) {
    stop("Could not compute fallback pilot effects.")
  }

  b0 <- qlogis(p_gn)
  b_case_nom <- qlogis(p_nn) - qlogis(p_gn)
  b_match_target <- qlogis(p_gt) - qlogis(p_gn)
  b_match_attr <- qlogis(p_ga) - qlogis(p_gn)
  b_case_nom_target <- (qlogis(p_nt) - qlogis(p_nn)) - b_match_target
  b_case_nom_match <- (qlogis(p_na) - qlogis(p_nn)) - b_match_attr

  list(
    intercept = b0,
    b_case_nom = b_case_nom,
    b_match_target = b_match_target,
    b_match_attr = b_match_attr,
    b_case_nom_target = b_case_nom_target,
    b_case_nom_match = b_case_nom_match,
    sd_subject = assumptions$fallback_sd_subject,
    sd_item = assumptions$fallback_sd_item,
    source = "cell_means_fallback"
  )
}

estimate_pilot_effects <- function(dat, assumptions) {
  fit <- tryCatch(
    suppressWarnings(
      glmer(
        response_yes ~ case * match + (1 | participant) + (1 | item_set),
        data = dat,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
      )
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    message("Pilot GLMM failed; using fallback from cell means.")
    return(fallback_pilot_effects(dat, assumptions))
  }

  fe <- fixef(fit)
  vc <- as.data.frame(VarCorr(fit))
  get_fe <- function(name) if (name %in% names(fe)) unname(fe[[name]]) else 0

  list(
    intercept = get_fe("(Intercept)"),
    b_case_nom = get_fe("casenom"),
    b_match_target = get_fe("matchtargetMatch"),
    b_match_attr = get_fe("matchattractorMatch"),
    b_case_nom_target = get_fe("casenom:matchtargetMatch"),
    b_case_nom_match = get_fe("casenom:matchattractorMatch"),
    sd_subject = vc$sdcor[vc$grp == "participant"][1] %||%
      assumptions$fallback_sd_subject,
    sd_item = vc$sdcor[vc$grp == "item_set"][1] %||%
      assumptions$fallback_sd_item,
    source = "pilot_glmm"
  )
}

build_prior_means <- function(
  pilot,
  assumptions,
  object_scale = assumptions$object_attraction_scale
) {
  b_match_target_pos <- 0
  b_match_pos <- (object_scale - 1) * pilot$b_match_attr
  b_case_nom_target_pos <- 0
  b_threeway <- (object_scale - 1) * pilot$b_case_nom_match

  list(
    intercept = pilot$intercept,
    b_case_nom = pilot$b_case_nom,
    b_match_target = pilot$b_match_target,
    b_match_attr = pilot$b_match_attr,
    b_pos_obj = assumptions$object_main_effect_logodds,
    b_case_nom_target = pilot$b_case_nom_target,
    b_case_nom_match = pilot$b_case_nom_match,
    b_case_nom_pos = assumptions$case_by_position_logodds,
    b_match_target_pos = b_match_target_pos,
    b_match_pos = b_match_pos,
    b_case_nom_target_pos = b_case_nom_target_pos,
    b_threeway = b_threeway,
    sd_subject = pilot$sd_subject,
    sd_item = pilot$sd_item
  )
}

build_prior_sds <- function(assumptions) {
  list(
    intercept = assumptions$prior_sd_intercept,
    b_case_nom = assumptions$prior_sd_fixed,
    b_match_target = assumptions$prior_sd_fixed,
    b_match_attr = assumptions$prior_sd_fixed,
    b_pos_obj = assumptions$prior_sd_fixed,
    b_case_nom_target = assumptions$prior_sd_fixed,
    b_case_nom_match = assumptions$prior_sd_fixed,
    b_case_nom_pos = assumptions$prior_sd_fixed,
    b_match_target_pos = assumptions$prior_sd_fixed,
    b_match_pos = assumptions$prior_sd_fixed,
    b_case_nom_target_pos = assumptions$prior_sd_fixed,
    b_threeway = assumptions$prior_sd_fixed,
    sd_subject = assumptions$prior_sd_sd_subject,
    sd_item = assumptions$prior_sd_sd_item
  )
}

pilot_accuracy_table <- function(pilot_data) {
  pilot_data %>%
    group_by(case, match) %>%
    summarize(
      n_trials = n(),
      p_yes = mean(response_yes),
      .groups = "drop"
    ) %>%
    mutate(accuracy_percent = 100 * p_yes)
}

cell_probabilities_from_means <- function(prior_means) {
  expand_grid(
    case = c("gen", "nom"),
    position = c("subject", "object"),
    match = c("noMatch", "targetMatch", "attractorMatch")
  ) %>%
    mutate(
      x_case_nom = as.integer(case == "nom"),
      x_pos_obj = as.integer(position == "object"),
      x_match_target = as.integer(match == "targetMatch"),
      x_match_attr = as.integer(match == "attractorMatch"),
      eta = prior_means$intercept +
        prior_means$b_case_nom * x_case_nom +
        prior_means$b_pos_obj * x_pos_obj +
        prior_means$b_match_target * x_match_target +
        prior_means$b_match_attr * x_match_attr +
        prior_means$b_case_nom_target * x_case_nom * x_match_target +
        prior_means$b_case_nom_pos * x_case_nom * x_pos_obj +
        prior_means$b_case_nom_match * x_case_nom * x_match_attr +
        prior_means$b_match_target_pos * x_pos_obj * x_match_target +
        prior_means$b_match_pos * x_pos_obj * x_match_attr +
        prior_means$b_case_nom_target_pos *
          x_case_nom *
          x_pos_obj *
          x_match_target +
        prior_means$b_threeway * x_case_nom * x_pos_obj * x_match_attr,
      p_yes = plogis(eta),
      accuracy_percent = 100 * p_yes
    ) %>%
    select(case, position, match, eta, p_yes, accuracy_percent)
}

attraction_effects_from_means <- function(prior_means) {
  tibble(
    effect = c(
      "subject_gen_attraction_attr_minus_no",
      "subject_nom_attraction_attr_minus_no",
      "object_gen_attraction_attr_minus_no"
    ),
    logodds = c(
      prior_means$b_match_attr,
      prior_means$b_match_attr + prior_means$b_case_nom_match,
      prior_means$b_match_attr + prior_means$b_match_pos
    )
  )
}

build_object_scale_sensitivity <- function(pilot, assumptions) {
  get_p <- function(cells, case_level, position_level, match_level) {
    cells %>%
      filter(
        case == case_level,
        position == position_level,
        match == match_level
      ) %>%
      pull(p_yes) %>%
      .[[1]]
  }

  map_dfr(assumptions$object_attraction_scale_grid, function(scale_value) {
    pm <- build_prior_means(pilot, assumptions, object_scale = scale_value)
    eff <- attraction_effects_from_means(pm)
    cells <- cell_probabilities_from_means(pm)

    subj_gen <- eff %>%
      filter(effect == "subject_gen_attraction_attr_minus_no") %>%
      pull(logodds) %>%
      .[[1]]
    subj_nom <- eff %>%
      filter(effect == "subject_nom_attraction_attr_minus_no") %>%
      pull(logodds) %>%
      .[[1]]
    obj_gen <- eff %>%
      filter(effect == "object_gen_attraction_attr_minus_no") %>%
      pull(logodds) %>%
      .[[1]]

    subj_gen_nomatch <- get_p(cells, "gen", "subject", "noMatch")
    subj_gen_attr <- get_p(cells, "gen", "subject", "attractorMatch")
    obj_gen_nomatch <- get_p(cells, "gen", "object", "noMatch")
    obj_gen_target <- get_p(cells, "gen", "object", "targetMatch")
    obj_gen_attr <- get_p(cells, "gen", "object", "attractorMatch")

    tibble(
      scale = scale_value,
      subject_gen_logodds = subj_gen,
      object_gen_logodds = obj_gen,
      subject_nom_logodds = subj_nom,
      subject_gen_nomatch_yes = subj_gen_nomatch,
      subject_gen_attractor_yes = subj_gen_attr,
      subject_gen_attr_minus_no_pp = 100 * (subj_gen_attr - subj_gen_nomatch),
      object_gen_nomatch_yes = obj_gen_nomatch,
      object_gen_target_yes = obj_gen_target,
      object_gen_attractor_yes = obj_gen_attr,
      object_gen_attr_minus_no_pp = 100 * (obj_gen_attr - obj_gen_nomatch)
    )
  })
}

print_assumption_walkthrough <- function(
  pilot_data,
  pilot,
  prior_means,
  assumptions
) {
  pilot_acc <- pilot_accuracy_table(pilot_data)
  model_acc <- cell_probabilities_from_means(prior_means)
  model_eff <- attraction_effects_from_means(prior_means)
  scale_sensitivity <- build_object_scale_sensitivity(pilot, assumptions)

  subj_gen <- model_eff %>%
    filter(effect == "subject_gen_attraction_attr_minus_no") %>%
    pull(logodds) %>%
    .[[1]]
  subj_nom <- model_eff %>%
    filter(effect == "subject_nom_attraction_attr_minus_no") %>%
    pull(logodds) %>%
    .[[1]]
  obj_gen <- model_eff %>%
    filter(effect == "object_gen_attraction_attr_minus_no") %>%
    pull(logodds) %>%
    .[[1]]

  subj_gen_target_yes <- model_acc %>%
    filter(case == "gen", position == "subject", match == "targetMatch") %>%
    pull(p_yes) %>%
    .[[1]]
  subj_gen_nomatch_yes <- model_acc %>%
    filter(case == "gen", position == "subject", match == "noMatch") %>%
    pull(p_yes) %>%
    .[[1]]
  subj_gen_attractor_yes <- model_acc %>%
    filter(case == "gen", position == "subject", match == "attractorMatch") %>%
    pull(p_yes) %>%
    .[[1]]
  obj_gen_nomatch_yes <- model_acc %>%
    filter(case == "gen", position == "object", match == "noMatch") %>%
    pull(p_yes) %>%
    .[[1]]
  obj_gen_target_yes <- model_acc %>%
    filter(case == "gen", position == "object", match == "targetMatch") %>%
    pull(p_yes) %>%
    .[[1]]
  obj_gen_attractor_yes <- model_acc %>%
    filter(case == "gen", position == "object", match == "attractorMatch") %>%
    pull(p_yes) %>%
    .[[1]]

  cat("\n================ EFFECT WALKTHROUGH ==================\n")
  cat("Pilot source for fixed effects:", pilot$source, "\n")
  cat("Attraction definition used here: AttractorMatch - NoMatch\n")
  cat(
    "Subject GEN attraction (log-odds; attr - no): b_match_attr =",
    sprintf("%0.4f", subj_gen),
    "\n"
  )
  cat(
    "Subject NOM attraction (log-odds; attr - no): b_match_attr + b_case_nom_match =",
    sprintf("%0.4f", subj_nom),
    "\n"
  )
  cat(
    "Object scaling rule: object_effect =",
    assumptions$object_attraction_scale,
    "* subject_effect\n"
  )
  cat(
    "Object GEN attraction (log-odds; attr - no):",
    sprintf("%0.4f", obj_gen),
    "\n"
  )
  cat(
    "Subject GEN assumed response_yes (targetMatch):",
    sprintf("%0.4f", subj_gen_target_yes),
    "\n"
  )
  cat(
    "Subject GEN assumed response_yes (noMatch):",
    sprintf("%0.4f", subj_gen_nomatch_yes),
    "\n"
  )
  cat(
    "Subject GEN assumed response_yes (attractorMatch):",
    sprintf("%0.4f", subj_gen_attractor_yes),
    "\n"
  )
  cat(
    "Subject GEN assumed attraction (attractor - noMatch; percentage points):",
    sprintf("%0.2f", 100 * (subj_gen_attractor_yes - subj_gen_nomatch_yes)),
    "\n"
  )
  cat(
    "Object GEN assumed response_yes (noMatch):",
    sprintf("%0.4f", obj_gen_nomatch_yes),
    "\n"
  )
  cat(
    "Object GEN assumed response_yes (targetMatch):",
    sprintf("%0.4f", obj_gen_target_yes),
    "\n"
  )
  cat(
    "Object GEN assumed response_yes (attractorMatch):",
    sprintf("%0.4f", obj_gen_attractor_yes),
    "\n"
  )
  cat(
    "Object GEN assumed attraction (attractor - noMatch; percentage points):",
    sprintf("%0.2f", 100 * (obj_gen_attractor_yes - obj_gen_nomatch_yes)),
    "\n"
  )
  cat("=====================================================\n\n")

  cat("Observed pilot accuracy (Yes-rate; subject-position experiment):\n")
  print(pilot_acc %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

  cat(
    "\nAssumed model cell accuracies for Gen / Object-Gen (fixed effects only):\n"
  )
  print(
    model_acc %>%
      filter(case == "gen", position %in% c("subject", "object")) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  )

  cat("\nSensitivity across object attraction scales (not just one scale):\n")
  print(scale_sensitivity %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

  invisible(
    list(
      pilot_accuracy = pilot_acc,
      assumed_cells = model_acc,
      scale_sensitivity = scale_sensitivity,
      attraction_effects = model_eff
    )
  )
}

format_normal_prior <- function(mu, sd) {
  sprintf("normal(%0.6f, %0.6f)", mu, sd)
}

make_fit_priors <- function(prior_means, prior_sds) {
  c(
    set_prior(
      format_normal_prior(prior_means$intercept, prior_sds$intercept),
      class = "Intercept"
    ),
    set_prior(
      format_normal_prior(prior_means$b_case_nom, prior_sds$b_case_nom),
      class = "b",
      coef = "x_case_nom"
    ),
    set_prior(
      format_normal_prior(prior_means$b_match_target, prior_sds$b_match_target),
      class = "b",
      coef = "x_match_target"
    ),
    set_prior(
      format_normal_prior(prior_means$b_match_attr, prior_sds$b_match_attr),
      class = "b",
      coef = "x_match_attr"
    ),
    set_prior(
      format_normal_prior(prior_means$b_pos_obj, prior_sds$b_pos_obj),
      class = "b",
      coef = "x_pos_obj"
    ),
    set_prior(
      format_normal_prior(
        prior_means$b_case_nom_target,
        prior_sds$b_case_nom_target
      ),
      class = "b",
      coef = "x_case_nom:x_match_target"
    ),
    set_prior(
      format_normal_prior(
        prior_means$b_case_nom_match,
        prior_sds$b_case_nom_match
      ),
      class = "b",
      coef = "x_case_nom:x_match_attr"
    ),
    set_prior(
      format_normal_prior(prior_means$b_case_nom_pos, prior_sds$b_case_nom_pos),
      class = "b",
      coef = "x_case_nom:x_pos_obj"
    ),
    set_prior(
      format_normal_prior(
        prior_means$b_match_target_pos,
        prior_sds$b_match_target_pos
      ),
      class = "b",
      coef = "x_match_target:x_pos_obj"
    ),
    set_prior(
      format_normal_prior(prior_means$b_match_pos, prior_sds$b_match_pos),
      class = "b",
      coef = "x_match_attr:x_pos_obj"
    ),
    set_prior(
      format_normal_prior(
        prior_means$b_case_nom_target_pos,
        prior_sds$b_case_nom_target_pos
      ),
      class = "b",
      coef = "x_case_nom:x_match_target:x_pos_obj"
    ),
    set_prior(
      format_normal_prior(prior_means$b_threeway, prior_sds$b_threeway),
      class = "b",
      coef = "x_case_nom:x_match_attr:x_pos_obj"
    ),
    set_prior(
      format_normal_prior(prior_means$sd_subject, prior_sds$sd_subject),
      class = "sd",
      coef = "Intercept",
      group = "participant"
    ),
    set_prior(
      format_normal_prior(prior_means$sd_item, prior_sds$sd_item),
      class = "sd",
      coef = "Intercept",
      group = "item_set"
    )
  )
}

# ---------------------------- Simulation --------------------------------------
condition_table <- expand_grid(
  case = c("gen", "nom"),
  match = c("noMatch", "targetMatch", "attractorMatch"),
  position = c("subject", "object")
) %>%
  mutate(cond_idx = row_number())

simulate_data <- function(
  seed,
  n_participants,
  n_item_sets,
  prior_means,
  prior_sds
) {
  set.seed(seed)

  sampled <- list(
    intercept = rnorm(1, prior_means$intercept, prior_sds$intercept),
    b_case_nom = rnorm(1, prior_means$b_case_nom, prior_sds$b_case_nom),
    b_match_target = rnorm(
      1,
      prior_means$b_match_target,
      prior_sds$b_match_target
    ),
    b_match_attr = rnorm(1, prior_means$b_match_attr, prior_sds$b_match_attr),
    b_pos_obj = rnorm(1, prior_means$b_pos_obj, prior_sds$b_pos_obj),
    b_case_nom_target = rnorm(
      1,
      prior_means$b_case_nom_target,
      prior_sds$b_case_nom_target
    ),
    b_case_nom_match = rnorm(
      1,
      prior_means$b_case_nom_match,
      prior_sds$b_case_nom_match
    ),
    b_case_nom_pos = rnorm(
      1,
      prior_means$b_case_nom_pos,
      prior_sds$b_case_nom_pos
    ),
    b_match_target_pos = rnorm(
      1,
      prior_means$b_match_target_pos,
      prior_sds$b_match_target_pos
    ),
    b_match_pos = rnorm(1, prior_means$b_match_pos, prior_sds$b_match_pos),
    b_case_nom_target_pos = rnorm(
      1,
      prior_means$b_case_nom_target_pos,
      prior_sds$b_case_nom_target_pos
    ),
    b_threeway = rnorm(1, prior_means$b_threeway, prior_sds$b_threeway),
    sd_subject = abs(rnorm(1, prior_means$sd_subject, prior_sds$sd_subject)),
    sd_item = abs(rnorm(1, prior_means$sd_item, prior_sds$sd_item))
  )

  n_cond <- nrow(condition_table)
  assigned_lists <- sample(rep(seq_len(n_cond), length.out = n_participants))

  participants <- tibble(
    participant = factor(sprintf("s_%03d", seq_len(n_participants))),
    list_id = assigned_lists
  )

  items <- tibble(
    item_set = factor(sprintf("i_%03d", seq_len(n_item_sets))),
    item_num = seq_len(n_item_sets)
  )

  subj_re <- tibble(
    participant = participants$participant,
    re_subj = rnorm(n_participants, 0, sampled$sd_subject)
  )

  item_re <- tibble(
    item_set = items$item_set,
    re_item = rnorm(n_item_sets, 0, sampled$sd_item)
  )

  d <- expand_grid(
    participant = participants$participant,
    item_set = items$item_set
  ) %>%
    left_join(participants, by = "participant") %>%
    left_join(items, by = "item_set") %>%
    mutate(cond_idx = ((item_num + list_id - 2L) %% n_cond) + 1L) %>%
    left_join(condition_table, by = "cond_idx") %>%
    left_join(subj_re, by = "participant") %>%
    left_join(item_re, by = "item_set") %>%
    mutate(
      x_case_nom = as.integer(case == "nom"),
      x_match_target = as.integer(match == "targetMatch"),
      x_match_attr = as.integer(match == "attractorMatch"),
      x_pos_obj = as.integer(position == "object"),
      eta = sampled$intercept +
        sampled$b_case_nom * x_case_nom +
        sampled$b_match_target * x_match_target +
        sampled$b_match_attr * x_match_attr +
        sampled$b_pos_obj * x_pos_obj +
        sampled$b_case_nom_target * x_case_nom * x_match_target +
        sampled$b_case_nom_match * x_case_nom * x_match_attr +
        sampled$b_case_nom_pos * x_case_nom * x_pos_obj +
        sampled$b_match_target_pos * x_match_target * x_pos_obj +
        sampled$b_match_pos * x_match_attr * x_pos_obj +
        sampled$b_case_nom_target_pos *
          x_case_nom *
          x_match_target *
          x_pos_obj +
        sampled$b_threeway * x_case_nom * x_match_attr * x_pos_obj +
        re_subj +
        re_item,
      p_yes = plogis(eta),
      response_yes = rbinom(n(), 1, p_yes),
      case = factor(case, levels = c("gen", "nom")),
      match = factor(
        match,
        levels = c("noMatch", "targetMatch", "attractorMatch")
      ),
      position = factor(position, levels = c("subject", "object"))
    ) %>%
    select(
      participant,
      item_set,
      case,
      match,
      position,
      x_case_nom,
      x_match_target,
      x_match_attr,
      x_pos_obj,
      response_yes
    )

  list(data = d, sampled_params = sampled)
}

true_contrasts <- function(sampled) {
  a_subj_gen <- sampled$b_match_attr
  a_subj_nom <- sampled$b_match_attr + sampled$b_case_nom_match
  a_obj_gen <- sampled$b_match_attr + sampled$b_match_pos

  c(
    attraction_subject_gen = a_subj_gen,
    attraction_subject_nom = a_subj_nom,
    attraction_object_gen = a_obj_gen,
    attraction_subject_avg = 0.5 * (a_subj_gen + a_subj_nom),
    attraction_object_gen_minus_subject_gen = a_obj_gen - a_subj_gen
  )
}

extract_contrast_draws <- function(draws) {
  n <- nrow(draws)
  get_col <- function(name) {
    if (name %in% names(draws)) draws[[name]] else rep(0, n)
  }

  b_match <- get_col("b_x_match_attr")
  b_case_match <- get_col("b_x_case_nom:x_match_attr")
  b_match_pos <- get_col("b_x_match_attr:x_pos_obj")

  list(
    attraction_subject_gen = b_match,
    attraction_subject_nom = b_match + b_case_match,
    attraction_object_gen = b_match + b_match_pos,
    attraction_subject_avg = b_match + 0.5 * b_case_match,
    attraction_object_gen_minus_subject_gen = b_match_pos
  )
}

summarize_contrast <- function(contrast_name, draw_vec, true_value) {
  q <- quantile(draw_vec, probs = c(0.025, 0.975), na.rm = TRUE)

  tibble(
    contrast = contrast_name,
    estimate = mean(draw_vec, na.rm = TRUE),
    Q2.5 = unname(q[[1]]),
    Q97.5 = unname(q[[2]]),
    true_value = true_value,
    significant = (q[[1]] > 0 && q[[2]] > 0) || (q[[1]] < 0 && q[[2]] < 0),
    ci_width = unname(q[[2]] - q[[1]]),
    covers_true = (q[[1]] <= true_value) && (true_value <= q[[2]])
  )
}

sim_d_and_fit <- function(
  seed,
  n_participants,
  fit_init,
  prior_means,
  prior_sds,
  config
) {
  sim <- simulate_data(
    seed = seed,
    n_participants = n_participants,
    n_item_sets = config$n_item_sets,
    prior_means = prior_means,
    prior_sds = prior_sds
  )

  fit_new <- tryCatch(
    update(
      fit_init,
      newdata = sim$data,
      seed = seed,
      chains = config$chains,
      cores = config$cores,
      threads = threading(config$threads),
      iter = config$iter,
      warmup = config$warmup,
      refresh = 0,
      recompile = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(fit_new)) {
    return(
      tibble(
        contrast = names(true_contrasts(sim$sampled_params)),
        estimate = NA_real_,
        Q2.5 = NA_real_,
        Q97.5 = NA_real_,
        true_value = unname(true_contrasts(sim$sampled_params)),
        significant = FALSE,
        ci_width = NA_real_,
        covers_true = FALSE
      )
    )
  }

  draws <- as_draws_df(fit_new)
  draw_contr <- extract_contrast_draws(draws)
  true_vals <- true_contrasts(sim$sampled_params)

  map_dfr(names(draw_contr), function(nm) {
    summarize_contrast(
      contrast_name = nm,
      draw_vec = draw_contr[[nm]],
      true_value = true_vals[[nm]]
    )
  })
}

calculate_power <- function(
  n_participants,
  n_sim,
  fit_init,
  prior_means,
  prior_sds,
  config
) {
  cat(sprintf("Running %d simulations for n = %d...\n", n_sim, n_participants))

  sim_results <- map_dfr(
    seq_len(n_sim),
    function(i) {
      if (i %% 25 == 0) {
        cat(sprintf("  simulation %d/%d\n", i, n_sim))
      }

      sim_d_and_fit(
        seed = n_participants * 10000 + i,
        n_participants = n_participants,
        fit_init = fit_init,
        prior_means = prior_means,
        prior_sds = prior_sds,
        config = config
      )
    },
    .id = "sim"
  )

  sim_results %>%
    group_by(contrast) %>%
    summarize(
      power = mean(significant, na.rm = TRUE),
      mean_estimate = mean(estimate, na.rm = TRUE),
      mean_true = mean(true_value, na.rm = TRUE),
      sd_true = sd(true_value, na.rm = TRUE),
      bias = mean(estimate - true_value, na.rm = TRUE),
      coverage = mean(covers_true, na.rm = TRUE),
      mean_ci_width = mean(ci_width, na.rm = TRUE),
      n_sims = n(),
      .groups = "drop"
    ) %>%
    mutate(n_participants = n_participants)
}

plot_power <- function(power_results, sample_sizes, n_simulations) {
  ggplot(power_results, aes(x = n_participants, y = power, color = contrast)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.8) +
    geom_hline(yintercept = 0.80, linetype = "dashed", linewidth = 0.7) +
    geom_hline(yintercept = 0.90, linetype = "dotted", linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = sample_sizes) +
    labs(
      title = "Bayesian Power Analysis (Prior Predictive)",
      subtitle = sprintf("response_yes | %d simulations per N", n_simulations),
      x = "Number of participants",
      y = "Power (95% CrI excludes 0)",
      color = "Attraction contrast"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# ---------------------------- Main --------------------------------------------
print_assumptions(assumptions, config)

script_path <- get_script_path()
project_root <- infer_project_root(script_path)
pilot_path <- resolve_data_path(
  project_root = project_root,
  explicit_path = config$pilot_data_path,
  script_path = script_path
)

cat("Runtime working directory:", normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n")
cat("Runtime script path:", ifelse(is.na(script_path), "<unknown>", script_path), "\n")
cat("Runtime project root:", project_root, "\n")
cat("Resolved pilot data path:", pilot_path, "\n\n")

pilot_data <- load_pilot_data(pilot_path)
pilot <- estimate_pilot_effects(pilot_data, assumptions)

prior_means <- build_prior_means(
  pilot = pilot,
  assumptions = assumptions,
  object_scale = assumptions$object_attraction_scale
)
prior_sds <- build_prior_sds(assumptions)

walkthrough_artifacts <- print_assumption_walkthrough(
  pilot_data = pilot_data,
  pilot = pilot,
  prior_means = prior_means,
  assumptions = assumptions
)

cat("Pilot effect source:", pilot$source, "\n")
cat("\nPrior means (log-odds / SD scale):\n")
print(round(unlist(prior_means), 3))
cat("\nPrior SDs:\n")
print(round(unlist(prior_sds), 3))

fit_priors <- make_fit_priors(prior_means, prior_sds)

power_formula <- bf(
  response_yes ~ x_case_nom *
    (x_match_target + x_match_attr) *
    x_pos_obj +
    (1 | participant) +
    (1 | item_set)
)

init_sim <- simulate_data(
  seed = 26022026,
  n_participants = max(config$sample_sizes),
  n_item_sets = config$n_item_sets,
  prior_means = prior_means,
  prior_sds = prior_sds
)

fit_init <- brm(
  formula = power_formula,
  data = init_sim$data,
  family = bernoulli(link = "logit"),
  prior = fit_priors,
  chains = config$chains,
  cores = config$cores,
  threads = threading(config$threads),
  iter = config$iter,
  warmup = config$warmup,
  seed = 26022026,
  backend = "cmdstanr",
  refresh = 0
)

power_results <- map_dfr(config$sample_sizes, function(n_subs) {
  calculate_power(
    n_participants = n_subs,
    n_sim = config$n_simulations,
    fit_init = fit_init,
    prior_means = prior_means,
    prior_sds = prior_sds,
    config = config
  )
})

output_csv <- resolve_output_path(config$output_csv, project_root, config$output_base_dir)
output_plot <- resolve_output_path(config$output_plot, project_root, config$output_base_dir)
output_assumptions_txt <- resolve_output_path(
  config$output_assumptions_txt,
  project_root,
  config$output_base_dir
)
output_pilot_accuracy_csv <- resolve_output_path(
  config$output_pilot_accuracy_csv,
  project_root,
  config$output_base_dir
)
output_assumed_cells_csv <- resolve_output_path(
  config$output_assumed_cells_csv,
  project_root,
  config$output_base_dir
)
output_attraction_effects_csv <- resolve_output_path(
  config$output_attraction_effects_csv,
  project_root,
  config$output_base_dir
)
output_scale_sensitivity_csv <- resolve_output_path(
  config$output_scale_sensitivity_csv,
  project_root,
  config$output_base_dir
)
output_prior_params_csv <- resolve_output_path(
  config$output_prior_params_csv,
  project_root,
  config$output_base_dir
)
output_min_n_csv <- resolve_output_path(
  config$output_min_n_csv,
  project_root,
  config$output_base_dir
)

write_csv(power_results, output_csv)
write_csv(walkthrough_artifacts$pilot_accuracy, output_pilot_accuracy_csv)
write_csv(walkthrough_artifacts$assumed_cells, output_assumed_cells_csv)
write_csv(walkthrough_artifacts$attraction_effects, output_attraction_effects_csv)
write_csv(walkthrough_artifacts$scale_sensitivity, output_scale_sensitivity_csv)

prior_param_tbl <- tibble(
  parameter = names(prior_means),
  prior_mean = as.numeric(unlist(prior_means)),
  prior_sd = as.numeric(unlist(prior_sds)[names(prior_means)])
)
write_csv(prior_param_tbl, output_prior_params_csv)

power_curve <- plot_power(
  power_results,
  sample_sizes = config$sample_sizes,
  n_simulations = config$n_simulations
)

ggsave(output_plot, power_curve, width = 11, height = 7, dpi = 300)

cat("\nSaved summary to:", output_csv, "\n")
cat("Saved plot to:", output_plot, "\n")

cat("\nMinimum N for 80% power:\n")
min_n_80 <- power_results %>%
  filter(power >= 0.80) %>%
  group_by(contrast) %>%
  slice_min(order_by = n_participants, n = 1, with_ties = FALSE) %>%
  ungroup()

print(min_n_80)
write_csv(min_n_80, output_min_n_csv)

assumptions_report_lines <- capture.output({
  print_assumptions(assumptions, config)
  cat("\n================ EFFECT WALKTHROUGH ==================\n")
  cat("Pilot source for fixed effects:", pilot$source, "\n")
  cat("Attraction definition used here: AttractorMatch - NoMatch\n")
  cat("\nObserved pilot accuracy (Yes-rate):\n")
  print(walkthrough_artifacts$pilot_accuracy %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  cat("\nAssumed model cell accuracies:\n")
  print(walkthrough_artifacts$assumed_cells %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  cat("\nAttraction effects (AttractorMatch - NoMatch):\n")
  print(walkthrough_artifacts$attraction_effects %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  cat("\nScale sensitivity:\n")
  print(walkthrough_artifacts$scale_sensitivity %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  cat("\nPrior means (log-odds / SD scale):\n")
  print(round(unlist(prior_means), 3))
  cat("\nPrior SDs:\n")
  print(round(unlist(prior_sds), 3))
  cat("\nMinimum N for 80% power:\n")
  print(min_n_80)
})
writeLines(assumptions_report_lines, output_assumptions_txt)

cat("Saved assumptions report to:", output_assumptions_txt, "\n")
cat("Saved pilot accuracy table to:", output_pilot_accuracy_csv, "\n")
cat("Saved assumed cells table to:", output_assumed_cells_csv, "\n")
cat("Saved attraction effects table to:", output_attraction_effects_csv, "\n")
cat("Saved scale sensitivity table to:", output_scale_sensitivity_csv, "\n")
cat("Saved prior parameter table to:", output_prior_params_csv, "\n")
cat("Saved min-N table to:", output_min_n_csv, "\n")
