library(brms)
library(tidyverse)

model_file <- "data/models/int.model.rds"
if (file.exists(model_file)) {
    print(paste("Attempting to load:", model_file))
    tryCatch(
        {
            fit <- readRDS(model_file)
            print("Model loaded.")
            print("Attempting posterior_summary...")
            summ <- posterior_summary(fit, pars = "^b_")
            print("Summary successful.")
            print(head(summ))
        },
        error = function(e) {
            print("Error encountered:")
            print(e)
        }
    )
} else {
    print("Model file does not exist.")
}
