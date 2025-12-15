library(knitr)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)
library(posterior)
library(glue)
library(magrittr)
library(ggplot2)

# Random Seed Generator (Random.org)
makeActiveBinding(
    "randnum",
    function() {
        as.integer(system(
            "curl -s 'https://www.random.org/integers/?num=1&min=10000000&max=99999999&col=1&base=10&format=plain&ind=new'",
            intern = TRUE
        ))
    },
    .GlobalEnv
)


options(brms.backend = "cmdstanr")
theme_set(theme_classic())
