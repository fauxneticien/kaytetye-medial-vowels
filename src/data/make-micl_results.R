# Helper to grab packages, if you're running this on a remote server
lapply(
    X = c("mclust", "dplyr", "readr", "here", "furrr"),
    FUN = function(package) if(!package %in% installed.packages()) install.packages(package)
)

library(mclust)
library(dplyr)
library(readr)
library(here)
library(furrr)

# Input data
vowels_med_analysis <-
    read_csv(
        file      = here("data/processed/vowels_med_analysis.csv"),
        col_types = "ciicccccddcdddddddddd"
    )

# Source helper functions
source(here("src/data/mclust_helpers.R"))

plan(multiprocess) # run future_map2 in parallel across multiple R sessions
set.seed(16)       # set seed for sample.int(n()) to be reproducible

mICL_df <-
    expand.grid(
        iteration = 1:10000,
        tracker = c("forest", "praat"),
        stringsAsFactors = FALSE
    ) %>%
    arrange(iteration) %>% 
    mutate(
        seed = sample.int(n()),
        data = sample_vowel_midpoints(tracker, seed) %>% list(),
        mICL = future_map2(
            .x = seed,
            .y = data,
            .f = ~ tidy_mclustICL(seed = .x, data = .y[,1:2], G = 2:5),
            .progress = TRUE
        )
    )

mICL_df %>%
    select(-data) %>%
    tidyr::unnest(mICL) %>%
    readr::write_csv(here("data/processed/micl_results.csv"))

