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

# Define helper functions

## Sampler function: since there are multiple mid-point annotations per vowel
## by different annotators, we can sample from multiple mid-point annotations
sample_vowel_midpoints <- function(tracker, seed, df = vowels_med_analysis) {
    f1_col <- paste0(tracker, ".f1_z")
    f2_col <- paste0(tracker, ".f2_z")
    
    # set given seed so sample_n() below is reproducible
    set.seed(seed)
    
    df %>% 
        group_by(source_file, rep_num, vowel_num) %>%
        sample_n(1) %>% 
        ungroup %>%
        # Put f1, f2 column first for easier access later
        select(f1_col, f2_col, source_file, rep_num, vowel_num, annotator)
}

## Fitter function: returns mclust's mICL fit in a tidy data format
tidy_mclustICL <- function(seed, G = 2:5, ...) {
    set.seed(seed)
    mICL <- mclustICL(G, ...)
    
    cbind(
        expand.grid(
            G = attr(mICL, "dimnames")[[1]],
            modelName = attr(mICL, "dimnames")[[2]],
            stringsAsFactors = FALSE
        ),
        ICL = as.numeric(mICL)
    )
}

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

