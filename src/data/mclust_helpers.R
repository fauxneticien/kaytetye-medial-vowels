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
