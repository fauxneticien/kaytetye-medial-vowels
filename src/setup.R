# Assuming starting from rocker/verse Docker image: https://hub.docker.com/r/rocker/verse/
# This image already includes all of tidyverse, devtools, tex & publishing-related packages

install.packages(c(
    "DT",
    "furrr",
    "here",
    "gridExtra",
    "ggrepel",
    "mclust"
    ))

devtools::install_github('VPetukhov/ggrastr')
