# To be run from the top-level directory (i.e. where 'kayt_vowels.Rproj' is),
# assuming you have access to, and have already cloned, all batch repos b001,
# b002, etc. into the data/raw directory
ls -d data/raw/* | xargs -P10 -I{} git -C {} rev-parse HEAD
