## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, fs, zoo,
               conflicted)
conflicts_prefer(purrr::discard)