## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted)
conflicts_prefer(purrr::discard)
## ------------------------------------------------------------

grade_tbl <- read_excel(file.path(path_home(), "Documents/GitHub/archive/_docs/grades.xlsx"))

ggplot(grade_tbl, aes(x = grade)) +
  theme_minimal() + 
  geom_histogram()

ggsave(file.path(path_home(), "Documents/GitHub/archive/_docs/density.png"))
  