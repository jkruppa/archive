## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted, see)
conflicts_prefer(purrr::discard)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## ------------------------------------------------------------

grade_tbl <- read_excel(file.path(path_home(), "Documents/GitHub/archive/_docs/grades.xlsx")) %>% 
  mutate(grade = as.character(grade))

ggplot(grade_tbl, aes(x = grade, fill = grade)) +
  theme_minimal() + 
  geom_bar(stat = "count") +
  labs(x = "Note", y = "Anzahl") +
  scale_fill_manual(values = c(rep("#56B4E9", 10), "#CC79A7")) +
  theme(legend.position = "none")
  

ggsave(file.path(path_home(), "Documents/GitHub/archive/_docs/density.png"))
  