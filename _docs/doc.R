## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted, see)
conflicts_prefer(purrr::discard)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## ------------------------------------------------------------

grade_tbl <- read_excel(file.path(path_home(), "Documents/GitHub/archive/_docs/grades.xlsx")) %>% 
  mutate(grade = as.character(grade))


count_tab <- grade_tbl %>% 
  pull(grade) %>% 
  table() %>% 
  enframe(name = "grade", value = "count") %>% 
  mutate(label = count/nrow(grade_tbl),
         label = as.vector(label),
         label = scales::percent(label))

ggplot(count_tab, aes(x = grade, y = count, fill = grade)) +
  theme_minimal() + 
  geom_bar(stat = "identity") +
  labs(x = "Note", y = "Anzahl") +
  scale_fill_manual(values = c(rep("#56B4E9", 10), "#CC79A7")) +
  theme(legend.position = "none") +
  geom_text(data = percent_tbl, aes(x = grade, y = count + 1, label = label)) +
  geom_vline(xintercept = 10.5, linetype = 2) +
  annotate("label", 9, max(count_tab$count), 
           label = str_c("n = ", nrow(grade_tbl)), size = 7)
  
ggsave(file.path(path_home(), "Documents/GitHub/archive/_docs/density.png"),
       width = 9, height = 6)