## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted, see)
conflicts_prefer(purrr::discard)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## ------------------------------------------------------------

grade_tbl <- read_excel(file.path(path_home(), "Documents/GitHub/archive/_docs/grades.xlsx")) %>% 
  mutate(grade = as.character(grade))


count_tbl <- grade_tbl %>% 
  pull(grade) %>% 
  table() %>% 
  enframe(name = "grade", value = "count") %>% 
  mutate(label = count/nrow(grade_tbl),
         label = as.vector(label),
         label = scales::percent(label))

ggplot(count_tbl, aes(x = grade, y = count, fill = grade)) +
  theme_minimal() + 
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  scale_fill_manual(values = c(rep("#56B4E9", 10), "#CC79A7")) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_text(data = count_tbl, aes(x = grade, y = count + 2, label = label),
            size = 5) +
  geom_vline(xintercept = 10.5, linetype = 2) +
  annotate("label", x = 9.5, y = max(count_tbl$count), label = str_c("n = ", nrow(grade_tbl)),
           size = 8)
  
ggsave(file.path(path_home(), "Documents/GitHub/archive/_docs/density.png"),
       width = 9, height = 6)
