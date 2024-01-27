## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted, see, janitor)
conflicts_prefer(purrr::discard)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## ------------------------------------------------------------

grade_tbl <- read_excel(file.path(path_home(), "work/GitHub/archive/_docs/grades.xlsx")) %>% 
  mutate(grade = as.character(grade))


count_tbl <- grade_tbl %>% 
  pull(grade) %>% 
  table() %>% 
  enframe(name = "grade", value = "count") %>% 
  mutate(label = count/nrow(grade_tbl),
         label = as.vector(label),
         label = scales::percent(label, accuracy = 0.1))

ggplot(count_tbl, aes(x = grade, y = count, fill = grade)) +
  theme_minimal() + 
  geom_bar(stat = "identity") +
  labs(x = "", y = "",
       caption = "Die Durchfallquote bezieht sich auf abgegebene und somit geschriebene Klausuren.") +
  scale_fill_manual(values = c(rep("#56B4E9", 10), "#CC79A7")) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_text(data = count_tbl, aes(x = grade, y = count + 3, label = label),
            size = 5, fill = "white") +
  geom_vline(xintercept = 10.5, linetype = 2) +
  annotate("label", x = 9.5, y = max(count_tbl$count), label = str_c("n = ", nrow(grade_tbl)),
           size = 6) +
  scale_y_continuous(breaks = seq(0, max(count_tbl$count) + 10, 10), 
                     limits = c(0,  max(count_tbl$count) + 10)) 
  
ggsave(file.path(path_home(), "work/GitHub/archive/_docs/density.jpg"),
       width = 8, height = 5, units = "cm")


count_year_tbl <- grade_tbl %>%
  mutate(grade = as.numeric(grade)) %>% 
  group_by(year) %>% 
  reframe(percent = tabyl(grade)) %>% 
  unnest(cols = c("percent"))

count_year_sum_tbl <- grade_tbl %>% 
  group_by(year) %>% 
  summarise(sum_n = str_c("n = ", n()),
            n = n()) %>% 
  mutate(grade = "1")

ggplot(count_year_tbl, aes(as.character(grade), n, 
                           fill = as.character(grade))) +
  theme_minimal() + 
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  labs(x = "", y = "",
       caption = "Die Durchfallquote bezieht sich auf abgegebene und somit geschriebene Klausuren.") +
  scale_fill_manual(values = c(rep("#56B4E9", 10), "#CC79A7")) +
  geom_vline(xintercept = 10.5, linetype = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_blank(),
        strip.text = element_text(size=14)) +
  geom_text(data = count_year_tbl, aes(x = as.character(grade), 
                                       y = n,
                                       label = scales::percent(percent, accuracy = 0.1)),
            size = 6, vjust = "bottom") +
  scale_y_continuous(expand = c(0.3, 0))#+
  #geom_label(data = count_year_sum_tbl, aes(grade, n/4, label = sum_n),
  #           fill = "white", size = 5.5)

ggsave(file.path(path_home(), "work/GitHub/archive/_docs/density_year.jpg"),
       width = 9, height = (nrow(count_year_sum_tbl) * 3), units = "cm")

