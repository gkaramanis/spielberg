library(tidyverse)
library(googlesheets4)

# Scrape Wikipedia table
# library(rvest)
# url <- "https://en.wikipedia.org/wiki/Steven_Spielberg_filmography"
# 
# filmography_director <- read_html(url) %>% 
#   html_nodes("table") %>% 
#   .[[1]] %>% 
#   html_table() %>% 
#   janitor::clean_names() %>% 
#   filter(credited_as == "Yes")
# 
# filmography_director %>% 
#   write_csv("~/Desktop/spielberg.csv")
  
film_years_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1c4n09dPArvz3K7DULj_yWHQocQwfA6WalZqQLjdPcRY/edit#gid=207543724") %>% 
  janitor::clean_names()
  
film_years <- film_years_raw %>% 
  mutate(
    film = as.character(film),
    film = str_wrap(film, 30),
    film = fct_reorder(film, -year),
    color = ifelse(year > takes_place_from, "purple", "darkorange"),
    takes_place_to = ifelse(!is.na(takes_place_to), takes_place_to, takes_place_from),
    takes_place_to = ifelse(takes_place_to > 3000, 2119, takes_place_to)
    ) %>% 
  filter(!is.na(takes_place_from))

f1 = "General Sans"

ggplot(film_years) +
  # Born line
  geom_vline(xintercept = 1946, color = "grey85", size = 1.5) +
  # Connect movie release year and year it takes place
  geom_segment(aes(x = year, xend = takes_place_from, y = film, yend = film, color = color), size = 0.4, lineend = "round") +
  # Line from takes_place_from to takes_place_to (using 0.05 makes it visible when takes_place_from = takes_place_to)
  geom_segment(aes(x = takes_place_from - 0.05, xend = takes_place_to + 0.05, y = film, yend = film, color = color), size = 4, lineend = "round") +
  # Release year
  geom_point(aes(x = year, y = film), shape = 21, size = 1.8, stroke = 1, fill = "grey97") +
  # Annotations
  annotate("text", x = 1995, y = 33, label = "Movie release year", family = f1, size = 5) +
  annotate("text", x = 1940, y = 33, label = "Born", family = f1, size = 5, hjust = 1) +
  annotate("text", x = 2120, y = 15, label = ">>>", family = f1, fontface = "bold", size = 4, hjust = 1, color = "grey97", vjust = 0.45) +
  # Scales, title, theme, etc.
  scale_x_continuous(breaks = seq(1840, 2100, 20), limits = c(1835, 2120)) +
  scale_color_identity() +
  labs(
    title = "When do Spielberg's movies take place?",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_text(size = 30, margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(size = 10, margin = margin(10, 0, 0, 0), color = "grey40")
  )

# Save image
ggsave("spielberg.png", dpi = 320, width = 11, height = 12)
