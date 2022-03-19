library(tidyverse)
library(nflfastR)
library(ggrepel)
library(patchwork)
library(fmsb)

cons_theme <- theme(
  text = element_text(family = "Source Serif Pro"),
  panel.border = element_blank(),
  panel.grid = element_line(size = 0.5),
  plot.title = element_text(size = 20, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic", margin = margin(10, 0, 10, 0)),
  axis.title.x = element_text(size = 15, margin=margin(20,0,0,0)),
  axis.title.y = element_text(size = 15, margin=margin(0,20,0,0))
)

pbp19 <- nflfastR::load_pbp(2019:2021)

pbp19 %>% 
  filter(passer_player_name == "D.Lock") %>% 
  group_by(game_id) %>% 
  summarize(epa = mean(epa, na.rm = T), cpoe = mean(cpoe, na.rm = T)) %>% 
  View()

horsecock <- pbp19 %>% 
  filter(passer_player_name == "D.Lock") %>% 
  mutate(epa_cumsum = cumsum(epa), n = 1:n())

epacum <- horsecock %>% 
  filter(pass == 1) %>% 
  mutate(neg = epa_cumsum < 0) %>% 
  ggplot() +
  geom_area(aes(x = n, y = ifelse(epa_cumsum > 0, epa_cumsum, 0)), fill = "#83af70") + 
  geom_area(aes(x = n, y = ifelse(epa_cumsum < 0, epa_cumsum, 0)), fill = "#ff7656") + 
  labs(x = "Play Number", y = "Cumulative EPA") +
  theme_minimal() +
  cons_theme

epacpoe <- pbp19 %>% 
  group_by(passer_player_name, posteam) %>% 
  filter(pass == 1) %>% 
  summarize(n = n(), epa = mean(epa, na.rm = T), cpoe = mean(cpoe, na.rm = T)) %>% 
  filter(n > 50) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  ggplot() +
  geom_point(aes(x = epa, y = cpoe, alpha = ifelse(passer_player_name == "D.Lock", 1, 0.2), color = team_color2, size = n)) +
  geom_label_repel(aes(x = epa, y = cpoe, label = ifelse(passer_player_name %in% c("D.Lock", "R.Wilson"), passer_player_name, "")), family = "Source Serif Pro", point.padding = 0.5, nudge_y = 1, nudge_x = 0.1) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  scale_alpha_identity(aesthetic = "alpha") +
  labs(x = "EPA/Play", y = "CPOE", title = "Drew Lock has been pretty mediocre, will that change this year?", subtitle = "QBs with at least 50 snaps from 2019-2021. Size of point = number of plays") +
  theme_minimal() +
  theme(legend.position = "none") +
  cons_theme

cpoe <- horsecock %>% 
  filter(pass == 1) %>% 
  mutate(passdesc = str_to_title(paste0(pass_length, " ", pass_location))) %>% 
  group_by(passdesc) %>% 
  summarize(cpoe = mean(cpoe, na.rm = T)) %>% 
  filter(!str_detect(passdesc, "Na")) %>%
  ggplot() +
  geom_bar(aes(x = passdesc, y = cpoe, fill = ifelse(cpoe < 0, "#ff7656", "#83af70")), stat = "identity", width = 0.5) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(y = "CPOE", x = "", caption = "@swingmisstake | Data: nflfastR") +
  theme_minimal() +
  cons_theme
  
  
epacpoe + (epacum / cpoe)

ggsave("~/desktop/projects/springviz/viz.png", width = 16, height = 10, units = "in")
