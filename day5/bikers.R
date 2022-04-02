library(tidyverse)
library(httr)
library(RSocrata)

fremont <- read.socrata(
  "https://data.seattle.gov/resource/65db-xm6k.json",
  app_token = "VGgsxc9wNy7UClwORQeCdIrkh",
  email     = "klobby19@gmail.com",
  password  = "pONriA4xlJ5E"
)

cons_theme <- 
  theme_minimal() +
  theme(
  text = element_text(family = "Source Serif Pro"),
  panel.border = element_blank(),
  panel.grid = element_line(size = 0.5),
  plot.title = element_text(size = 20, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic", margin = margin(10, 0, 10, 0)),
  axis.title.x = element_text(size = 20, margin=margin(20,0,0,0)),
  axis.title.y = element_text(size = 20, margin=margin(0,20,0,0)),
  axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8),
  axis.text.y = element_text(size = 15, face = "italic"),
  plot.margin = margin(20, 20, 20, 20)
)

fremont$time <- format(fremont$date, format = "%H:%M:%S")
fremont$date <- as.Date(fremont$date)

fremont$fremont_bridge <- as.numeric(fremont$fremont_bridge)

fremont %>% 
  ggplot() +
  geom_histogram(aes(x = fremont_bridge)) +
  facet_wrap(~ time)

fremont %>% 
  mutate(year = str_sub(date, 1, 4), day = str_sub(date, 6, 10), month = str_sub(date, 6, 7)) %>%
  group_by(month, year) %>% 
  summarize(avg = mean(fremont_bridge, na.rm = T), cumsum = cumsum(fremont_bridge)) %>% 
  ggplot() +
  geom_line(aes(x = month, y = avg, color = year, group = year))

fremont %>% 
  ggplot() +
  geom_histogram(aes(x = fremont_bridge), binwidth = 10)

fremont %>% 
  ggplot() +
  geom_density(aes(x = fremont_bridge, color = time)) +
  xlim(0, 300)

fremont %>%
  group_by(time) %>% 
  summarize(total = sum(fremont_bridge, na.rm = T), avg = mean(fremont_bridge, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x = time, y = avg, group = 1))

fremont %>% 
  mutate(year = str_sub(date, 1, 4), day = str_sub(date, 6, 10), month = str_sub(date, 6, 7)) %>%
  filter(!is.na(fremont_bridge) & !is.na(year)) %>%
  group_by(time, year) %>% 
  summarize(avg = mean(fremont_bridge, na.rm = T)) %>% 
  ggplot() +
  geom_tile(aes(x = time, y = year, fill = avg)) +
  labs(x = "", y = "", title = "Average Number of Bikers That Use The Fremont Bridge by Time of Day(2012-2022)", subtitle = "How long will it take for biker levels to go back to pre-pandemic levels? The lighter blue represents more bikers*", caption = "@swingmisstake | Data: City of Seattle Open Data Program") +
  cons_theme +
  theme(legend.position = "none")

ggsave("~/desktop/projects/springviz/day5/bikers.png", width = 14, height = 8, units = "in")
