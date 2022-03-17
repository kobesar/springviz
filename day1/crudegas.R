library(tidyverse)
library(rjson)
library(httr)
library(lubridate)
library(ggrepel)

cons_theme <- theme(
  text = element_text(family = "Source Serif Pro"),
  panel.border = element_blank(),
  panel.grid = element_line(size = 1),
  plot.title = element_text(size = 16, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic"),
  axis.text.x = element_text(size = 10, margin=margin(0,0,20,0)),
  axis.text.y = element_text(size = 10, margin=margin(0,0,20,0))
)


API_KEY <- "TSIriKOxJ9uYTbUdjcieeIgkgwLAIXzdHXehW8Y3"

GET(sprintf("https://api.eia.gov/series/?api_key=%s&series_id=PET.EMM_EPM0_PTE_NUS_DPG.W", API_KEY)) %>% 
  content() -> avg_gas 

GET(sprintf("https://api.eia.gov/series/?api_key=%s&series_id=PET.RCLC1.W", API_KEY)) %>% 
  content() -> avg_barrel

convert_to_df <- function(stuff) {
  data <- stuff$series[[1]]$data
  result <- data.frame("date"=NA, "price"=NA)
  for (row in data) {
    date <- row[[1]][1]
    date <- str_c(str_sub(date, 1, 4), "-", str_sub(date, 5, 6), "-", str_sub(date, 7, 8))
    price <- row[[2]][1]
    price <- as.numeric(price)
    print(price)
    result <- rbind(result, list(date, price))
  }
  result <- result %>% tail(-1)
  return(result)
}

gas <- convert_to_df(avg_gas)
barrel <- convert_to_df(avg_barrel)

gas$date <- as.Date(gas$date)
barrel$date <- as.Date(barrel$date)

ggplot() +
  geom_line(data = barrel, aes(x = date, y = price)) +
  geom_line(data =gas, aes(x = date, y = price))

gas <- gas %>% 
  mutate(i = 1:n())

barrel <- barrel %>% 
  mutate(i = 1:n())

full <- left_join(gas, barrel, by = "i")

full <- full %>% 
  mutate(year = str_sub(date.x, 1, 4), what_year = ifelse(year >= 2020, year, "1993-2019"))

full_min <- full %>% 
  filter(what_year != "1993-2019") %>% 
  group_by(what_year) %>% 
  summarize(i = floor(median(i)))

full %>%
  ggplot() +
  geom_point(aes(x = price.y, y = price.x, color = what_year, alpha = what_year), size = 2) +
  geom_text_repel(data = full[full$i %in% full_min$i, ], aes(x = price.y, y = price.x, label = what_year, size = 0.5), nudge_y = 0.3, family = "Source Serif Pro", segment.colour = NA) +
  labs(x = "Price/Barrel ($)", y = "Price/Gallon ($)", title = "Consumers are paying a premium for gas recently", subtitle = "Weekly Gas Price vs. Barrel Price (Cushing, OK Crude Oil Future Contract) (1993-2022) \n * Each point represents a single week", caption = expression(paste("@swingmisstake | source: https://www.eia.gov/opendata/"))) +
  scale_color_manual(values = c("black", "#21465c", "#5a7baf", "#baacff")) +
  scale_alpha_manual(values = c(0.1, 1, 1, 1)) + 
  theme_minimal() +
  cons_theme +
  theme(legend.position = "none",
        title = element_text(size = 15),
        plot.title = element_text(size=22), 
        plot.subtitle = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")))

ggsave("~/desktop/projects/springviz/day1/viz.png", width = 10, height = 6, units = "in")

full <- full %>% 
  mutate(ratio = price.y / price.x)

full %>% 
  ggplot() +
  geom_line(aes(x = date.x, y = ratio))
