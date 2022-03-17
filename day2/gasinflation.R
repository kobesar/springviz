library(tidyverse)
library(httr)
library(rvest)

cons_theme <- theme(
  text = element_text(family = "Source Serif Pro"),
  panel.border = element_blank(),
  panel.grid = element_line(size = 0.5),
  plot.title = element_text(size = 16, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic"),
  axis.text.x = element_text(size = 15, margin=margin(0,0,20,0)),
  axis.text.y = element_text(size = 15, margin=margin(0,0,20,0))
)

API_KEY <- "665dd852aaab13eab30b3dc4eb70fa02"

url <- sprintf("https://api.stlouisfed.org/fred/series/observations?series_id=CORESTICKM159SFRBATL&api_key=%s&file_type=json", API_KEY)

inflation <- GET(url) %>% 
  content()

inflation_hist <- data.frame()

for (row in inflation$observations) {
  inflation_hist <- rbind(inflation_hist, c(row$date, row$value))  
}

colnames(inflation_hist) <- c("date", "value")

inflation_hist$date <- as.Date(inflation_hist$date)

inflation_hist$value <- as.numeric(inflation_hist$value)

inflation_hist %>% 
  ggplot() +
  geom_line(aes(x = date, y = value))

inflation_hist <- inflation_hist %>% 
  mutate(inc = value - lag(value))

inflation_hist %>% 
  ggplot() +
  geom_line(aes(x = date, y = inc))

inflation_hist %>% 
  mutate(year = str_sub(date, 1, 4)) %>% 
  group_by(year) %>% 
  summarize(value = mean(value)) %>% 
  mutate(inc = value - lag(value)) %>% 
  ggplot() +
  geom_line(aes(x = as.numeric(year), y = inc))

full %>% 
  ggplot() +
  geom_histogram(aes(x = price.x, fill = what_year))

ggplot() +
  geom_line(data = inflation_hist[as.numeric(str_sub(inflation_hist$date, 1, 4)) > 1993, ], aes(x = date, y = value, color = "#fdbf43"), size = 1, alpha = 0.25) +
  geom_line(data = full, aes(x = date.x, y = price.x, color = "#89c07b"), size = 0.8) + 
  scale_y_discrete(limits = c("$1", "$2", "$3", "$4")) +
  scale_color_manual(name = '', 
                      values = c('#fdbf43'='#fdbf43','#89c07b'='#89c07b'), labels = c('Gas Price','Inflation')) + 
  labs(x = "Date", y = "Price/Gallon ($)", title = "Gas Prices Increase With Rising Inflation", subtitle = "Inflation here is represented by CPI of goods that don't experience frequent price change (possibly factoring in future movement)", caption = "@swingmisstake | source: FRED") +
  theme_minimal() +
  cons_theme +
  theme(legend.position = c(0.1, 0.9),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")))

ggsave("~/desktop/projects/springviz/day2/viz.png", width = 10, height = 6, units = "in")
