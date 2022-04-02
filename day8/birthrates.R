library(tidyverse)

brate <- read.csv("birthrates.csv") %>% 
  pivot_longer(
    cols = str_c("X", c(1960:2020)),
    names_to = "year",
    values_to = "rate"
  )

# My theme
  cons_theme <-
  theme_minimal() + theme(
    text = element_text(family = "Titillium Web"),
    panel.border = element_rect(color = bg_color),
    panel.grid = element_line(size = 0.5, color = "#b9b9b9"),
    plot.title = element_text(size = 24, margin = margin(30,0,0,0)),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(size = 15, margin=margin(20,0,0,0)),
    axis.title.y = element_text(size = 15, margin=margin(0,20,20,0)),
    plot.background = element_rect(fill = bg_color),
    panel.background = element_rect(fill = bg_color)
    )
# Main plot
brate %>% 
  filter(!is.na(rate)) %>% 
  mutate(year = str_remove(year, "X")) %>% 
  ggplot() +
  geom_hex(aes(x = as.Date(year, format = "%Y"), y = rate)) +
  ylim(c(0, 65)) +
  scale_fill_viridis() +
  annotate("segment", x = as.Date("1985", format = "%Y"), xend = as.Date("2010", format = "%Y"), y = 42, yend = 38, size = 1, alpha = 0.3, arrow=arrow(type = "closed")) +
  annotate("segment", x = as.Date("1985", format = "%Y"), xend = as.Date("2010", format = "%Y"), y = 42, yend = 22, size = 1, alpha = 0.3, arrow=arrow(type = "closed")) +
  labs(x = "", y = "Birth Rate", fill = "# of Countries", title = "Birth Rates Have Been Going Down, What's Next?",
           subtitle = "A subtle split can be seen in this plot, where some countries held a steady decreasing pace while some were faced with faster decreasing pace (shown by the arrows)", caption = "@swingmisstake | Data Source: World Bank") +
  cons_theme +
  theme(axis.text.x = element_text(size = 14),
            legend.direction = "horizontal",
            legend.position = c(0.85, 0.92),
            legend.title = element_text(size = 16, margin = margin(0, 0, 12, 0), face = "italic"))