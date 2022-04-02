library(tidyverse)
library(scales)

cons_theme <-
  theme_minimal() + theme(
    text = element_text(family = "Titillium Web"),
    panel.border = element_blank(),
    panel.grid = element_line(size = 0.5),
    plot.title = element_text(size = 24, margin = margin(30,0,0,0)),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(size = 15, margin=margin(20,0,0,0)),
    axis.title.y = element_text(size = 15, margin=margin(0,20,20,0))
  )

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

biblenames <- read.csv("https://mdbeckman.github.io/dcSupplement/data/BibleNames.csv")

babynames %>% 
  mutate(end_a = str_sub(name, -1) == "a") %>% 
  group_by(year, sex) %>% 
  summarize(prop_a = mean(end_a, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = prop_a))

babynames$is_bible <- babynames$name %in% biblenames$name
  
babynames %>% 
  group_by(year, sex) %>% 
  summarize(bible = sum(prop, na.rm = T)) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = bible, fill = sex), stat = "identity", position = "dodge")

babynames %>% 
  group_by(year) %>% 
  summarize(len = mean(str_length(name))) %>% 
  ggplot() +
  geom_line(aes(x = year, y = len))

df1 <- babynames %>% 
  group_by(year, sex) %>% 
  summarize(bible = sum(prop, na.rm = T))

df2 <- babynames %>% 
  group_by(year) %>% 
  summarize(len = mean(str_length(name)))

df1 %>% 
  left_join(df2, by = "year") %>%
  ggplot() +
  geom_point(aes(x = len, y = bible, color = sex))

babynames %>% 
  group_by(year, sex) %>% 
  summarize(bible = sum(prop, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = bible, color = sex)) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("#d07878", "#3e7fa4"), name = "", labels = c("Female", "Male")) +
  labs(title = "Percentage Of American Babies Whose Names Are From The Bible",
       subtitle = "Though most names continue to be from the bible (>90%), it has seen constant decline since the 1960s",
       x = "Year",
       y = "",
       caption = expression(paste("@swingmisstake | Data: ", italic("babynames"), " - Hadley Wickam"))) +
  cons_theme +
  theme(legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, "cm"),
        panel.background = element_rect(fill = "#d4d4d4", color = "#d4d4d4"),
        plot.background = element_rect(fill = "#d4d4d4"),
        panel.grid = element_line(color = "#bcbcbc"))

ggsave("~/desktop/projects/springviz/day6/babies.png", width = 16, height = 10, units = "in")
