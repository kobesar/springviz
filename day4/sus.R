library(tidyverse)
library(timeDate)
library(patchwork)
library(ggrepel)

cons_theme <-
  theme_minimal() + theme(
  text = element_text(family = "Source Serif Pro"),
  panel.border = element_blank(),
  panel.grid = element_line(size = 0.5),
  plot.title = element_text(size = 24, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic", margin = margin(10, 0, 10, 0)),
  axis.title.x = element_text(size = 15, margin=margin(20,0,0,0)),
  axis.title.y = element_text(size = 15, margin=margin(0,20,20,0))
)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom$Status <- factor(freedom$Status, levels = c("F", "PF", "NF"))

url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

covid <- read.csv(url)

covid$date <- as.Date(covid$date)

side1 <- covid %>% 
  mutate(group = paste0(strftime(date, format = "%V"), "-", strftime(date, "%Y"))) %>% 
  group_by(location, group) %>% 
  summarize(mean = mean(new_deaths, na.rm = T), variance = var(new_deaths, na.rm = T), sus = variance < mean) %>% 
  group_by(location) %>% 
  summarize(mean = mean(mean, na.rm = T), variance = var(variance, na.rm = T), sus = mean(sus, na.rm = T)) %>% 
  left_join(freedom[freedom$year == 2020, ], by = c("location" = "country")) %>%
  filter(!is.na(Status)) %>% 
  ggplot() +
  geom_boxplot(aes(x = sus, y = Status, fill = Status), alpha = 0.5, width = 0.3) +
  scale_fill_manual(name = "", labels = c("Free", "Partially Free", "Not Free"), values = c("#488f31", "#ffe9ac", "#de425b")) +
  scale_y_discrete(labels = c("Free", "Partially Free", "Not Free")) +
  labs(y = "", x = "Sus Meter", subtitle = "Sus Meter by freedom value of country") +
  cons_theme + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, face = "italic"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(80, 0, 10, 0)))

main <- covid %>% 
  filter(location %in% c("United States", "United Kingdom", "Russia")) %>% 
  ggplot() +
  geom_line(aes(x = date, y = new_deaths, color = location, group = location, alpha = ifelse(location == "Russia", 1, 0.2))) +
  scale_color_manual(name = "", values = c("#003f5c", "#bc5090", "#ffa600")) +
  scale_alpha_identity(aesthetic = "alpha") +
  labs(x = "Date", y = "New Deaths", title = "COVID-19: Analyzing Mean and Variance of Weekly New Death Counts", 
       subtitle = "Russia's counts do not vary as much as the United State's counts. This is easily seen in the huge up and down movement in the counts representing the United States (yellow) whilst Russia's (navy) \n counts stay relatively stable. The Sus Meter is the proportion of weeks a country's variance was lower than the mean, signaling there is some weird trend in the reporting of new deaths.") +
  cons_theme + 
  theme(legend.position = c(0.15, 0.8),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        plot.subtitle = element_text(margin = margin(-20, 0, 0, 0)))

tab <- covid %>% 
  mutate(group = paste0(strftime(date, format = "%V"), "-", strftime(date, "%Y"))) %>% 
  group_by(location, group) %>% 
  summarize(mean = mean(new_deaths, na.rm = T), variance = var(new_deaths, na.rm = T), sus = variance < mean) %>% 
  group_by(location) %>% 
  summarize(mean = mean(mean, na.rm = T), variance = var(variance, na.rm = T), sus = mean(sus, na.rm = T)) %>% 
  filter(!is.na(sus) & sus > 0) %>% 
  arrange(sus)

side2 <- tab %>% 
  ggplot() +
  geom_bar(aes(x = sus, y = reorder(location, sus), fill = location %in% c("United States", "United Kingdom", "Russia"), alpha = ifelse(location %in% c("United States", "United Kingdom", "Russia"), 1, 0.2)), stat = "identity", width = 0.4) +
  geom_label_repel(data = tab[tab$location %in% c("United States", "United Kingdom", "Russia"), ], aes(x = sus, y = location, label = location), nudge_x = 0.05) +
  scale_alpha_identity(aesthetic = "alpha") +
  scale_fill_manual(values = c("#003f5c", "#bc5090")) +
  scale_y_discrete(labels = NULL) +
  labs(x = "", y = "", subtitle = "Sus Meter of Each Country", caption = "@swingmisstake | Source: OWID, Freedom House, United Nations") +
  cons_theme +
  theme(panel.grid = element_line(size = 0),
        legend.position = "none")

plt <- main + (side1 / side2)

main + side1

side1 / side2

ggsave("~/desktop/projects/springviz/day4/deathsus.png", width = 16, height = 10, units = "in")
