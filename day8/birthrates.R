library(tidyverse)
library("httr")
library("readxl")
GET("https://query.data.world/s/gdmk2vkoqs4rp2dcojruidz2qbjdvg", write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf)

GET("https://query.data.world/s/uhm6tdjngk4ijrh4ayduz7qxhto6h5", write_disk(tf <- tempfile(fileext = ".xls")))
lifexp <- read_excel(tf)

df %>% 
  ggplot() +
  geom_histogram(aes(x = `2013`))

birth_rate <- read.csv("birthrates.csv")
