library(crypto2)
library(ggtext)
library(tidyverse)

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

cryptos_list <- c("BTC", "ETH", "LTC", "BTH", "XLM", "LINK", "DFI", "UNI")

logos <- c("https://cryptologos.cc/logos/uniswap-uni-logo.png", "https://cryptologos.cc/logos/defichain-dfi-logo.png", "https://cryptologos.cc/logos/bitcoin-cash-bch-logo.png", "https://cryptologos.cc/logos/stellar-xlm-logo.png", "https://cryptologos.cc/logos/litecoin-ltc-logo.png", "https://cryptologos.cc/logos/ethereum-eth-logo.png", "https://cryptologos.cc/logos/chainlink-link-logo.png")

cryptos <- crypto_list() %>% 
  filter(symbol %in% cryptos_list)

crypto_prices <- crypto_history(cryptos, start_date="20100101", end_date="20220324")

crypto_prices <- crypto_prices %>% 
  mutate(date = as.Date(strftime(timestamp, format = "%Y-%m-%d"))) 

cryptos_notbtc <- crypto_prices %>% 
  filter(symbol != "BTC")

btc <- crypto_prices %>% 
  filter(symbol == "BTC") %>% 
  select(date, close) %>% 
  rename("closebtc" = "close")

cryptos_notbtc <- left_join(cryptos_notbtc, btc, by = "date")

df <- cryptos_notbtc %>% 
  group_by(symbol) %>% 
  summarize(r = cor(close, closebtc), sample = n()) %>% 
  arrange(r)

df$symbol <- factor(df$symbol, levels = df$symbol)

df$logo <- logos

df <- df %>%
  mutate(logo = paste0("<img src=", logo, "/>"))

labels <- setNames(paste0("<img src='", logos, "'style='width=5%; height=5%'/>"), df$symbol)

df %>% 
  ggplot() +
  geom_bar(aes(x = symbol, y = r, fill = symbol), stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("#FF007A", "#ff00af", "#8dc351", "black", "#d3d3d3", "#4D7DBF", "#215CAF")) +
  scale_x_discrete(labels = labels) +
  labs(x = "", y = "") +
  cons_theme +
  theme(legend.position = "none",
        axis.text.x = element_markdown(color = "black", size = 7))

dfi_btc <- right_join(crypto_prices[crypto_prices$symbol == "BTC", ], crypto_prices[crypto_prices$symbol == "DFI", ], by = "date")

dfi_btc %>%
  group_by(date) %>% 
  summarize(btc = mean(close.x, na.rm = T), dfi = mean(close.y, na.rm = T)) %>% 
  mutate(ratio = btc / dfi) %>% 
  ggplot() +
  geom_point(aes(x = date, y = ratio))
