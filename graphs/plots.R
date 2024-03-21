library("tidyverse")
library(ggrepel)
library(ggthemes)
library("lubridate")
set.seed(42)
library(scales)
library(RColorBrewer)
library(zoo)
library("elementalist")
library(gridExtra)
library(patchwork)

setwd("code/l2_structure/graphs")

### uni light theme
colors <- c(
  "#FA3492", # uni pink
  "#A5AFFF", # pastel blue
  "#AD218E", # maroon
  "#5769FF", #navy blue
  "#CDD0E8", # light grey
  "#FFA6D1" # light pink
)

uni_black <- "#3D3D3D"
uni_lightgrey <- "#F1F1F1"
uni_grey <- "#D0D0D0"
uni_pink <- '#FA3492'
uni_darkgrey <- "#747474"
uni_white <- "#FFFFFF"
uni_black <- "#000000"

uni_theme <- theme(
  text = element_text(size = 8, family = 'Inter', color = "black"),
  rect = element_rect(fill = uni_white), # all rectangles
  axis.title = element_text(size = 15, color = uni_black, family = 'Inter'), 
  plot.title = element_text(size = 20, color = uni_darkgrey, family = 'Inter'),
  legend.title = element_text(size = 18, color = uni_black, family = 'Inter'),
  legend.text = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(l=-3, r = 4)),
  axis.text.x = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(t=10, b=10)),
  axis.text.y = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(l=10, r=10)),
  legend.box.background = element_blank(),
  
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(color = uni_darkgrey, size = rel(.25)),
  axis.ticks.length.x.bottom = unit(.5, "cm"),
  panel.background = element_blank(),
  plot.background = element_blank(),
  panel.spacing = unit(-1, "lines"),
  panel.grid.major.y = element_line(colour = uni_grey, size = rel(.25)),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position ="top", 
  legend.box = "horizontal",
  legend.margin = margin(5,5,5,5),
  legend.box.spacing = margin(b=20),
  legend.justification = 'right',
  legend.box.just = 'right',
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  plot.margin = margin(1, 1, 1, 1, unit = "cm")
)

## share of txs
df <- read_csv("data/share_of_txs.csv") %>% 
      mutate(Chain = factor(str_to_title(blockchain), levels = c("Polygon", "Optimism", 'Arbitrum', "Ethereum"))) %>% 
      mutate(dt = ymd_hms(dt)) %>% 
      group_by(dt) %>% 
      mutate(percentage = count_of_txs / sum(count_of_txs) * 100)


ggplot(df, aes(fill=Chain, y=percentage, x = dt)) +
  geom_area(position="stack", stat = "identity") +
  uni_theme +
  scale_fill_manual(values=c('#803ce4', '#ff0424', "#30344c", "#687cec")) +
  labs(x = "", 
       y = "Share of Daily Transactions (%)", 
       title = '')

ggsave("plots/share_of_txs.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

## share of vol
df <- read_csv("data/share_of_vol.csv") %>% 
  mutate(Chain = factor(str_to_title(blockchain), levels = c("Polygon", "Optimism", 'Arbitrum', "Ethereum"))) %>% 
  mutate(dt = ymd_hms(day)) %>% 
  mutate(total_usd_volume = total_usd_volume / 1e9) %>% 
  group_by(dt) %>% 
  mutate(percentage = total_usd_volume / sum(total_usd_volume) * 100) %>% 
  mutate(total_vol = sum(total_usd_volume))

ggplot(df, aes(fill=Chain, y=total_usd_volume, x = dt)) +
  geom_bar(position="stack", stat = "identity") +
  uni_theme +
  scale_fill_manual(values=c('#803ce4', '#ff0424', "#30344c", "#687cec")) +
  labs(x = "", 
       y = "Daily Volume ($bn)", 
       title = '')

ggsave("plots/level_of_vol.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

coeff <- 7
ggplot(df, aes(x = dt)) +
  geom_area(aes(y=percentage, fill=Chain), position="stack", stat = "identity") +
  #geom_line(aes(y=total_vol*coeff), color = uni_black) +
  #scale_y_continuous(
  #  name = "Share of Daily Volume (%)",
  #  sec.axis = sec_axis(~./coeff, name="Total Daily Volume")
  #) +
  uni_theme +
  scale_fill_manual(values=c('#803ce4', '#ff0424', "#30344c", "#687cec")) +
  labs(x = "", 
       y = "Share of Daily Volume (%)", 
       title = '')

ggsave("plots/share_of_vol.png", width = 9, height = 6, dpi = 300)#, dpi = 125)


# breakeven costs
df <- read_csv("data/breakeven.csv") %>% 
  drop_na() %>% 
  select('adjCost', 'date')

# breakeven set gas costs
df_be <- read_csv("data/breakeven_setgas.csv") %>% 
  drop_na() %>% 
  select('adjCost', 'date') %>% 
  rename(adjCostBE = adjCost)

df <- df %>% left_join(df_be)

ggplot(df, aes(x = date)) +
  geom_line(aes(y=adjCost), color = uni_pink) + 
  geom_line(aes(y=adjCostBE), color = uni_black, alpha = .4) + 
  scale_y_continuous(
    name = "Size of breakeven trade ($thousands)",
    sec.axis = sec_axis(~., name="Breakeven trade with median gas")
  ) +
  uni_theme +
  theme(
    axis.title.y = element_text(color = uni_pink, size=13),
    axis.title.y.right = element_text(color = uni_black, size=13, margin = margin(l=15, r=10)),
    axis.text.y.right = element_blank()
  ) +
  labs(x = "Date", 
       title = '')

ggsave("plots/breakeven_swap.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

## distribution

arb <- read_csv("data/arb.csv") %>% 
  group_by(tick_adj) %>% 
  summarize(liquidity = median(liquidity)) %>% 
  ungroup() %>% 
  mutate(symbol = "Arbitrum")

op <- read_csv("data/op.csv") %>% 
  group_by(tick_adj) %>% 
  summarize(liquidity = median(liquidity)) %>% 
  ungroup() %>% 
  mutate(symbol = "Optimism")

eth <- read_csv("data/eth.csv") %>% 
  group_by(tick_adj) %>% 
  summarize(liquidity = median(liquidity)) %>% 
  ungroup() %>% 
  mutate(symbol = "Ethereum")

df <- bind_rows(arb, op, eth) %>% 
  mutate(Chain = factor(str_to_title(symbol), levels = c("Optimism", 'Arbitrum', "Ethereum"))) %>% 
  mutate(tick_adj = tick_adj / 100)

ggplot(df, aes(x = tick_adj, y = liquidity, colour = Chain)) +
  geom_line(stat = 'identity', key_glyph = draw_key_rect) +
  uni_theme +
  scale_color_manual(values=c('#ff0424', "#30344c", "#687cec")) +
  labs(x = "Percentage away from mid", 
       y = "Normalized liquidity (%)",
       title = '')
  
ggsave("plots/normalized_liquidity.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

## lp fees
# ARB
arb <- read_csv("data/arbitrum_fees.csv")
arbMean <- arb %>% select('wethPct') %>% summarize(mean = mean(wethPct)) 
arbMean <- arbMean$mean
topArb <- 25

p1 <- ggplot(arb, aes(x=wethPct)) +
  geom_histogram(binwidth = 10, fill = "#AD218E") +
  geom_segment(data = arb, aes(x = arbMean, xend = arbMean, y = -Inf, yend = topArb), color = uni_black, linetype = "dashed", size = 1.5) +
  annotate("text", x = arbMean, y = topArb + .75, label = paste0("Mean: ", round(arbMean, 0), "%"),  size = 8) +
  labs(x = "Increase in daily L2 returns vs. L1 (%)", 
       y = "Count of days",
       title = 'Distribution of WETH returns') +
  uni_theme +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        plot.title = element_text(size = 34),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

otherMean <- arb %>% select('otherPct') %>% summarize(mean = mean(otherPct)) 
otherMean <- otherMean$mean
topOther <- 30

p2 <- ggplot(arb, aes(x=otherPct)) +
  geom_histogram(binwidth = 10, fill = "#A5AFFF") +
  geom_segment(data = arb, aes(x = otherMean, xend = otherMean, y = -Inf, yend = topOther), color = uni_black, linetype = "dashed", size = 1.5) +
  annotate("text", x = otherMean, y = topOther + .75, label = paste0("Mean: ", round(otherMean, 0), "%"), size = 8) +
  labs(x = "Increase in daily L2 returns vs. L1 (%)", 
       y = "Count of days",
       title = 'Distribution of USDC returns') +
  uni_theme +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        plot.title = element_text(size = 34),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

p1 + p2 

ggsave("plots/arb_fees.png", width = 9 * 2, height = 9, dpi = 300)#, dpi = 125)

# OP
arb <- read_csv("data/op_fees.csv")
arbMean <- arb %>% select('wethPct') %>% summarize(mean = mean(wethPct)) 
arbMean <- arbMean$mean
topArb <- 50

p1 <- ggplot(arb, aes(x=wethPct)) +
  geom_histogram(binwidth = 10, fill = "#AD218E") +
  geom_segment(data = arb, aes(x = arbMean, xend = arbMean, y = -Inf, yend = topArb), color = uni_black, linetype = "dashed", size = 1.5) +
  annotate("text", x = arbMean, y = topArb + .75, label = paste0("Mean: ", round(arbMean, 0), "%"), size = 8) +
  labs(x = "Increase in daily L2 returns vs. L1 (%)", 
       y = "Count of days",
       title = 'Distribution of WETH returns') +
  uni_theme +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        plot.title = element_text(size = 34),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

otherMean <- arb %>% select('otherPct') %>% summarize(mean = mean(otherPct)) 
otherMean <- otherMean$mean
topOther <- 42

p2 <- ggplot(arb, aes(x=otherPct)) +
  geom_histogram(binwidth = 10, fill = "#A5AFFF") +
  geom_segment(data = arb, aes(x = otherMean, xend = otherMean, y = -Inf, yend = topOther), color = uni_black, linetype = "dashed", size = 1.5) +
  annotate("text", x = otherMean, y = topOther + 1, label = paste0("Mean: ", round(otherMean, 0), "%"), size = 8) +
  labs(x = "Increase in daily L2 returns vs. L1 (%)", 
       y = "Count of days",
       title = 'Distribution of USDC returns') +
  uni_theme +
  theme(axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        plot.title = element_text(size = 34),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

p1 + p2 

ggsave("plots/op_fees.png", width = 9 * 2, height = 9, dpi = 300)#, dpi = 125)

