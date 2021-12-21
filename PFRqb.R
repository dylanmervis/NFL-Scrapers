source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
library(ggthemes)
library(ggrepel)
library(scales)
library(rvest)

# scrape data from PFR
url <- "https://www.pro-football-reference.com/years/2019/passing_advanced.htm"
pfr_raw <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame()

# clean the scraped data
colnames(pfr_raw) <- make.names(pfr_raw[1,], unique = TRUE, allow_ = TRUE)
pfr <- pfr_raw %>%
  slice(-1) %>%
  select(Player, Tm, IAY.PA, Bad., Att) %>%
  rename(team = Tm) %>%
  mutate(
    Player = str_replace(Player, "\\*", ""),
    Player = str_replace(Player, "\\+", ""),
    IAY.PA = as.numeric(IAY.PA),
    Bad. = as.numeric(str_replace(Bad., "%", "")),
    Passattempts = as.numeric(Att)
  ) %>%
  apply_colors_and_logos() %>%
  filter(Passattempts>180) %>%
  arrange(Bad.)

# create the plot
pfr %>%
  ggplot(aes(x = IAY.PA, y = Bad./100)) +
  geom_hline(aes(yintercept = mean(Bad./100)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept =  mean(IAY.PA)), color = "red", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color="black", size=0.3) +
  geom_point(color = pfr$use_color, aes(cex=Passattempts), alpha=1/4) +
  geom_text_repel(aes(label=Player), force=1, point.padding=0, segment.size=0.1) +
  scale_y_continuous(labels=percent) +
  scale_size_area(max_size = 8) +
  labs(x = "Average Depth of Target in Yards",
       y = "Bad Throw Percentage",
       caption = "Bad% = Percentage of throws that weren't catchable excluding throwaways",
       title = 'QB Passing Performance 2019'+
  theme_stata() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.position = "right"))
pfr