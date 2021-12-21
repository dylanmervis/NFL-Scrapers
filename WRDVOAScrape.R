url <- paste0("https://www.footballoutsiders.com/stats/nfl/wr/2006")

library(tidyverse)
library(rvest)
h <- read_html(url)
WR <- h %>% html_nodes("table")
WR[[1]]
WR <- WR[[1]] %>% html_table
class(WR)
WR <- WR %>% setNames(c("Player", "Team", "DYAR", "Rk", "YAR", "Rk", "DVOA", "Rk", "VOA", "Passes", "Yards",
                        "EYds", "TD", "Catch_Rate", "FUM", "DPI"))
WR <- WR %>%
  select(Player, Team, DYAR, YAR, DVOA, VOA, Passes, Yards, EYds, TD, Catch_Rate, FUM, DPI)
WR


WR_2 <- h %>% html_nodes("table")
WR_2[[2]]
WR_2 <- WR_2[[2]] %>% html_table
class(WR_2)
WR_2 <- WR_2 %>% setNames(c("Player", "Team", "DYAR", "YAR", "DVOA", "VOA", "Passes",
                            "Yards", "EYds", "TD", "Catch_Rate", "FUM", "DPI"))
WR_2 <- WR_2 %>%
  select(Player, Team, DYAR, YAR, DVOA, VOA, Passes, Yards, EYds, TD, Catch_Rate, FUM, DPI)

WR <- rbind(WR, WR_2)

WR_3 <- h %>% html_nodes("table")
WR_3[[3]]
WR_3 <- WR_3[[3]] %>% html_table
class(WR_3)
WR_3 <- WR_3 %>% setNames(c("Player", "Team", "R_DYAR", "Rk", "R_YAR", "Rk", "R_DVOA", "Rk", "R_VOA",
                            "Runs", "R_Yards", "R_TD", "R_FUM"))
WR_3 <- WR_3 %>%
  select(Player, R_DYAR, R_YAR, R_DVOA, R_VOA, Runs, R_Yards)
WR_3

WR <- left_join(WR, WR_3, by = "Player")
View(WR)
WR <- WR %>%
  slice(-1, -16, -31, -47, -62, -77, -93, -108, -123, -138)


#Change Year
write_csv(WR, "WR_DVOA_06.csv")