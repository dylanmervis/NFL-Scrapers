headers <- c(
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.105 Safari/537.36',
  `Accept` = '*/*',
  `Accept-Language` = 'en-US,en;q=0.5',
  `Origin` = 'https://ibet.ag',
  `Connection` = 'keep-alive',
  `Referer` = 'https://ibet.ag/lines.html',
  `TE` = 'Trailers'
)
params <- list(`lg` = '1')
res <-
  httr::POST(url = 'https://qar.ibet-host.info/LiveLines.aspx',
             httr::add_headers(.headers = headers),
             query = params)

table <- httr::content(res) %>% 
  rvest::html_table(fill=TRUE,header = TRUE) %>% 
  purrr::pluck(1) %>% 
  janitor::clean_names() %>% 
  dplyr::select(-x) %>% 
  dplyr::slice(-1)


afc_north_odds <- odds.us2dec(c(550, -1000, 2200, 100000))
colnames(afc_north_odds) <- c("BAL", "PIT", "CLE", "CIN")

afc_west_odds <- odds.us2dec(c(-2000, 25000, 25000, 600))
colnames(afc_west_odds) <- c("KC", "LAC", "DEN", "LV")

res1 <- implied_probabilities(afc_north_odds, method = 'power')
res1$probabilities


## implied package
implied
## oddsconverter package
oddsconverter


## Don Best Scraper
library(xml2)
library(rvest)
library(jsonlite)
library(purrr)
library(dplyr)

url_DB <- "https://www.donbest.com/api/events"

raw_DB <- fromJSON(url_DB, flatten = TRUE) %>%
  pluck("events") %>%
  select(date, league_abbr = league.abbreviation, away_rot = away.id, away_name = away.name,
         away_abbr = away.team.abbreviation, home_rot = home.id, home_name = home.name,
         home_abbr = home.team.abbreviation, odds.spreads, odds.money, odds.totals, odds.sides,
         openOdds.spreads, openOdds.money, openOdds.totals, openOdds.sides, closeOdds.spreads,
         closeOdds.money, closeOdds.totals, closeOdds.sides) %>%
  filter(!is.na(away_name)) 


