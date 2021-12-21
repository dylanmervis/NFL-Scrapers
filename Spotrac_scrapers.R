  ## Data Import/Export
  library(here)
  
  ## Scraping
  library(rvest)
  library(httr)
  
  ## Processing
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(janitor)
  
  # Signalling
  library(glue)

  
## SCRAPING TEAM TABLE FOR A GIVEN YEAR ------------------------------------------------------------------------------
  
  base_url <- "http://www.spotrac.com/nfl"
  
  team_names <- c("arizona-cardinals", "atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers",
                  "chicago-bears", "cincinnati-bengals", "cleveland-browns", "dallas-cowboys", "denver-broncos", 
                  "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars",
                  "kansas-city-chiefs", "los-angeles-chargers", "los-angeles-rams", "las-vegas-raiders",
                  "miami-dolphins", "minnesota-vikings", "new-orleans-saints", "new-england-patriots",
                  "new-york-giants", "new-york-jets", "philadelphia-eagles", "pittsburgh-steelers", 
                  "san-francisco-49ers", "seattle-seahawks", "tampa-bay-buccaneers", "tennessee-titans",
                  "washington-football-team")
  year <- 2019
  cap <- 'cap'
  
  # Clean up the URLs to get the team names by themselves.
  team_url <- paste(base_url, team_names, cap, year, sep = "/")
  
  
  dfList <- lapply(team_url, function(i) {
    webpage <- read_html(i, handle = curl::new_handle("useragent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36")) %>%
      html_nodes("table")
   
    # Select the first table
    team_table <- webpage[[1]] %>% html_table() %>%
      setNames(c("Name", "Position", "Base_Salary", "Signing_Bonus", "Roster_Bonus", "Option_Bonus", 
                 "Workout_Bonus", "Restruc_Bonus", "Misc", "Dead_Cap", "Cap_Hit", "Cap_Perc", "Misc2"))
    
    # Drop last column
    team_table <- team_table[-1,]
    
    # Fix Player Names & Bind
    Player <- sub('.*  ', '', team_table$Name)
    team_table <- cbind(Player, team_table) %>%
      dplyr::select(-Name, -Misc, -Misc2) %>%
      head(-1)
  })
  
  finaldf <- do.call(rbind, dfList) %>%
    mutate(Season = year)
  
  write_csv(finaldf, "/Users/dylanmervis/Desktop/UVA/Module 2/7206 - Leadership I - Management Comms/Individual Assignment/nfl_contracts_18.csv")
  
  
  
## SCRAPING TOP 2000 CONTRACT ------------------------------------------------------------------------------------------
  
  spotrac_url <- "https://www.spotrac.com/nfl/contracts/sort-value/limit-2000/"
  
  spotrac_html <- read_html(spotrac_url) %>% 
    html_element(".teams")
  
  spotrac_details <- tibble(
    player_name = spotrac_html %>% html_elements(".rank-name .team-name") %>% html_text(trim = TRUE),
    player_url = spotrac_html %>% html_elements(".rank-name .team-name") %>% html_attr("href"),
    position = spotrac_html %>% html_elements('.rank-name .rank-position') %>% html_text2() %>% magrittr::extract(str_detect(.,"\\d|Originally",negate = TRUE)),
    duration = spotrac_html %>% html_elements('.rank-name .rank-position') %>% html_text2() %>% magrittr::extract(str_detect(.,"\\d",)),
  ) %>% 
    mutate(
      position = str_remove_all(position, "\\|") %>% str_squish()
    ) %>% 
    tidyr::extract(
      duration,
      into = c("contract_start_year","contract_end_year","fa_year"),
      regex = "^(\\d+)-(\\d+) \\(FA: (\\d+)\\)$"
    )
  
  df_spotrac <- spotrac_html %>% 
    html_table() %>% 
    clean_names() %>% 
    select(-rank,-player,-yrs) %>% 
    bind_cols(spotrac_details,.)
  
  
  
## SCRAPING YEARLY CAP FIGURES ---------------------------------------------------------------------------------------

  base_url <- "http://www.spotrac.com/nfl"
  
  team_names <- c("arizona-cardinals", "atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers",
                  "chicago-bears", "cincinnati-bengals", "cleveland-browns", "dallas-cowboys", "denver-broncos", 
                  "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars",
                  "kansas-city-chiefs", "los-angeles-chargers", "los-angeles-rams", "las-vegas-raiders",
                  "miami-dolphins", "minnesota-vikings", "new-orleans-saints", "new-england-patriots",
                  "new-york-giants", "new-york-jets", "philadelphia-eagles", "pittsburgh-steelers", 
                  "san-francisco-49ers", "seattle-seahawks", "tampa-bay-buccaneers", "tennessee-titans",
                  "washington-football-team")
  
  cap <- 'yearly/cap/roster'
  
  # Clean up the URLs to get the team names by themselves.
  team_url <- paste(base_url, team_names, cap, sep = "/")
  
  
  dfList <- lapply(team_url, function(i) {
    webpage <- read_html(i, handle = curl::new_handle("useragent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36")) %>%
      html_nodes("table")
    
    # Select the first table
    team_table <- webpage[[2]] %>% html_table() %>%
      setNames(c("Name", "Position", "year_2021", "year_2022", "year_2023", "year_2024", "year_2025"))
    
    
    # Fix Player Names & Bind
    Player <- sub('.*  ', '', team_table$Name)
    team_table <- cbind(Player, team_table) %>%
      dplyr::select(-Name) %>%
      head(-1)
  })
  
  finaldf <- do.call(rbind, dfList) 
  
  write_csv(finaldf, "/Users/dylanmervis/Desktop/UVA/Module 2/7206 - Leadership I - Management Comms/Individual Assignment/nfl_contracts_by_year.csv")
  
  
  
## SCRAPING CONTRACT TERMS ------------------------------------------------------------------------------------------
  
  base_url <- "http://www.spotrac.com/nfl"
  
  team_names <- c("arizona-cardinals", "atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers",
                  "chicago-bears", "cincinnati-bengals", "cleveland-browns", "dallas-cowboys", "denver-broncos", 
                  "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars",
                  "kansas-city-chiefs", "los-angeles-chargers", "los-angeles-rams", "las-vegas-raiders",
                  "miami-dolphins", "minnesota-vikings", "new-orleans-saints", "new-england-patriots",
                  "new-york-giants", "new-york-jets", "philadelphia-eagles", "pittsburgh-steelers", 
                  "san-francisco-49ers", "seattle-seahawks", "tampa-bay-buccaneers", "tennessee-titans",
                  "washington-football-team")
  
  cap <- 'contracts'
  
  # Clean up the URLs to get the team names by themselves.
  team_url <- paste(base_url, team_names, cap, sep = "/")
  
  
  dfList <- lapply(team_url, function(i) {
    webpage <- read_html(i, handle = curl::new_handle("useragent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36")) %>%
      html_nodes("table")
    
    # Select the first table
    team_table <- webpage[[1]] %>% html_table() %>%
      setNames(c("Name", "Position", "Age", "Experience", "Contract_Terms", "Avg_Salary", "Guaranteed", 
                 "Expires", "Acquired"))
    
    
    # Fix Player Names & Bind
    Player <- sub("^.*?([A-Z])", "\1", team_table$Name)
    Player <- sub("^.*?([A-Z])", "\\1", Player)
    team_table <- cbind(Player, team_table) %>%
      dplyr::select(-Name)
  })
  
  finaldf <- do.call(rbind, dfList) 

  
  write_csv(finaldf, "/Users/dylanmervis/Desktop/UVA/Module 2/7206 - Leadership I - Management Comms/Individual Assignment/nfl_contract_terms.csv")
  