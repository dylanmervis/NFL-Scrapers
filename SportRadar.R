library(tidyverse)
library(jsonlite)
library(purrr)
library(furrr)
key = "src7r6s6q79jb6ru3zxfxumm" #dylanmervis
key = "exx57d227kzn5fartb9apcdg" #ajlollis500
key = "n968h4e9d773p36bgtddjwrc" #mattmervis
key = "f5m37tr8eecukxmqehtaqa64" #dylansmervis

## SPORT RADAR SCHEDULE DATA

url <- glue::glue("http://api.sportradar.us/nfl/official/trial/v7/en/games/2018/REG/schedule.json?api_key={key}")
request <- httr::GET(url)

games <- request %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)

weeks <- games$weeks$games

all <- map_df(1 : 17, function(x) {
  
  weeks[[x]] %>%
    as_tibble() %>%
    mutate(week = x)
  
})

all %>%
  saveRDS("/Users/dylanmervis/Google Drive/schedule_2018.rds")


## 2020 Schedule - Use Game Id's to assist scraping PBP and participation data
sched_2018 <- readRDS("/Users/dylanmervis/Google Drive/schedule_2018.rds") %>%
  dplyr::filter(status == "closed") %>%
  dplyr::select(id, home = home.alias, away = away.alias, home.game_number, away.game_number, scheduled, week) %>%
  mutate(game_date = as.Date(substr(scheduled, 1, 10))) %>% 
  # filter(game_date <= lubridate::today()) %>%
  filter(week <= 17)


# dependencies
library(RCurl)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyverse)


## link to SportRadar single game pbp raw JSON
url_json_pbp <- glue::glue("http://api.sportradar.us/nfl/official/trial/v7/en/games/0822b924-eadc-4398-bfe6-83cbbf3a2912/pbp.json?api_key={key}")
raw_json_pbp <- jsonlite::fromJSON(url_json_pbp, flatten = TRUE)
raw_json_pbp %>%
  saveRDS("/Users/dylanmervis/Desktop/SportRadar PBP/2018/0822b924-eadc-4398-bfe6-83cbbf3a2912.rds")

scraped_games <- list.files("/Users/dylanmervis/Desktop/SportRadar PBP/2018") %>%
  tibble::as_tibble() %>%
  dplyr::rename(
    id = value
  ) %>%
  dplyr::mutate(
    id = substr(id, 1, (nchar(id) - 4))
  )

#figure out what games to scrape
server_ids <- unique(scraped_games$id)
finished_ids <-unique(sched_2018$id)

need_scrape <- sched_2018[!finished_ids %in% server_ids,]
message(glue::glue("Already have {length(server_ids)} games; missing {nrow(need_scrape)} games"))

for (j in 1 : nrow(need_scrape)) {
  
  message(glue::glue("Getting game {j} of {nrow(need_scrape)}"))
  
  url <- glue::glue("http://api.sportradar.us/nfl/official/trial/v7/en/games/{need_scrape %>% dplyr::slice(j) %>% pull(id)}/pbp.json?api_key={key}")
  request <- httr::GET(url)
  
  game <- request %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  if (length(game$periods$pbp[[1]]$events) > 0) {
    
    # message(glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/2018/{need_scrape %>% dplyr::slice(j) %>% pull(id)}.rds"))
    saveRDS(game, file = glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/2018/{need_scrape %>% dplyr::slice(j) %>% pull(id)}.rds"))
    
  }
  
  Sys.sleep(1)
  
}

## Clean all the games that we have in SportRadar PBP
  
  scraped_games <- list.files("/Users/dylanmervis/Desktop/SportRadar PBP/2018") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  
  get_game <- function(scraped_games, i) {
    
    game <- readRDS(glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/2018/{scraped_games %>% dplyr::slice(i) %>% pull(id)}"))
    
    home <- game$summary$home$alias
    away <- game$summary$away$alias
    season <- game$summary$season$year
    week <- game$summary$week$sequence
    
    pbp <- game %>%
      pluck("periods", "pbp") %>%
      bind_rows() %>%
      as_tibble() %>%
      pull(events) %>%
      bind_rows() %>%
      as_tibble() %>%
      dplyr::select(
        reference, description, sequence, screen_pass, play_action, run_pass_option, statistics, details, 
        players_rushed, men_in_box, play_blitz = blitz, play_direction, left_tightends, right_tightends,
        pocket_location, qb_at_snap, pass_route, running_lane
      ) %>%
      unnest_wider(statistics) %>%
      unnest(cols = stat_type : att_yards) %>%
      dplyr::rename(
        existing_description = description,
        existing_yards = yards,
        play_sequence = sequence
      ) %>%
      dplyr::select(
        reference, play_sequence, existing_description, screen_pass, run_pass_option, play_action, players_rushed,
        men_in_box, left_tightends, right_tightends, pocket_location, qb_at_snap, pass_route, running_lane,
        play_direction, missed_tackles, play_blitz = blitz, hurry, knockdown, incompletion_type,
        batted_pass, pocket_time, on_target_throw, dropped, catchable, pass_defended, yards_after_contact
      ) %>%
      unnest(cols = c(
        on_target_throw, dropped, catchable, incompletion_type, missed_tackles, pass_route, qb_at_snap,
        men_in_box, batted_pass, hurry, play_blitz, pocket_time, pass_defended
      )) %>%
      dplyr::select(
        reference, play_sequence, existing_description, screen_pass, play_action, run_pass_option,
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz, players_rushed,
        play_direction, left_tightends, right_tightends, pocket_time,
        on_target_throw, dropped, catchable, incompletion_type, missed_tackles, pass_route, qb_at_snap,
        men_in_box, batted_pass, hurry, yards_after_contact
      ) %>%
      group_by(reference) %>%
      summarize(
        sr_desc = dplyr::first(existing_description),
        sequence = dplyr::first(play_sequence),
        play_action = if_else(dplyr::first(na.omit(play_action)), 1, 0),
        screen_pass = if_else(dplyr::first(na.omit(screen_pass)), 1, 0), 
        run_pass_option = if_else(dplyr::first(na.omit(run_pass_option)), 1, 0), 
        blitz = dplyr::first(play_blitz), 
        hurry = max(na.omit(hurry)), 
        batted_pass = max(na.omit(batted_pass)), 
        incompletion_type = dplyr::first(na.omit(incompletion_type)),
        on_target_throw = dplyr::first(na.omit(on_target_throw)), 
        dropped = dplyr::first(na.omit(dropped)), 
        catchable = dplyr::first(na.omit(catchable)), 
        pocket_location = dplyr::first(na.omit(pocket_location)), 
        players_rushed = dplyr::first(na.omit(players_rushed)),
        men_in_box = dplyr::first(na.omit(men_in_box)),
        left_tightends = dplyr::first(na.omit(left_tightends)),
        right_tightends = dplyr::first(na.omit(right_tightends)),
        play_direction = dplyr::first(na.omit(play_direction)),
        pocket_time = dplyr::first(na.omit(pocket_time)),
        pass_route = dplyr::first(na.omit(pass_route))
      ) %>%
      ungroup() %>%
      dplyr::rename(play_id = reference) %>%
      mutate(
        game_id = 
          glue::glue('{season}_{formatC(week, width=2, flag=\"0\")}_{away}_{home}'),
        hurry = if_else(hurry == -Inf, NA_real_, hurry),
        batted_pass = if_else(batted_pass == -Inf, NA_real_, batted_pass)
      ) %>%
      arrange(sequence)
    
    return(pbp)
    
  }
  
all_games <- future_map_dfr(1 : nrow(scraped_games), function(x) {
    
    message(glue::glue("{x}"))
    get_game(scraped_games, x)
    
  })
  
  
all_games %>%
  saveRDS("/Users/dylanmervis/Google Drive/pbp_2018.rds")
  
  
  
  
## link to SportRadar single game participation raw JSON 
  url_json_participation <- glue::glue("http://api.sportradar.us/nfl/official/trial/v5/en/plays/018556f9-1977-4a0b-8244-20cad15df9a4/participation.json?api_key={key}")
  raw_json_participation <- fromJSON(url_json_participation)
  raw_json_participation %>%
    saveRDS("/Users/dylanmervis/Desktop/SportRadar Participation/2019/018556f9-1977-4a0b-8244-20cad15df9a4.rds")
  
  scraped_games <- list.files("/Users/dylanmervis/Desktop/SportRadar Participation") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    ) %>%
    dplyr::mutate(
      id = substr(id, 1, (nchar(id) - 4))
    )
  
  #figure out what we need
  server_ids <- unique(scraped_games$id)
  finished_ids <-unique(sched_2020$id)
  
  need_scrape <- sched_2020[!finished_ids %in% server_ids,]
  message(glue::glue("Already have {length(server_ids)} games; missing {nrow(need_scrape)} games"))
  
  
  for (j in 1 : nrow(need_scrape)) {
    
    message(glue::glue("Getting game {j} of {nrow(need_scrape)}"))
    
    url <- paste0("http://api.sportradar.us/nfl/official/trial/v5/en/plays/",need_scrape %>% dplyr::slice(j) %>% pull(id),"/participation.json?api_key=",key)
    request <- httr::GET(url)
    
    game <- request %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    # only save if there's actually data
    if (length(game$plays) > 0) {
      # message(glue::glue("/Users/dylanmervis/Desktop/SportRadar Participation/{need_scrape %>% dplyr::slice(j) %>% pull(id)}.rds"))
      saveRDS(game, file = glue::glue("/Users/dylanmervis/Desktop/SportRadar Participation/{need_scrape %>% dplyr::slice(j) %>% pull(id)}.rds"))
    }
    
    
    Sys.sleep(1)
  }
  
  
  ## Clean Participation Data
  scraped_games <- list.files("/Users/dylanmervis/Desktop/SportRadar Participation") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    )
  
  get_game <- function(i) {
    
    game <- readRDS(glue::glue("/Users/dylanmervis/Desktop/SportRadar Participation/{scraped_games %>% dplyr::slice(i) %>% pull(id)}"))
    
    if (length(game$plays) > 0) {
      
      plays <- game$plays
      
      game_players <- map_df(1 : nrow(plays), function(j) {
        
        row = plays %>% dplyr::slice(j)
        
        tibble::tibble(
          play_id = row$sequence,
          time = row$clock,
          desc = row$description,
          home_players = row$home.players,
          away_players = row$away.players,
          # for easy searching
          home_names = list(row$home.players[[1]]$name) %>% paste(),
          away_names = list(row$away.players[[1]]$name) %>% paste()
          
        ) %>%
          return()
        
      })
      
      game_players %>%
        # unnest(cols = c(home_players, away_players), names_repair = "universal") %>%
        # dplyr::select("play_id", "time", "desc", "home_name" = "name...5", "away_name" = "name...11") %>%
        # nest(home = home_name, away = away_name) %>%
        mutate(
          home_team = game$summary$home$alias,
          away_team = game$summary$away$alias,
          week = game$summary$week$sequence,
          season = game$summary$season$year    
        ) %>%
        return()
      
    } else {
      message(glue::glue("Nothing for {game$summary$away$alias} @ {game$summary$home$alias} week {game$summary$week$sequence}"))
      return(data.frame())
    }
    
    
  }
  
  all_games <- map_df(1 : nrow(scraped_games), function(x) {
    
    message(glue::glue("{x}"))
    get_game(x)
    
  })
  
  all_games %>%
    saveRDS("/Users/dylanmervis/Google Drive/participation_2020.rds")
  
  
  raw_json_participation %>% 
    pluck("plays", "home", "players") %>% 
    glimpse() 
  
  ## Home Players involved in each play of the game  
  participation_df_home <- pluck(raw_json_participation, "plays", "home", "players") %>%
    enframe() %>%
    unnest_auto(value) %>%
    dplyr::select(-jersey, -position, -sr_id) %>%
    unnest_wider(id) %>%
    rename("id_1" = "...1", "id_2" = "...2", "id_3" = "...3", "id_4" = "...4", "id_5" = "...5", "id_6" = "...6",
           "id_7" = "...7", "id_8" = "...8", "id_9" = "...9", "id_10" = "...10", "id_11" = "...11") %>%
    unnest_wider(name...3) %>%
    rename("name_1" = "...1", "name_2" = "...2", "name_3" = "...3", "name_4" = "...4", "name_5" = "...5", 
           "name_6" = "...6", "name_7" = "...7", "name_8" = "...8", "name_9" = "...9", "name_10" = "...10",
           "name_11" = "...11") %>%
    unnest_wider(reference) %>%
    rename("gsis_1" = "...1", "gsis_2" = "...2", "gsis_3" = "...3", "gsis_4" = "...4", "gsis_5" = "...5", 
           "gsis_6" = "...6", "gsis_7" = "...7", "gsis_8" = "...8", "gsis_9" = "...9", "gsis_10" = "...10",
           "gsis_11" = "...11") %>%
    dplyr::select(-name...1)
  
  ## Repeat process for Away Players
  participation_df_away <- pluck(raw_json_participation, "plays", "away", "players") %>%
    enframe() %>%
    unnest_auto(value) %>%
    dplyr::select(-jersey, -position, -sr_id) %>%
    unnest_wider(id) %>%
    rename("id_1" = "...1", "id_2" = "...2", "id_3" = "...3", "id_4" = "...4", "id_5" = "...5", "id_6" = "...6",
           "id_7" = "...7", "id_8" = "...8", "id_9" = "...9", "id_10" = "...10", "id_11" = "...11") %>%
    unnest_wider(name...3) %>%
    rename("name_1" = "...1", "name_2" = "...2", "name_3" = "...3", "name_4" = "...4", "name_5" = "...5", 
           "name_6" = "...6", "name_7" = "...7", "name_8" = "...8", "name_9" = "...9", "name_10" = "...10",
           "name_11" = "...11") %>%
    unnest_wider(reference) %>%
    rename("gsis_1" = "...1", "gsis_2" = "...2", "gsis_3" = "...3", "gsis_4" = "...4", "gsis_5" = "...5", 
           "gsis_6" = "...6", "gsis_7" = "...7", "gsis_8" = "...8", "gsis_9" = "...9", "gsis_10" = "...10",
           "gsis_11" = "...11") %>%
    dplyr::select(-name...1)
  
  ## Bind together to get full game participation dataset
  game_participation <- cbind(participation_df_home, participation_df_away)
  
  
  

  
  
  scraped_games <- list.files("/Users/dylanmervis/Desktop/SportRadar PBP/2017",
                              pattern = ".rds", 
                              full.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  get_game <- function(scraped_games, i) {
    
    game <- readRDS(glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/2017/{scraped_games %>% dplyr::slice(i) %>% pull(id)}"))
    
    home <- game$summary$home$alias
    away <- game$summary$away$alias
    season <- game$summary$season$year
    week <- game$summary$week$sequence
    
    pbp <- game %>%
      pluck("periods", "pbp") %>%
      bind_rows() %>%
      as_tibble() %>%
      pull(events) %>%
      bind_rows() %>%
      as_tibble() %>%
      dplyr::select(
        reference, description, sequence, screen_pass, play_action, run_pass_option, statistics, details, 
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed
      ) %>%
      unnest_wider(statistics) %>%
      unnest(cols = stat_type : att_yards) %>%
      dplyr::rename(
        existing_description = description,
        existing_yards = yards,
        play_sequence = sequence
      ) %>%
      dplyr::select(
        reference, play_sequence, existing_description, screen_pass, play_action, run_pass_option, details, 
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed,
        on_target_throw, dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, batted_pass, hurry
      ) %>%
      unnest_wider(details) %>%
      unnest(cols = c(
        on_target_throw, dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, batted_pass, hurry, play_blitz
      )) %>%
      dplyr::select(
        play_blitz, reference, play_sequence, existing_description, play_action, screen_pass, run_pass_option, 
        hurry, batted_pass, incompletion_type,
        on_target_throw, dropped, catchable, pocket_location, players_rushed,
        men_in_box, running_lane, pass_route
      ) %>%
      group_by(reference) %>%
      summarize(
        sr_desc = dplyr::first(existing_description),
        sequence = dplyr::first(play_sequence),
        play_action = if_else(dplyr::first(na.omit(play_action)), 1, 0),
        screen_pass = if_else(dplyr::first(na.omit(screen_pass)), 1, 0), 
        run_pass_option = if_else(dplyr::first(na.omit(run_pass_option)), 1, 0), 
        blitz = dplyr::first(play_blitz), 
        hurry = max(na.omit(hurry)), 
        batted_pass = max(na.omit(batted_pass)), 
        incompletion_type = dplyr::first(na.omit(incompletion_type)),
        on_target_throw = dplyr::first(na.omit(on_target_throw)), 
        dropped = dplyr::first(na.omit(dropped)), 
        catchable = dplyr::first(na.omit(catchable)), 
        pocket_location = dplyr::first(na.omit(pocket_location)), 
        players_rushed = dplyr::first(na.omit(players_rushed)),
        running_lane = dplyr::first(na.omit(running_lane)),
        men_in_box = dplyr::first(na.omit(men_in_box)),
        pass_route = dplyr::first(na.omit(pass_route))
      ) %>%
      ungroup() %>%
      dplyr::rename(play_id = reference) %>%
      mutate(
        play_id = as.numeric(play_id),
        game_id = 
          glue::glue('{season}_{formatC(week, width=2, flag=\"0\")}_{away}_{home}'),
        hurry = if_else(hurry == -Inf, NA_real_, hurry),
        batted_pass = if_else(batted_pass == -Inf, NA_real_, batted_pass)
      ) %>%
      arrange(sequence)
    
    return(pbp)
    
  }
  
  all_games <- future_map_dfr(1 : nrow(scraped_games), function(x) {
    
    message(glue::glue("{x}"))
    get_game(scraped_games, x)
    
  })
  
  
  ## REPEAT FOR GAMES MISSING PLAY ACTION
  
  scraped_games_no_pa <- list.files("/Users/dylanmervis/Desktop/SportRadar PBP/No PA 2017",
                                    pattern = ".rds", 
                                    full.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  get_game_no_pa <- function(scraped_games_no_pa, i) {
    
    game <- readRDS(glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/No PA 2017/{scraped_games_no_pa %>% dplyr::slice(i) %>% pull(id)}"))
    
    home <- game$summary$home$alias
    away <- game$summary$away$alias
    season <- game$summary$season$year
    week <- game$summary$week$sequence
    
    pbp_no_pa <- game %>%
      pluck("periods", "pbp") %>%
      bind_rows() %>%
      as_tibble() %>%
      pull(events) %>%
      bind_rows() %>%
      as_tibble() %>%
      dplyr::select(
        reference, description, sequence, screen_pass, statistics, details, 
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed
      ) %>%
      unnest_wider(statistics) %>%
      unnest(cols = stat_type : att_yards) %>%
      dplyr::rename(
        existing_description = description,
        existing_yards = yards,
        play_sequence = sequence
      ) %>%
      dplyr::select(
        reference, play_sequence, existing_description, screen_pass, details, 
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed,
        dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, hurry
      ) %>%
      unnest_wider(details) %>%
      unnest(cols = c(
        dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, hurry, play_blitz
      )) %>%
      dplyr::select(
        play_blitz, reference, play_sequence, existing_description, screen_pass,
        hurry, incompletion_type,
        dropped, catchable, pocket_location, players_rushed,
        men_in_box, running_lane, pass_route
      ) %>%
      group_by(reference) %>%
      summarize(
        sr_desc = dplyr::first(existing_description),
        sequence = dplyr::first(play_sequence),
        play_action = NA,
        screen_pass = if_else(dplyr::first(na.omit(screen_pass)), 1, 0), 
        run_pass_option = NA,
        blitz = dplyr::first(play_blitz), 
        hurry = max(na.omit(hurry)), 
        batted_pass = NA,
        incompletion_type = dplyr::first(na.omit(incompletion_type)),
        on_target_throw = NA, 
        dropped = dplyr::first(na.omit(dropped)), 
        catchable = dplyr::first(na.omit(catchable)), 
        pocket_location = dplyr::first(na.omit(pocket_location)), 
        players_rushed = dplyr::first(na.omit(players_rushed)),
        running_lane = dplyr::first(na.omit(running_lane)),
        men_in_box = dplyr::first(na.omit(men_in_box)),
        pass_route = dplyr::first(na.omit(pass_route))
      ) %>%
      ungroup() %>%
      dplyr::rename(play_id = reference) %>%
      mutate(
        play_id = as.numeric(play_id),
        game_id = 
          glue::glue('{season}_{formatC(week, width=2, flag=\"0\")}_{away}_{home}'),
        hurry = if_else(hurry == -Inf, NA_real_, hurry)
      ) %>%
      arrange(sequence)
    
    return(pbp_no_pa)
    
  }
  
  all_games_no_pa <- future_map_dfr(1 : nrow(scraped_games_no_pa), function(x) {
    
    message(glue::glue("{x}"))
    get_game_no_pa(scraped_games_no_pa, x)
    
  })
  
  all_games <- rbind(all_games, all_games_no_pa) %>%
    arrange(sequence)
  all_games %>%
    saveRDS("/Users/dylanmervis/Google Drive/pbp_2017.rds")
  
  ## REPEAT FOR GAMES MISSING DETAILS - AND GAMES PRIOR TO 2017
  
  scraped_games_no_details <- list.files("/Users/dylanmervis/Desktop/SportRadar PBP/2015",
                                         pattern = ".rds", 
                                         full.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      id = value
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  get_game_no_details <- function(scraped_games_no_details, i) {
    
    game <- readRDS(glue::glue("/Users/dylanmervis/Desktop/SportRadar PBP/2015/{scraped_games_no_details %>% dplyr::slice(i) %>% pull(id)}"))
    
    home <- game$summary$home$alias
    away <- game$summary$away$alias
    season <- game$summary$season$year
    week <- game$summary$week$sequence
    
    pbp_no_details <- game %>%
      pluck("periods", "pbp") %>%
      bind_rows() %>%
      as_tibble() %>%
      pull(events) %>%
      bind_rows() %>%
      as_tibble() %>%
      dplyr::select(
        reference, description, sequence, screen_pass, statistics,
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed
      ) %>%
      unnest_wider(statistics) %>%
      unnest(cols = stat_type : att_yards) %>%
      dplyr::rename(
        existing_description = description,
        existing_yards = yards,
        play_sequence = sequence
      ) %>%
      dplyr::select(
        reference, play_sequence, existing_description, screen_pass,
        pocket_location, qb_at_snap, pass_route, running_lane, men_in_box, play_blitz = blitz, players_rushed,
        dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, hurry
      ) %>%
      unnest(cols = c(
        dropped, catchable, incompletion_type, broken_tackles, pass_route, qb_at_snap,
        men_in_box, hurry, play_blitz
      )) %>%
      dplyr::select(
        play_blitz, reference, play_sequence, existing_description, screen_pass,
        hurry, incompletion_type,
        dropped, catchable, pocket_location, players_rushed,
        men_in_box, running_lane, pass_route
      ) %>%
      group_by(reference) %>%
      summarize(
        sr_desc = dplyr::first(existing_description),
        sequence = dplyr::first(play_sequence),
        play_action = "NA",
        screen_pass = if_else(dplyr::first(na.omit(screen_pass)), 1, 0), 
        run_pass_option = "NA",
        blitz = dplyr::first(play_blitz), 
        hurry = max(na.omit(hurry)), 
        batted_pass = "NA",
        incompletion_type = dplyr::first(na.omit(incompletion_type)),
        on_target_throw = "NA", 
        dropped = dplyr::first(na.omit(dropped)), 
        catchable = dplyr::first(na.omit(catchable)), 
        pocket_location = dplyr::first(na.omit(pocket_location)), 
        players_rushed = dplyr::first(na.omit(players_rushed)),
        running_lane = dplyr::first(na.omit(running_lane)),
        men_in_box = dplyr::first(na.omit(men_in_box)),
        pass_route = dplyr::first(na.omit(pass_route))
      ) %>%
      ungroup() %>%
      dplyr::rename(play_id = reference) %>%
      mutate(
        game_id = 
          glue::glue('{season}_{formatC(week, width=2, flag=\"0\")}_{away}_{home}'),
        hurry = if_else(hurry == -Inf, NA_real_, hurry)
      ) %>%
      arrange(sequence)
    
    return(pbp_no_details)
    
  }
  
  all_games_no_details <- future_map_dfr(1 : nrow(scraped_games_no_details), function(x) {
    
    message(glue::glue("{x}"))
    get_game_no_details(scraped_games_no_details, x)
    
  })
  
  
  
  all_games %>%
    saveRDS("/Users/dylanmervis/Google Drive/pbp_2020.rds")
  
  
  