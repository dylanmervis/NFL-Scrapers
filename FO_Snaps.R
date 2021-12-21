library(httr)
library(rvest)
library(dplyr)
library(tidyr)
library(janitor)


getFOSnaps <- function(x = 2021,
                       user = "dmervis",
                       pwd = "mervisman9046") {
  message(paste("Scraping", x, "Season Snaps"))
  POST(
    paste0(
      'https://www.footballoutsiders.com/stats/nfl/snap-counts?year=',
      x,
      '&team=ALL&week=4&position=ALL'
    ),
    body = list(
      form_id = 'user_login_form',
      name = user,
      pass = pwd,
      op = 'Login'
    ),
    encode = 'form',
    config(followlocation = 0L)
  ) %>%
    read_html %>%
    html_table %>%
    bind_rows %>%
    clean_names %>%
    rename(name = player) %>%
    mutate(
      season = x,
      name = sub("[.]", ". ", name),
      number = sub("-.*", "", name) %>% as.numeric,
      name = sub(".*[0-9][-]", "", name),
    ) %>%
    mutate_at(vars(contains('rate')), ~ sub("[%]", "", .) %>% as.numeric) %>%
    mutate_at(vars(contains('rate')), ~ . / 100) %>%
    as_tibble
}

weekly_snaps <- getFOSnaps() %>%
  mutate(week = 1)

write_csv(weekly_snaps, "/Users/dylanmervis/Desktop/Model Testing/2020/Snaps/week_4_2021_snaps.csv")


## SUM SNAPS PER PLAYER
cum_snaps <-
  list.files(path = "/Users/dylanmervis/Desktop/Model Testing/2019/Snaps",
             pattern = "snaps.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  mutate(name = str_replace(name, ". ", ".")) %>%
  mutate(starter = ifelse(started == "YES", 1, 0),
         week = as.numeric(week)) %>%
  mutate(team = case_when(team == "LAR" ~ "LA",
                          team == "OAK" ~ "LV",
                          team == "SD" ~ "LAC",
                          TRUE ~ team)) %>%
  filter(!is.na(starter)) %>%
  group_by(name, team, position) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(cum_o_snaps = cumsum(offense_snaps),
         cum_d_snaps = cumsum(defense_snaps),
         cum_st_snaps = cumsum(special_teams_snaps)) %>%
  ungroup()

write_csv(cum_snaps,"/Users/dylanmervis/Desktop/Model Testing/2020/Snaps/cum_weekly_snaps_2020.csv")

## SEASON SNAPS
season_snaps <- 
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Snaps",
             pattern = "snaps.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  mutate(name = str_replace(name, ". ", "."))
  

## WEEKLY TEAM SNAPS OFFENSE AND DEFENSE
weekly_team_pass_o_snaps <- pbp_fastr %>%
  filter(season %in% 2017:2020,
         pass == 1) %>%
  group_by(posteam, season, week) %>%
  summarise(team_pass_o_snaps = n())
weekly_team_rush_o_snaps <- pbp_fastr %>%
  filter(season %in% 2017:2020,
         rush == 1) %>%
  group_by(posteam, season, week) %>%
  summarise(team_rush_o_snaps = n())

weekly_team_pass_d_snaps <- pbp_fastr %>%
  filter(season %in% 2017:2020,
         pass == 1) %>%
  group_by(defteam, season, week) %>%
  summarise(team_pass_d_snaps = n())
weekly_team_rush_d_snaps <- pbp_fastr %>%
  filter(season %in% 2017:2020,
         rush == 1) %>%
  group_by(defteam, season, week) %>%
  summarise(team_rush_d_snaps = n())

weekly_team_snaps <- left_join(weekly_team_pass_o_snaps, weekly_team_rush_o_snaps,
                               by = c("posteam", "season", "week")) %>%
  left_join(weekly_team_pass_d_snaps, by = c("posteam" = "defteam", "season", "week")) %>%
  left_join(weekly_team_rush_d_snaps, by = c("posteam" = "defteam", "season", "week")) %>%
  mutate(team_total_o = team_pass_o_snaps + team_rush_o_snaps,
         team_total_d = team_pass_d_snaps + team_rush_d_snaps) %>%
  group_by(posteam, season) %>%
  mutate(team_cum_o = cumsum(team_total_o),
         team_cum_d = cumsum(team_total_d)) %>%
  ungroup()


## READ IN YEARLY CUMULATIVE SNAPS AND JOIN WITH TEAM CUMULATIVE

cum_snaps_17 <- read_csv("/Users/dylanmervis/Desktop/Model Testing/2017/Snaps/cum_weekly_snaps_2017.csv")
cum_snaps_18 <- read_csv("/Users/dylanmervis/Desktop/Model Testing/2018/Snaps/cum_weekly_snaps_2018.csv")
cum_snaps_19 <- read_csv("/Users/dylanmervis/Desktop/Model Testing/2019/Snaps/cum_weekly_snaps_2019.csv")
cum_snaps_20 <- read_csv("/Users/dylanmervis/Desktop/Model Testing/2020/Snaps/cum_weekly_snaps_2020.csv")

cum_snaps <- rbind(cum_snaps_17, cum_snaps_18, cum_snaps_19, cum_snaps_20) %>%
  dplyr::select(name, position, number, season, team, week, starter, total_snaps, offense_snaps,
                defense_snaps, cum_o_snaps, cum_d_snaps) %>%
  left_join(weekly_team_snaps, by = c("team" = "posteam", "season", "week")) %>%
  mutate(o_snap_rate = round(cum_o_snaps/team_cum_o, digits = 4),
         d_snap_rate = round(cum_d_snaps/team_cum_d, digits = 4)) %>%
  mutate(o_snap_rate = ifelse(o_snap_rate > 1, 1.00, o_snap_rate),
         d_snap_rate = ifelse(d_snap_rate > 1, 1.00, d_snap_rate))


## MAKE NAME EDITS AND JOIN WITH MASTER ROSTER
qb_season_snaps <- season_snaps %>%
  filter(position == "QB") %>%
  mutate(offense_snap_rate = round(as.numeric(offense_snap_rate), 4),
         season = as.numeric(season),
         number = as.numeric(number)) %>%
  filter(offense_snap_rate > 0.01) %>%
  mutate(team = case_when(team == "LARM/STL" ~ "LA",
                          team == "LAR" ~ "LA",
                          team == "SD" ~ "LAC",
                          team == "OAK" ~ "LV",
                          name == "J.Freeman" & season == "2013" ~ "MIN",
                          name == "R.Mallett" & season == "2015" ~ "BAL",
                          name == "B.Weeden" & season == "2015" ~ "DAL",
                          name == "M.Flynn" & season == "2013" ~ "GB",
                          name == "J.Clausen" & season == "2015" ~ "BAL",
                          TRUE ~ team),
         name = case_when(name == "Matt.Moore" | name == "M.Moore" ~ "Ma.Moore",
                           name == "Sh.Hill" ~ "S.Hill",
                           name == "D.Carr" & team == "NYG" ~ "Da.Carr",
                           name == "R.Smith" ~ "Ru.Smith",
                          name == "J.Freeman" | name == "Jo.Freeman" ~ "J.Freeman",
                          name == "Dom.Davis" ~ "Do.Davis",
                          name == "T.Hill" & team == "NO" ~ "Ta.Hill",
                          name == "K.Allen" & team == "CAR" ~ "Ky.Allen",
                          name == "K.Allen" & team == "WAS" ~ "Ky.Allen",
                          name == "D.Jones" & team == "NYG" ~ "Da.Jones",
                           TRUE ~ name)) %>%
  left_join(qb_rapm_roster, by = c("name" = "Abbr", "team" = "Team", "position" = "Position",
                                   "season" = "Season")) %>%
  filter(!is.na(Full_Name)) %>%
  dplyr::select(full_name=Full_Name, abbr=name, gsis_id, position, jersey=number, season,
                team, offense_snap_rate)


qb_weekly_snaps <- cum_snaps %>%
  filter(position == "QB") %>%
  mutate(season = as.numeric(season),
         number = as.numeric(number)) %>%
  mutate(team = case_when(team == "LARM/STL" ~ "LA",
                          team == "LAR" ~ "LA",
                          team == "SD" ~ "LAC",
                          team == "OAK" ~ "LV",
                          TRUE ~ team),
         name = case_when(name == "Matt.Moore" | name == "M.Moore" ~ "Ma.Moore",
                          name == "Sh.Hill" ~ "S.Hill",
                          name == "D.Carr" & team == "NYG" ~ "Da.Carr",
                          name == "R.Smith" ~ "Ru.Smith",
                          name == "J.Freeman" | name == "Jo.Freeman" ~ "J.Freeman",
                          name == "Dom.Davis" ~ "Do.Davis",
                          name == "T.Hill" & team == "NO" ~ "Ta.Hill",
                          name == "K.Allen" & team == "CAR" ~ "Ky.Allen",
                          name == "K.Allen" & team == "WAS" ~ "Ky.Allen",
                          name == "D.Jones" & team == "NYG" ~ "Da.Jones",
                          TRUE ~ name)) %>%
  left_join(qb_rapm_roster, by = c("name" = "Abbr", "team" = "Team", "position" = "Position",
                                   "season" = "Season")) %>%
  filter(!is.na(Full_Name)) %>%
  dplyr::select(full_name=Full_Name, abbr=name, gsis_id, position, jersey=number, season,
                team, week, starter, offense_snaps, cum_o_snaps, team_total_o, team_cum_o,
                o_snap_rate)
  
  
## REPEAT FOR SKILL
skill_season_snaps <- season_snaps %>%
  filter(position %in% c("RB", "FB", "WR", "TE")) %>%
  mutate(offense_snap_rate = round(as.numeric(offense_snap_rate), 4),
         season = as.numeric(season),
         number = as.numeric(number)) %>%
  filter(offense_snap_rate > 0.01) %>%
  mutate(team = case_when(team == "LARM/STL" ~ "LA",
                          team == "LAR" ~ "LA",
                          team == "SD" ~ "LAC",
                          team == "OAK" ~ "LV",
                          name == "C.Michael" & season == "2016" ~ "GB",
                          name == "K.Benjamin" & season == "2017" ~ "BUF",
                          TRUE ~ team),
         name = case_when(name == "Ja.Jones" & team == "GB" ~ "J.Jones",
                           name == "Bruc.Miller" ~ "B.Miller",
                           name == "Jz.Rodgers" ~ "J.Rodgers",
                           name == "C.Wells" ~ "B.Wells",
                           name == "S.Johnson" & team == "BUF" |
                             name == "S.Johnson" & team == "LAC" |
                             name == "S.Johnson" & team == "SF" & season == "2014" ~ "St.Johnson",
                           name == "Al.Smith" & team == "CLE" ~ "A.Smith",
                           name == "De.Williams" & team == "CAR" ~ "D.Williams",
                           name == "Dar.Johnson" & team == "ATL" ~ "D.Johnson",
                           name == "Jo.Brown" & team == "ARI" ~ "J.Brown",
                           name == "Dan.Fells" & team == "NYG" ~ "D.Fells",
                           name == "Ch.Johnson" & team == "MIN" ~ "C.Johnson",
                           name == "Dar.Fells" ~ "D.Fells",
                           name == "D.Harris" & team == "NYG" & season == "2015" ~ "Dw.Harris",
                          name == "D.Harris" & team == "CLE" & season == "2019" ~ "Dem.Harris",
                           name == "De.Williams" & team == "PIT" & season == "2015" ~ "D.Williams",
                           name == "D.Johnson" & team == "CLE" & number == "29" ~ "Du.Johnson",
                           name == "D.Johnson" & team == "ARI" & number == "31" ~ "Da.Johnson",
                           name == "Ka.Williams" & team == "BUF" ~ "K.Williams",
                           name == "Ty.Williams" & team == "LAC" ~ "Tyr.Williams",
                           name == "M.Thomas-NO" ~ "M.Thomas",
                           name == "D.Harris" & number == "84" ~ "Dem.Harris",
                           name == "D.Washington" & team == "DET" & season == "2016" ~ "Dw.Washington",
                           name == "Ju.Jones" & team == "ATL" ~ "J.Jones",
                           name == "J.Hill" & team == "NO" ~ "Jo.Hill",
                           name == "Jo.Howard" & team == "CHI" ~ "J.Howard",
                           name == "T.Taylor" & team == "SF" ~ "Tr.Taylor",
                           name == "R.Smith" & team == "DAL" ~ "Ro.Smith",
                           name == "M.Williams" & number == "87" & team == "BAL" | 
                            name == "M.Williams" & number == "87" & team == "ARI" ~ "Ma.Williams",
                           name == "M.Brown-LA" ~ "M.Brown",
                           name == "J.Brown" & team == "SEA" ~ "Ja.Brown",
                           name == "De.Thomas" & team == "DEN" |
                             name == "De.Thomas" & team == "HOU" ~ "D.Thomas",
                           name == "E.St Brown" ~ "E.St. Brown",
                           name == "Dam.Williams" ~ "D.Williams",
                           name == "J.Adams-PHI" ~ "J.Adams",
                           name == "DK.Metcalf" ~ "D.Metcalf",
                           name == "Ro.Anderson" & team == "NYJ" ~ "R.Anderson",
                           name == "I.Smith" & team == "MIN" ~ "Ir.Smith",
                           name == "T.Williams" & team == "LV" & season == "2019" ~ "Tyr.Williams",
                           name == "A.Brown" & team == "TEN" ~ "AJ.Brown",
                           name == "D.Johnson" & team == "HOU" & number == "25" ~ "Du.Johnson",
                           name == "D.Johnson" & team == "HOU" & number == "31" ~ "Da.Johnson",
                           name == "T.Johnson" & team == "DET" & number == "31" ~ "Ty.Johnson",
                           name == "M.Brown" & team == "LA" & season == "2019" | 
                            name == "M.Brown" & team == "LA" & season == "2020" ~ "Mal.Brown",
                           name == "K.Johnson" & team == "ARI" & season == "2019" | 
                            name == "K.Johnson" & team == "ARI" & season == "2020" ~ "Ke.Johnson",
                           name == "Di.Johnson" ~ "D.Johnson",
                           name == "T.Johnson" & season == "2020" & team == "NYJ" ~ "Ty.Johnson",
                           name == "J.Smith" & season == "2020" & team == "NYJ" ~ "Je.Smith",
                           name == "Ty.Johnson" & team == "LAC" ~ "Tyr.Johnson",
                           TRUE ~ name)) %>%
  left_join(skill_rapm_roster, by = c("name" = "Abbr", "team" = "Team",
                                      "season" = "Season")) %>%
  filter(!is.na(Full_Name)) %>%
  dplyr::select(full_name=Full_Name, abbr=name, gsis_id, position, jersey=number, season,
                team, offense_snap_rate) %>%
  distinct(full_name, gsis_id, season, team, .keep_all = TRUE)


skill_weekly_snaps <- cum_snaps %>%
  filter(position %in% c("RB", "FB", "WR", "TE")) %>%
  mutate(season = as.numeric(season),
         number = as.numeric(number)) %>%
  mutate(team = case_when(team == "LARM/STL" ~ "LA",
                          team == "LAR" ~ "LA",
                          team == "SD" ~ "LAC",
                          team == "OAK" ~ "LV",
                          name == "C.Michael" & season == "2016" ~ "GB",
                          name == "K.Benjamin" & season == "2017" ~ "BUF",
                          TRUE ~ team),
         name = case_when(name == "Ja.Jones" & team == "GB" ~ "J.Jones",
                          name == "Bruc.Miller" ~ "B.Miller",
                          name == "Jz.Rodgers" ~ "J.Rodgers",
                          name == "C.Wells" ~ "B.Wells",
                          name == "S.Johnson" & team == "BUF" |
                            name == "S.Johnson" & team == "LAC" |
                            name == "S.Johnson" & team == "SF" & season == "2014" ~ "St.Johnson",
                          name == "Al.Smith" & team == "CLE" ~ "A.Smith",
                          name == "De.Williams" & team == "CAR" ~ "D.Williams",
                          name == "Dar.Johnson" & team == "ATL" ~ "D.Johnson",
                          name == "Jo.Brown" & team == "ARI" ~ "J.Brown",
                          name == "Dan.Fells" & team == "NYG" ~ "D.Fells",
                          name == "Ch.Johnson" & team == "MIN" ~ "C.Johnson",
                          name == "Dar.Fells" ~ "D.Fells",
                          name == "D.Harris" & team == "NYG" & season == "2015" ~ "Dw.Harris",
                          name == "D.Harris" & team == "CLE" & season == "2019" ~ "Dem.Harris",
                          name == "De.Williams" & team == "PIT" & season == "2015" ~ "D.Williams",
                          name == "D.Johnson" & team == "CLE" & number == "29" ~ "Du.Johnson",
                          name == "D.Johnson" & team == "ARI" & number == "31" ~ "Da.Johnson",
                          name == "Ka.Williams" & team == "BUF" ~ "K.Williams",
                          name == "Ty.Williams" & team == "LAC" ~ "Tyr.Williams",
                          name == "M.Thomas-NO" ~ "M.Thomas",
                          name == "D.Harris" & number == "84" ~ "Dem.Harris",
                          name == "D.Washington" & team == "DET" & season == "2016" ~ "Dw.Washington",
                          name == "Ju.Jones" & team == "ATL" ~ "J.Jones",
                          name == "J.Hill" & team == "NO" ~ "Jo.Hill",
                          name == "Jo.Howard" & team == "CHI" ~ "J.Howard",
                          name == "T.Taylor" & team == "SF" ~ "Tr.Taylor",
                          name == "R.Smith" & team == "DAL" ~ "Ro.Smith",
                          name == "M.Williams" & number == "87" & team == "BAL" | 
                            name == "M.Williams" & number == "87" & team == "ARI" ~ "Ma.Williams",
                          name == "M.Brown-LA" ~ "M.Brown",
                          name == "J.Brown" & team == "SEA" ~ "Ja.Brown",
                          name == "De.Thomas" & team == "DEN" |
                            name == "De.Thomas" & team == "HOU" ~ "D.Thomas",
                          name == "E.St Brown" ~ "E.St. Brown",
                          name == "Dam.Williams" ~ "D.Williams",
                          name == "J.Adams-PHI" ~ "J.Adams",
                          name == "DK.Metcalf" ~ "D.Metcalf",
                          name == "Ro.Anderson" & team == "NYJ" ~ "R.Anderson",
                          name == "I.Smith" & team == "MIN" ~ "Ir.Smith",
                          name == "T.Williams" & team == "LV" & season == "2019" ~ "Tyr.Williams",
                          name == "A.Brown" & team == "TEN" ~ "AJ.Brown",
                          name == "D.Johnson" & team == "HOU" & number == "25" ~ "Du.Johnson",
                          name == "D.Johnson" & team == "HOU" & number == "31" ~ "Da.Johnson",
                          name == "T.Johnson" & team == "DET" & number == "31" ~ "Ty.Johnson",
                          name == "M.Brown" & team == "LA" & season == "2019" | 
                            name == "M.Brown" & team == "LA" & season == "2020" ~ "Mal.Brown",
                          name == "K.Johnson" & team == "ARI" & season == "2019" | 
                            name == "K.Johnson" & team == "ARI" & season == "2020" ~ "Ke.Johnson",
                          name == "Di.Johnson" ~ "D.Johnson",
                          name == "T.Johnson" & season == "2020" & team == "NYJ" ~ "Ty.Johnson",
                          name == "J.Smith" & season == "2020" & team == "NYJ" ~ "Je.Smith",
                          name == "Ty.Johnson" & team == "LAC" ~ "Tyr.Johnson",
                          TRUE ~ name)) %>%
  left_join(skill_rapm_roster, by = c("name" = "Abbr", "team" = "Team", "position" = "Position",
                                   "season" = "Season")) %>%
  filter(!is.na(Full_Name)) %>%
  dplyr::select(full_name=Full_Name, abbr=name, gsis_id, position, jersey=number, season,
                team, week, starter, offense_snaps, cum_o_snaps, team_total_o, team_cum_o,
                o_snap_rate)

line_rapm_roster <- rapm_roster %>%
  filter(Position %in% c("C", "G", "T", "OL", "OG", "OT")) %>%
  dplyr::select(-Position) %>%
  mutate(position = "OL") %>%
  mutate(Team = case_when(Team == "BAL" & Season == "2019" & Full_Name == "Andre Smith" ~ "CIN",
                          Team == "ARI" & Season == "2019" & Full_Name == "Deion Calhoun" ~ "MIA",
                          Team == "MIA" & Season == "2020" & Full_Name == "Cameron Tom" ~ "NO",
                          Team == "ARI" & Season == "2020" & Full_Name == "Deion Calhoun" ~ "CIN",
                          Team == "JAX" & Season == "2020" & Full_Name == "Derwin Gray" ~ "PIT",
                          TRUE ~ Team)) %>%
  mutate(Full_Name = case_when(Full_Name == "Shaq Calhoun" & Team == "MIA" & Season == "2019" ~ "Deion Calhoun",
                               Full_Name == "Mike Jordan" & Team == "CIN" & Season == "2019" ~ "Michael Jordan",
                               Full_Name == "Julien Davenport" ~ "Julie'n Davenport",
                               Full_Name == "Yosuah Nijman" ~ "Yosh Nijman",
                               TRUE ~ Full_Name)) %>%
  mutate(Abbr = case_when(Full_Name == "Deion Calhoun" ~ "D.Calhoun",
                          TRUE ~ Abbr))

line_weekly_snaps <- cum_snaps %>%
  filter(position == "OL") %>%
  mutate(season = as.numeric(season),
         number = as.numeric(number)) %>%
  mutate(team = case_when(team == "LARM/STL" ~ "LA",
                          team == "LAR" ~ "LA",
                          team == "SD" ~ "LAC",
                          team == "OAK" ~ "LV",
                          TRUE ~ team)) %>%
  mutate(name = case_when(name == "M.Pouncey-LAC" ~ "M.Pouncey",
                          name == "M.Pouncey-PIT" ~ "M.Pouncey",
                          name == "Br.Allen" ~ "B.Allen",
                          TRUE ~ name)) %>%
  left_join(line_rapm_roster, by = c("season" = "Season", "team" = "Team", "name" = "Abbr",
                                     "position")) %>%
  filter(!is.na(Full_Name)) %>%
  dplyr::select(full_name = Full_Name, abbr = name, gsis_id, position, jeryse = number, season,
                team, week, starter, offense_snaps, cum_o_snaps, team_total_o, team_cum_o,
                o_snap_rate)
  

## NFLREADR SNAPS VERSION 
skill_season_snaps <- nflreadr::load_snap_counts(c(2013:2021)) %>%
  select(pfr_player_id, game_id, season, ends_with(c("snaps", "pct"))) %>%
  left_join(nflreadr::load_rosters(c(2013:2021)), by = c("pfr_player_id" = "pfr_id", "season")) %>%
  dplyr::select(full_name, gsis_id, pfr_id = pfr_player_id, position, season, game_id,
                offense_snaps, offense_pct, defense_snaps, defense_pct) %>%
  group_by(full_name, season) %>%
  mutate(cum_o_snaps = cumsum(offense_snaps),
         cum_d_snaps = cumsum(defense_snaps)) %>%
  ungroup()

team_o_snaps <- nflreadr::load_pbp(2013:2021) %>%
  group_by(game_id, posteam) %>%
  summarise(o_snaps = n()) %>%
  ungroup()

team_d_snaps <- nflreadr::load_pbp(2013:2021) %>%
  group_by(game_id, defteam) %>%
  summarise(d_snaps = n()) %>%
  ungroup()

