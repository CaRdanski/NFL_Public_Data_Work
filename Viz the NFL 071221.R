library(nflfastR)
library(tidyverse)
library(na.tools)

###Load Playcaller dataset
playcallers <- read.csv("https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/playcallers.csv")
write.csv(playcallers,"playcallers.csv")

pcs <- playcallers %>% 
  group_by(season,posteam,off_play_caller) %>% 
  summarise(games = n_distinct(game_id)) %>% 
  filter(games == max(games))


###Load roster data
roster <- fast_scraper_roster(2019:2021)
##write.csv(roster,"nfl_rosters 2019-2021.csv")

roster_skinny <- roster %>% 
  select(season,team,position,full_name,gsis_id,birth_date,height,weight,headshot_url) %>% 
  filter(!is.na(team))

roster_skinny$team <- str_replace(roster_skinny$team,"OAK","LV")
roster_skinny$team <- str_replace(roster_skinny$team,"SD","LAC")
roster_skinny$team <- str_replace(roster_skinny$team,"STL","LA")


###Load games data (includes vegas odds)
games <- read_csv("http://www.habitatring.com/games.csv")  ##Game-level data - provided by Lee Sharpe, found on twitter as @LeeSharpeNFL

###Load vegas pre-season win totals
preseason.lines <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/win_totals.csv")  ##provided by Lee Sharpe, found on twitter as @LeeSharpeNFL

write.csv(preseason.lines,"preseason_lines.csv")

###Load play by play data
seasons <- 2019:2021          
pbp <- load_pbp(seasons)

pbp_skinny <- pbp %>%
  select(season,
         week,
         game_id,
         posteam,
         defteam,
         play_id,
         play_type,
         play_type_nfl,
         down,
         ydstogo,
         yardline_100,
         yards_gained,
         score_differential,
         desc,
         home_team,
         away_team,
         game_seconds_remaining,
         total_home_score,
         total_away_score,
         rusher_player_name,
         rusher_player_id,
         receiver_player_name,
         receiver_player_id,
         passer_player_name,
         passer_player_id,
         pass,
         rush,
         ep,
         epa,
         wp,
         wpa,
         season_type,
         spread_line,
         total_line,
         home_coach,
         away_coach,
         xpass,
         pass_oe,
         cp,
         cpoe,
         series,
         series_success,
         series_result,
         fixed_drive,
         fixed_drive_result,
         drive,
         play_clock,
         drive_play_count,
         drive_time_of_possession,
         drive_first_downs,
         away_wp,
         home_wp,
         interception,
         touchdown,
         fumble_lost,
         safety,
         kickoff_attempt,
         field_goal_attempt,
         field_goal_result,
         fourth_down_failed,
         punt_attempt
         )

write.csv(pbp_skinny,"skinny pbp 2019-2021.csv")

###Filter and summarize pbp data to player level
rusher_pbp <- pbp %>% 
  filter(play_type_nfl %in% c("RUSH"),
         !play_type %in% c("qb_kneel","qb_spike"),
         season_type == "REG",
         penalty == 0) %>% 
  group_by(season,posteam,defteam,week,game_date,game_id,fantasy_player_name,fantasy_player_id) %>% 
  rename(player_id = fantasy_player_id) %>% 
  summarize(Rushing.Yards = sum(yards_gained),
            Rushes = n(), 
            Rush.TDs = sum(touchdown),
            Rush.Inside.5 = length(rush_attempt[yardline_100<=5]),
            Rush.Inside.10 = length(rush_attempt[yardline_100<=10]),
            Rush.Inside.1 = length(rush_attempt[yardline_100<=1]),
            Rusher.Fumbles = sum(fumble_lost),
            Rush.EPA = sum(epa))

rec_pbp <- pbp %>% 
  filter(play_type_nfl %in% c("PASS"),
         !play_type %in% c("qb_kneel","qb_spike"),
         season_type == "REG",
         penalty == 0) %>% 
  group_by(season,posteam,defteam,week,game_date,game_id,fantasy_player_name,fantasy_player_id) %>%
  rename(player_id = fantasy_player_id) %>% 
  summarize(Receiving.Yards = sum(yards_gained),
            Receptions = sum(complete_pass),
            Targets = n(),
            Rec.TDs = sum(touchdown),
            Rec.Airyards = sum(air_yards), 
            Target.Inside.10 = length(qb_dropback[yardline_100<=10]),
            Target.Inside.5 = length(qb_dropback[yardline_100<=5]),
            Receiver.Fumbles = sum(fumble),
            Rec.EPA = sum(epa),
            Completed.Airyards = sum(air_yards[complete_pass == 1]),
            Completed.Airyards.Percent = Completed.Airyards/Rec.Airyards)  

passer_pbp <- pbp %>% 
  filter(play_type_nfl %in% c("PASS"),
         !play_type %in% c("qb_kneel","qb_spike"),
         season_type == "REG",
         penalty == 0) %>%
  group_by(season,posteam,defteam,week,game_date,game_id,passer_player_name,passer_id) %>%
  rename(player_id = passer_id) %>% 
  summarize(Passing.Yards = sum(yards_gained),
            Completions = sum(complete_pass),
            Total.Passes = n(),
            INTs = sum(interception), 
            Pass.TDs = sum(touchdown),
            Airyards = sum(air_yards), 
            Attempts.Inside.10 = length(qb_dropback[yardline_100<=10]),
            Attempts.Inside.5 = length(qb_dropback[yardline_100<=5]),
            mean.CPOE = mean(cpoe,na.rm = T),
            sum.CPOE = sum(cpoe),
            QB.Epa = sum(qb_epa)) 

team_pbp <- pbp %>% 
  filter(!play_type %in% c("qb_kneel","qb_spike"),
         season_type == "REG",
         !is.na(posteam)) %>%
  group_by(season,posteam,defteam,week,game_date,game_id) %>%
  summarize(sum.team.epa = sum(epa),
            avg.team.epa = mean(epa),
            total.team.plays = n(),
            successful.team.plays = sum(success)
            ) 

drive_pbp <- pbp %>% 
  filter(!play_type %in% c("qb_kneel","qb_spike"),
         season_type == "REG",
         !is.na(posteam)) %>%
  group_by(season,posteam,defteam,week,game_date,game_id,series,series_success) %>%
  summarise(count = n()) %>% 
  group_by(season,posteam,week,game_date,game_id) %>% 
  summarise(drives = n_distinct(series),
            successful.drives = sum(series_success))


rec_rush_join_1 <- full_join(rec_pbp,rusher_pbp, by = c("player_id","season","week","game_id","game_date"))
player_data1 <- full_join(rec_rush_join_1,passer_pbp, by = c("player_id","season","week","game_id","game_date")) 

player_data2 <- player_data1 %>%
  mutate(FF.Standard.Points.Pass.6 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*6),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
           na.replace((Rush.TDs*6),0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
           na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
         FF.Standard.Points.Pass.4 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*4),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
           na.replace((Rush.TDs*6),0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
           na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
         FF.PPR.Points.Pass.6 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*6),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
           na.replace((Rush.TDs*6),0)+na.replace(Receptions,0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
           na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
         FF.PPR.Points.Pass.4 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*4),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
           na.replace((Rush.TDs*6),0)+na.replace(Receptions,0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
           na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0)
         )


player_data <- player_data2 %>%
  mutate(player_name = ifelse(!is.na(fantasy_player_name.x), fantasy_player_name.x,
                              ifelse(!is.na(fantasy_player_name.y), fantasy_player_name.y, passer_player_name))) %>%
  mutate(team = ifelse(!is.na(posteam.x), posteam.x,
                       ifelse(!is.na(posteam.y), posteam.y, posteam)))%>% 
  mutate(def_team = ifelse(!is.na(defteam.x),defteam.x,
                           ifelse(!is.na(defteam.y),defteam.y,defteam))) %>% 
  select(-fantasy_player_name.x,-fantasy_player_name.y,-passer_player_name,-posteam.x,-posteam.y,-posteam,-defteam.x,-defteam.y,-defteam) %>%  
  select(player_name,team,season,everything()) %>% 
  mutate_if(is.numeric, funs(replace_na(., 0))) %>% 
  mutate(Total.Touches = Total.Passes+Targets+Rushes)

  
###Summarize pbp data to game level
game_data <- player_data %>% 
  group_by(season,team,def_team,week,game_date,game_id) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE)
  
###Summarize pbp data to season level
season_data_base <- player_data %>% 
  group_by(season,team,def_team) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  select(-week)

###Add roster data to player level
  ###First Check to see what falls off, fix if needed
falls_off_pbp <- anti_join(player_data,roster_skinny,by = c("season","team","player_id" = "gsis_id"))
x <- falls_off_pbp %>% 
  group_by(season,team) %>% 
  summarize(count = n()) ##All players that fall off are what we could consider 'backup players', but some are fantasy relevant for certain weeks, e.g. backup QBs
falls_off_roster <- anti_join(roster_skinny,player_data,by = c("season","team","gsis_id" = "player_id"))
z <- falls_off_roster %>% 
  group_by(season,team) %>% 
  summarize(count = n())
  ###reasonable amount fall off, assuming some rostered players never get a target, carry, or snap at QB


player_data <- left_join(player_data,roster_skinny,by = c("season","team","player_id" = "gsis_id"))

  ###blend to calculate fantasy points, weekly fantasy position ranks, and annual fantasy position ranks
player_data_pos_ranks <- player_data %>% 
  arrange(season, week, position, desc(FF.Standard.Points.Pass.6)) %>%
  group_by(season, week, position) %>%
  mutate(weekly_fantasy_rank_standard_6 = rank(desc(FF.Standard.Points.Pass.6), ties.method = "first")) %>% 
  arrange(season, week, position, desc(FF.Standard.Points.Pass.4)) %>%
  group_by(season, week, position) %>%
  mutate(weekly_fantasy_rank_standard_4 = rank(desc(FF.Standard.Points.Pass.4), ties.method = "first")) %>% 
  arrange(season, week, position, desc(FF.PPR.Points.Pass.6)) %>%
  group_by(season, week, position) %>%
  mutate(weekly_fantasy_rank_ppr_6 = rank(desc(FF.PPR.Points.Pass.6), ties.method = "first")) %>% 
  arrange(season, week, position, desc(FF.PPR.Points.Pass.4)) %>%
  group_by(season, week, position) %>%
  mutate(weekly_fantasy_rank_ppr_4 = rank(desc(FF.PPR.Points.Pass.4), ties.method = "first"))

  ###group, sum total touches and assign depth chart position ranks based on total season touches, then ungroup
season_touches_player_rank <- player_data_pos_ranks %>% 
  group_by(season,team,def_team,position,player_name,player_id) %>% 
  summarise(touches = sum(Total.Touches)) %>% 
  arrange(season,team,position) %>% 
  group_by(season,team,position) %>% 
  mutate(season_pos_roster_rank = rank(desc(touches), ties.method = "first"))

player_data <- full_join(player_data_pos_ranks,season_touches_player_rank,by = c("season","team","player_id","player_name"))

write.csv(player_data,"player_data_viz.csv")

###Add Playcaller data to game and season level
game_data1 <- left_join(game_data,playcallers, by = c("season","team" = "posteam","game_id"))
game_data2 <- full_join(game_data1,team_pbp, by = c("season","team" = "posteam","game_id","week","game_date"))
game_data3 <- full_join(game_data2,drive_pbp, by = c("season","team" = "posteam","game_id","week","game_date"))

pit <- game_data3 %>% filter(team == "PIT",season == 2021)

game_lines1 <- pbp_skinny %>% 
  group_by(game_id,posteam,defteam,home_team,spread_line,total_line) %>% 
  summarise()

game_lines2 <- as_tibble(game_lines1) %>% 
  filter(!is.na(posteam)) %>% 
  mutate(implied_home_score = (total_line/2)+abs(spread_line)/2,
         implied_away_score = (total_line/2) - abs(spread_line)/2,
         implied_posteam_score = ifelse(home_team == posteam,implied_home_score,implied_away_score),
         implied_posteam_win_margin = ifelse(home_team == posteam,abs(spread_line),abs(spread_line)*-1))

game_lines3 <- game_lines2 %>% 
  select(game_id,posteam,total_line,implied_posteam_score,implied_posteam_win_margin)
  

game_data4 <- full_join(game_data3,game_lines3, by = c("game_id","team" = "posteam"))

###Add Vegas expected wins and actual wins to season level
  ##Need to cleanse team name abbreviations to properly join this (LA vs LAC vs LAR vs LV, etc)
season_data_lines <- left_join(season_data_base,preseason.lines, by = c("season","team"))

games <- games %>%                ##Blend to get regular season win-loss record. will need to split away/home data, then union
  filter(season > 2014) %>%
  mutate(home_win_loss = case_when(result >= 0 ~ 1, ##Ties treated as wins for this exercise
                                   result < 0 ~ 0),
         away_win_loss = case_when(result < 0 ~ 1,
                                   result > 0 ~ 0))

home_reg <- games %>%
  filter(game_type == "REG") %>%
  group_by(season,home_team) %>%
  summarize(home_wins = sum(home_win_loss), home_losses = sum(away_win_loss))

away_reg <- games %>%
  filter(game_type == "REG") %>%
  group_by(season,away_team) %>%
  summarize(away_wins = sum(away_win_loss), away_losses = sum(home_win_loss))

win_loss <- inner_join(home_reg,away_reg,by = c("season","home_team"="away_team"))

win_loss <- win_loss %>%
  mutate(home_team_reg_season_wins = home_wins+away_wins,
         home_team_reg_season_losses = home_losses+away_losses) %>%
  select(season,home_team,home_team_reg_season_wins)

season_data_wins <- left_join(season_data_lines,win_loss, by = c("season","team" = "home_team"))

playcallers_season <- playcallers %>% 
  group_by(season,posteam,off_play_caller) %>% 
  summarise() ##need to check that there's only one playcaller per team per season, some seasons are duplicating things (e.g. green bay 2015)

season_data_playcallers <- left_join(season_data_wins,playcallers_season, by = c("season","team" = "posteam"))

    ###Add flags for change of playcaller (new playcaller) at the the season level, 
    ###then put those back into the game and player level data sets
season_data_arrange <- season_data_playcallers %>% 
  arrange(team,season) %>% 
  as.data.frame()

season_data <- season_data_arrange %>% 
  mutate(lag_playcaller = lag(off_play_caller,n = 1,default = NA_character_,order_by = team,season),
         lag_team = lag(team,n = 1,default = NA_character_,order_by = team,season),
         playcaller_change = ifelse(team != lag_team, NA,
                                    ifelse(off_play_caller == lag_playcaller,"YoY same playcaller","playcaller change"))) %>% 
  select(-lag_playcaller,-lag_team)

write.csv(season_data,"season_data_viz.csv")
write.csv(game_data4,"team_game_data_viz.csv")
