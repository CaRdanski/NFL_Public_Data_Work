library(nflfastR)
library(tidyverse)
library(na.tools)

###Load roster data
future:plan("multisession")
roster <- fast_scraper_roster(2009:2021)

roster_skinny <- roster %>% 
  filter(position %in% c("RB","WR","QB","TE")) %>% 
  select(season,team,position,full_name,gsis_id,birth_date,height,weight,headshot_url) %>% 
  filter(!is.na(team))

roster_skinny$team <- str_replace(roster_skinny$team,"OAK","LV")
roster_skinny$team <- str_replace(roster_skinny$team,"SD","LAC")
roster_skinny$team <- str_replace(roster_skinny$team,"STL","LA")


future::plan("multisession")  
seasons1 <- 2015:2021          
pbp1 <- load_pbp(seasons1)
seasons2 <- 2009:2014
pbp2 <- load_pbp(seasons2)
pbp <- union(pbp1,pbp2)

pbp_skinny <- pbp %>%
  filter(season_type == "REG",
         penalty == 0) %>% 
  select(season,
         week,
         game_id,
         posteam,
         defteam,
         play_id,
         play_type,
         play_type_nfl,
         ##down,
         ##ydstogo,
         ##yardline_100,
         yards_gained,
         ##score_differential,
         desc,
         rusher_player_name,
         rusher_player_id,
         receiver_player_name,
         receiver_player_id,
         passer_player_name,
         passer_player_id,
         pass,
         rush,
         interception,
         touchdown,
         fumble_lost,
         game_date,
         fantasy_player_name,
         fantasy_player_id,
         passer_id,
         complete_pass,
  )

rusher_pbp <- pbp_skinny %>% 
  filter(play_type_nfl %in% c("RUSH"),
         !play_type %in% c("qb_kneel","qb_spike")) %>% 
  group_by(season,posteam,defteam,week,game_date,game_id,fantasy_player_name,fantasy_player_id) %>% 
  rename(player_id = fantasy_player_id) %>% 
  summarize(Rushing.Yards = sum(yards_gained),
            Rushes = n(), 
            Rush.TDs = sum(touchdown),
            Rusher.Fumbles = sum(fumble_lost),
            )

rec_pbp <- pbp_skinny %>% 
  filter(play_type_nfl %in% c("PASS"),
         !play_type %in% c("qb_kneel","qb_spike")) %>% 
  group_by(season,posteam,defteam,week,game_date,game_id,fantasy_player_name,fantasy_player_id) %>%
  rename(player_id = fantasy_player_id) %>% 
  summarize(Receiving.Yards = sum(yards_gained),
            Targets = n(),
            Receptions = sum(complete_pass),
            Rec.TDs = sum(touchdown),
            Receiver.Fumbles = sum(fumble_lost),
        )  

passer_pbp <- pbp_skinny %>% 
  filter(play_type_nfl %in% c("PASS"),
         !play_type %in% c("qb_kneel","qb_spike")) %>%
  group_by(season,posteam,defteam,week,game_date,game_id,passer_player_name,passer_id) %>%
  rename(player_id = passer_id) %>% 
  summarize(Passing.Yards = sum(yards_gained),
            Total.Passes = n(),
            INTs = sum(interception), 
            Pass.TDs = sum(touchdown),
            ) 

rec_rush_join_1 <- full_join(rec_pbp,rusher_pbp, by = c("player_id","season","week","game_id","game_date"))
player_data1 <- full_join(rec_rush_join_1,passer_pbp, by = c("player_id","season","week","game_id","game_date")) 

player_data2 <- player_data1 %>%
  mutate(#FF.Standard.Points.Pass.6 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*6),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
  #          na.replace((Rush.TDs*6),0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
  #          na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
  #        FF.Standard.Points.Pass.4 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*4),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
  #          na.replace((Rush.TDs*6),0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
  #          na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
  #        FF.PPR.Points.Pass.6 = na.replace((Passing.Yards/25),0)+na.replace((Pass.TDs*6),0)-na.replace((INTs*2),0)+na.replace((Rushing.Yards/10),0)+
  #          na.replace((Rush.TDs*6),0)+na.replace(Receptions,0)+na.replace((Receiving.Yards/10),0)+na.replace((Rec.TDs*6),0)-
  #          na.replace((Receiver.Fumbles*2),0)-na.replace((Rusher.Fumbles*2),0),
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


player_data_w_roster <- inner_join(player_data,roster_skinny,by = c("season","team","player_id" = "gsis_id"))

player_data_w_roster <- player_data_w_roster %>% 
  mutate(flex_position = case_when(
                          position %in% c("RB","WR","TE") ~ "FLEX",
                          TRUE ~ position
  ))

###blend to calculate fantasy points, weekly fantasy position ranks, and annual fantasy position ranks
player_data_pos_ranks <- player_data_w_roster %>% 
  # arrange(season, week, position, desc(FF.Standard.Points.Pass.6)) %>%
  # group_by(season, week, position) %>%
  # mutate(weekly_fantasy_rank_standard_6 = rank(desc(FF.Standard.Points.Pass.6), ties.method = "first")) %>% 
  # arrange(season, week, position, desc(FF.Standard.Points.Pass.4)) %>%
  # group_by(season, week, position) %>%
  # mutate(weekly_fantasy_rank_standard_4 = rank(desc(FF.Standard.Points.Pass.4), ties.method = "first")) %>% 
  # arrange(season, week, position, desc(FF.PPR.Points.Pass.6)) %>%
  # group_by(season, week, position) %>%
  # mutate(weekly_fantasy_rank_ppr_6 = rank(desc(FF.PPR.Points.Pass.6), ties.method = "first")) %>% 
  arrange(season, week, position, desc(FF.PPR.Points.Pass.4)) %>%
  group_by(season, week, position) %>%
  mutate(weekly_fantasy_rank_ppr_4 = rank(desc(FF.PPR.Points.Pass.4), ties.method = "first")) %>% 
  # arrange(season, week, flex_position, desc(FF.Standard.Points.Pass.6)) %>%
  # group_by(season, week, flex_position) %>%
  # mutate(weekly_flex_rank_standard_6 = rank(desc(FF.Standard.Points.Pass.6), ties.method = "first")) %>% 
  # arrange(season, week, flex_position, desc(FF.Standard.Points.Pass.4)) %>%
  # group_by(season, week, flex_position) %>%
  # mutate(weekly_flex_rank_standard_4 = rank(desc(FF.Standard.Points.Pass.4), ties.method = "first")) %>% 
  # arrange(season, week, flex_position, desc(FF.PPR.Points.Pass.6)) %>%
  # group_by(season, week, flex_position) %>%
  # mutate(weekly_flex_rank_ppr_6 = rank(desc(FF.PPR.Points.Pass.6), ties.method = "first")) %>% 
  arrange(season, week, flex_position, desc(FF.PPR.Points.Pass.4)) %>%
  group_by(season, week, flex_position) %>%
  mutate(weekly_flex_rank_ppr_4 = rank(desc(FF.PPR.Points.Pass.4), ties.method = "first"))  
  

replacement_level <- player_data_pos_ranks %>% 
  filter(position == "RB" & weekly_fantasy_rank_ppr_4 == 25
          | position =="WR" & weekly_fantasy_rank_ppr_4 ==25
          | position =="QB" & weekly_fantasy_rank_ppr_4 ==13
          | position == "TE" & weekly_fantasy_rank_ppr_4 == 13) %>% 
  select(season,week,position,FF.PPR.Points.Pass.4) %>% 
  rename(rep_lvl_pos_FF_points = FF.PPR.Points.Pass.4)

replacement_flex <- player_data_pos_ranks %>% 
  filter(flex_position == "FLEX" & weekly_flex_rank_ppr_4 == 61) %>% 
  select(season,week,flex_position,FF.PPR.Points.Pass.4) %>% 
  rename(rep_lvl_flex_FF_points = FF.PPR.Points.Pass.4)

player_data_w_replacement_lvl1 <- full_join(player_data_pos_ranks,replacement_level, by = c("season","week","position","flex_position"))
player_data_w_replacement_lvl2 <- full_join(player_data_w_replacement_lvl1,replacement_flex, by = c("season","week","flex_position"))

##consider removing players with less touches
all_player <- player_data_w_replacement_lvl2 %>% 
  group_by(player_id,position,season) %>% 
  summarise(touches = sum(Total.Touches)) %>% 
  group_by(player_id,position) %>% 
  summarise(avg_touches = mean(touches)) %>% 
  filter(avg_touches > 102) ##these players average more than 6 touches per game (327 players)

player_data_w_replacement_lvl <- semi_join(player_data_w_replacement_lvl2,all_player, by = c("player_id"))

write.csv(player_data_w_replacement_lvl,"fantasy_points_by_week_w_replacement_level.csv")


