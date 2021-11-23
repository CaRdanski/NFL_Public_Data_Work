##library(nflscrapR)
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(nflfastR)
library(modelr)


##Grab the last 6 years of play by play data. This is sourced from Ben Baldwin's github account. He is a sports writer for the Athletic can be found on twitter @BenBBaldwin

future::plan("multisession")         
pbp <- load_pbp(2015:2020)


pbpSkinny <- pbp %>%          ##Main Dataset. With data blending below, we'll add a few columns.
  filter(play_type_nfl %in% c("RUSH","PASS")) %>%
  filter(season_type == "REG") %>% 
  select(season
         ,rusher_player_name
         ,receiver_player_name
         ,game_id
         ,week
         ,posteam
         ,defteam
         ,play_id
         ,desc
         ,epa
         ,ep
         ,yards_gained
         ,yardline_100
         ,total_home_score
         ,total_away_score,pass
         ,rush,play_type
         ,play_type_nfl
         ,down
         ,ydstogo
         ,rusher_player_id
         ,receiver_player_id
         ,home_team
         ,season_type
         ,wp
         ,cpoe
         ,score_differential
         ,game_seconds_remaining)



RushRank <- pbp %>%          ##Rank Rushers by count of weeks where they were the highest rushes taken, then 2nd highest, etc.
  filter(play_type_nfl == "RUSH") %>%
  group_by(season,posteam, rusher_player_name,rusher_player_id) %>%
  summarise(rushes = n()) %>%
  arrange(season, posteam, desc(rushes)) %>%
  group_by(season, posteam) %>%
  mutate(rank = rank(desc(rushes), ties.method = "first")) %>%
  rename(id = rusher_player_id)

RushTotals <- RushRank %>%
  group_by(season,posteam) %>% 
  summarise(total.rushes = sum(rushes))

RushTbl <- full_join(RushRank,RushTotals,by = c("season","posteam"))

RushTbl <- RushTbl %>% 
  mutate(Rush.Split = rushes/total.rushes,
         RusherRankRushes = paste0("Rusher",rank,"Rushes"),
         RusherRankRush.Split = paste0("Rusher",rank,"Rush.Split"))

RushTblRushes <- RushTbl %>% 
  select(season,posteam,RusherRankRushes,rushes) %>% 
  pivot_wider(names_from = RusherRankRushes, values_from = rushes)

RushTblRush.Split <- RushTbl %>% 
  select(season,posteam,RusherRankRush.Split,Rush.Split) %>% 
  pivot_wider(names_from = RusherRankRush.Split, values_from = Rush.Split)

RecRank <- pbp %>%          ##Rank Rushers by count of weeks where they were the highest rushes taken, then 2nd highest, etc.
  filter(play_type_nfl == "PASS") %>%
  group_by(season,posteam, receiver_player_name,receiver_player_id) %>%
  summarise(targets = n()) %>%
  arrange(season, posteam, desc(targets)) %>%
  group_by(season, posteam) %>%
  mutate(rank = rank(desc(targets), ties.method = "first")) %>%
  rename(id = receiver_player_id)

RecTotals <- RecRank %>%
  group_by(season,posteam) %>% 
  summarise(total.targets = sum(targets))

RecTbl <- full_join(RecRank,RecTotals,by = c("season","posteam"))

RecTbl <- RecTbl %>% 
  mutate(Target.Split = targets/total.targets,
         RecRankTarget.Split = paste0("Rec",rank,"Target.Split"),
         RecRankTargets = paste0("Rec",rank,"Targets"))

RecTblTargets <- RecTbl %>% 
  select(season,posteam,RecRankTargets,total.targets) %>% 
  pivot_wider(names_from = RecRankTargets, values_from = total.targets)

RushTblRush.Split <- RushTbl %>% 
  select(season,posteam,RusherRankRush.Split,Rush.Split) %>% 
  pivot_wider(names_from = RusherRankRush.Split, values_from = Rush.Split)

##Thoughts... need to create the dataset with all the years specified, then translate to +/- each year as zero year...
##Add columns for season wins, diff_2020, diff_2019, etc. then use those diff columns to pivot...
##pivot option 1: filter to diff = 0, then join to (filtered) diff = -1, join to diff = -2, etc... do we need to keep some kind of unique ID? does keeping season do that for us?
##pivot option 2: does pivot_longer, then wider solve this?... pivot_longer so that the diff_ column headers are now a single field, with their values as a single field next to them, then use those values to pivot_wider

proj.train <-pbpSkinny %>% 
  mutate(diff_2020 = (2020-season),
         diff_2019 = (2019-season),
         diff_2018 = (2018-season),
         diff_2017 = (2017-season),
         diff_2016 = (2016-season),
         diff_2015 = (2015-season)) %>%
  pivot_longer(
    cols = c('diff_2020','diff_2019','diff_2018','diff_2017','diff_2016','diff_2015'),
    names_to = "year.zero",
    values_to = "years.diff"
  ) %>% 
  filter(years.diff >= 0) %>% ##need to find a way to join to the win_loss dataset before season field is removed
  group_by(year.zero,years.diff,posteam,play_type_nfl) %>% 
  summarise(plays = n(),
            avg_cpoe = sum(cpoe, na.rm = TRUE)/n()
            ,sum_epa = sum(epa, na.rm = TRUE)
            ) %>% 
  pivot_wider(names_from = play_type_nfl,
              values_from = c(plays, avg_cpoe, sum_epa)
              ) %>% 
  mutate(pass.percent = plays_PASS/(plays_PASS+plays_RUSH),
         total.plays = plays_PASS+plays_RUSH,
         total.epa = sum_epa_PASS+sum_epa_RUSH) %>%
  select(-avg_cpoe_RUSH) %>% 
  pivot_longer(
    cols = c('plays_PASS','plays_RUSH','total.plays', 'pass.percent','sum_epa_PASS','sum_epa_RUSH','total.epa','avg_cpoe_PASS'),
    names_to = "measures",
    values_to = "values") %>% 
  pivot_wider(
    names_from = c(measures, years.diff),
    names_sep = "",
    values_from = values) 
##%>% 
  ##mutate(ThreeYrAvgTotalPlays = (total.plays1 + total.plays2 + total.plays3)/3,
         ##ThreeYrAvgPassPer = (pass.percent1 + pass.percent2 + pass.percent3)/3)

##################
##add in data regarding preseason expectations
#################

winlines <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/win_totals.csv")

winlines2020 <- winlines %>% 
  filter(season == 2020) %>% 
  select(-season,-under_odds) %>% 
  rename(line0yrsago = line,
         overodds0yrsago = over_odds)

winlines2019 <- winlines %>% 
  filter(season == 2019) %>% 
  select(-season,-under_odds) %>% 
  rename(line1yrsago = line,
         overodds1yrsago = over_odds)

pass.rush.split <- full_join(proj.train,winlines2020,by = c("posteam" = "team"))

pass.rush.split2 <- full_join(pass.rush.split,winlines2019,by = c("posteam" = "team"))

############################
##add flags for QB change and playcaller change
################
playcallers <- read.csv("https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/playcallers.csv")
pcs <- playcallers %>% 
  group_by(season,posteam,off_play_caller) %>% 
  summarise(games = n_distinct(game_id)) %>% 
  filter(games == max(games)) %>% 
  arrange(posteam,season)

pcs2 <- as_tibble(pcs) %>% 
  mutate(playcaller_yoy = ifelse(posteam == lag(posteam), ifelse(off_play_caller == lag(off_play_caller)
                                 ,"no_change","playcaller_change"),"first_year_of_data"))

pcs2020 <- pcs2 %>% 
  filter(season == 2020) %>% 
  select(-season,-off_play_caller,-games) %>% 
  rename(playcaller_yoy0 = playcaller_yoy)

pcs2019 <- pcs2 %>% 
  filter(season == 2019) %>% 
  select(-season,-off_play_caller,-games) %>% 
  rename(playcaller_yoy1 = playcaller_yoy)

qbs <- pbp %>% 
  filter(!is.na(passer_player_name)) %>% 
  group_by(season,posteam,passer_player_name) %>% 
  summarise(games = n_distinct(game_id)) %>% 
  filter(games == max(games)) %>% 
  arrange(posteam,season) 

qbs2 <- as_tibble(qbs) %>% 
  mutate(qb_yoy = ifelse(posteam == lag(posteam), ifelse(passer_player_name == lag(passer_player_name)
                                                                 ,"no_change","qb_change"),"first_year_of_data"))
qbs2020 <- qbs2 %>% 
  filter(season == 2020) %>% 
  select(-season,-passer_player_name,-games) %>% 
  rename(qb_yoy0 = qb_yoy)

qbs2019 <- qbs2 %>% 
  filter(season == 2019) %>% 
  select(-season,-passer_player_name,-games) %>% 
  rename(qb_yoy1 = qb_yoy)

pass.rush.split3 <- full_join(pass.rush.split2,pcs2020,by = c("posteam"))

pass.rush.split4 <- full_join(pass.rush.split3,pcs2019,by = c("posteam"))

pass.rush.split5 <- full_join(pass.rush.split4,qbs2020,by = c("posteam"))

pass.rush.split6 <- full_join(pass.rush.split5,qbs2019,by = c("posteam"))

predict_plays <- lm(total.plays0 ~ 
                      total.plays1 
                    ##+ total.plays2 
                    ##+ total.plays3
                    ##+ pass.percent1 
                    + qb_yoy0
                    + qb_yoy1
                    + playcaller_yoy0
                    + line0yrsago 
                    ##+ overodds0yrsago
                    ##+ total.epa1
                    ##+ avg_cpoe_PASS1
                    ##+ line1yrsago
                    ##+ overodds1yrsago
                    ##QB Change? Playcaller change?
                    ,
   data = pass.rush.split6)

summary(predict_plays)


predicted_plays <- pass.rush.split6 %>% 
  add_predictions(predict_plays) %>% 
  mutate(prediction.delta = pred-total.plays0)

ggplot(predicted_plays, aes(x = prediction.delta))+
  geom_density()

##total plays off between -40 and 40 seems acceptable (that's 2.5plays per game off)
##but there are also outliers around -100 and 100

ggplot(predicted_plays, aes(x = pred, y = total.plays0))+
  geom_point()+
  coord_cartesian(xlim = c(850,1100),
                  ylim = c(850,1100))


##next we'll try more game-script style variables, e.g. how many plays per game-minute do they run while winning? while losing?
##and how many minutes will they be winning/losing during the season, given the pre-season win expectations?

pbpGameScript <- pbp %>%          ##Main Dataset. With data blending below, we'll add a few columns.
  filter(play_type_nfl %in% c("RUSH","PASS")) %>%
  filter(season_type == "REG") %>% 
  select(season,game_id,week,posteam,play_id,desc,epa,ep,yards_gained,yardline_100,
         total_home_score,total_away_score,pass,rush,play_type,play_type_nfl,down,ydstogo,home_team,season_type,wp,score_differential,
         game_seconds_remaining) %>% 
  mutate(wpGameScript = cut(wp, breaks = c(-Inf,0.4,0.6,Inf),labels=c("Losing","Neutral","Winning")),
         ScoreDiffGameScript = cut(score_differential, breaks = c(-Inf,-7,7,Inf),labels=c("Losing","Neutral","Winning"))) %>% 
  group_by(season,posteam,ScoreDiffGameScript,play_type_nfl) %>% ##can change this to group by wpGameScript if desired
  summarise(plays = n()) %>% 
  pivot_wider(names_from = play_type_nfl,
              values_from = plays) %>% 
  replace(is.na(.),0) %>% 
  mutate(Total.Plays = PASS+RUSH) %>% 
  select(-PASS,-RUSH) %>% 
  pivot_wider(names_from = ScoreDiffGameScript,
              values_from = Total.Plays)

Timing <- pbp %>%          ##Main Dataset. With data blending below, we'll add a few columns.
  filter(play_type_nfl %in% c("RUSH","PASS")) %>%
  filter(season_type == "REG") %>% 
  select(season,game_id,week,posteam,time_of_day,play_id,desc,epa,ep,yards_gained,yardline_100,
         total_home_score,total_away_score,pass,rush,play_type,play_type_nfl,down,ydstogo,home_team,season_type,wp,score_differential,
         game_seconds_remaining) %>% 
  mutate(wpGameScript = cut(wp, breaks = c(-Inf,0.4,0.6,Inf),labels=c("Losing","Neutral","Winning")),
        ScoreDiffGameScript = cut(score_differential, breaks = c(-Inf,-7,7,Inf),labels=c("Losing","Neutral","Winning"))) %>% 
  group_by(season, game_id,posteam) %>% 
  mutate(seconds = game_seconds_remaining -lag(game_seconds_remaining,default = game_seconds_remaining[1])) %>% 
  group_by(season,posteam,ScoreDiffGameScript) %>% 
  summarise(seconds = sum(seconds))



##also, how does wins in regular season correllate with plays?

plays <- pbpSkinny %>% 
  group_by(season,posteam) %>% 
  summarise(plays = n())


games <- read_csv("http://www.habitatring.com/games.csv")  ##Game-level data - provided by Lee Sharpe, found on twitter as @LeeSharpeNFL

games <- games %>%                ##Blend to get regular season win-loss record. will need to split away/home data, then union
  filter(season > 2014) %>%
  mutate(home_win_loss = case_when(result > 0 ~ 1,
                                   result == 0 ~ .5,
                                   result < 0 ~ 0),
         away_win_loss = case_when(result < 0 ~ 1,
                                   result == 0 ~ .5,
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
  select(season,home_team,home_team_reg_season_wins,home_team_reg_season_losses)

plays.wins <- left_join(plays,win_loss, by = c("season","posteam" = "home_team"))

ggplot(plays.wins, aes(x = home_team_reg_season_wins, y = plays))+
  geom_point()+
  geom_smooth()

WinsGameScript <- inner_join(pbpGameScript,win_loss, by = c("season","posteam" = "home_team"))

ggplot(WinsGameScript, aes(x = home_team_reg_season_wins, y = Losing)) +
  geom_point() ##this one is pretty good

p01 <- lm(Losing ~ home_team_reg_season_wins,data = WinsGameScript)

summary(p01) ##Adj R Sq: 0.6607

ggplot(WinsGameScript, aes(x = home_team_reg_season_wins, y = Winning)) +
  geom_point()

p02 <- lm(Winning ~ home_team_reg_season_wins,data = WinsGameScript)

summary(p02) ##Adj R Sq: 0.5892

ggplot(WinsGameScript, aes(x = home_team_reg_season_wins, y = Neutral)) +
  geom_point()+
  geom_smooth() ##Definite trend of teams with more wins running more plays in neutral, but large variance




###############################################
home_playoff <-games %>%        ##filter to playoff games to create a playoff team flag
  filter(season > 2014) %>%
  filter(game_type != "REG") %>%
  group_by(season,home_team) %>%
  summarize(games_played = n()) %>%
  rename(team = home_team)

away_playoff <-games %>%
  filter(season > 2014) %>%
  filter(game_type != "REG") %>%
  group_by(season,away_team) %>%
  summarize(games_played = n()) %>%
  rename(team = away_team)

playoff_teams <- rbind(home_playoff,away_playoff) %>%
  group_by(season,team) %>%
  summarise(posteam_playoff_games = sum(games_played))







final <- games %>%            ##grab game-level final outcomes
  filter(season > 2014) %>%
  select(game_id,season,home_score,away_score,result,home_win_loss,away_win_loss)%>%
  rename(final_score_home_team = home_score, final_score_away_team = away_score,final_point_diff_home_team = result,
         home_team_win = home_win_loss, away_team_win = away_win_loss)








###Blend all data together - need to be mindful that the game-level and season-level data will be at a different level of detail than the pbp data and will cause double counting if used incorrectly

pbpSkinny <- left_join(pbpSkinny,RushPosRank, by = c("season","posteam","rusher_player_name","rusher_player_id"="id"))

pbpSkinny <- left_join(pbpSkinny,RecPosRank, by = c("season","posteam","receiver_player_name","receiver_player_id"))

pbpSkinny <- left_join(pbpSkinny,win_loss, by = c("season","home_team"))

pbpSkinny <- left_join(pbpSkinny,playoff_teams, by = c("season","posteam"="team"))

pbpSkinny <- left_join(pbpSkinny,final,by = c("season","game_id"))



##add fields to simplify any posteam vs hometeam calculation issues brought up by join availability
pbpSkinny <- pbpSkinny %>%
  mutate(posteam_win = ifelse(posteam == home_team,home_team_win,away_team_win),
         final_score_posteam = ifelse(posteam == home_team,final_score_home_team,final_score_away_team),
         final_point_diff_posteam = ifelse(posteam == home_team,final_point_diff_home_team,final_point_diff_home_team*-1))