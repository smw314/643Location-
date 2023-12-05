library(tidyverse)
library(janitor)
library(data.table)
library(reticulate)

pitches_2022 <- fread('xwobacon2022.csv')

pitches_2022 <- clean_names(pitches_2022)

valid_pitch_call <- c("BallCalled", "StrikeSwinging", "StrikeCalled", "InPlay",
                      "FoulBall", "HitByPitch", "BallIntentional", "BallinDirt")

pitches_2022 <- pitches_2022 %>% filter(pitch_call %in% valid_pitch_call)

pitches_2023 <- fread('xwobacon2023.csv')

pitches_2023 <- clean_names(pitches_2023)

valid_pitch_call <- c("BallCalled", "StrikeSwinging", "StrikeCalled", "InPlay",
                      "FoulBall", "HitByPitch", "BallIntentional", "BallinDirt")

pitches_2023 <- pitches_2023 %>% filter(pitch_call %in% valid_pitch_call) %>% select(-c0)

all_pitches <- rbind(pitches_2022, pitches_2023)

all_pitches$count <- paste(all_pitches$balls, all_pitches$strikes, sep = "-")

all_pitches <- all_pitches %>% 
  filter(count %in% c("0-1", "1-0", "0-0", "1-1", "0-2", "1-2", "3-1", "2-2", 
                      "3-0", "3-2", "2-0", "2-1"))

balls <- c("BallCalled") #"BallIntentional", "BallinDirt")
strikes_foul <- c("StrikeSwinging", "StrikeCalled", "FoulBall")
strikes <- c("StrikeSwinging", "StrikeCalled")

ml_data <- all_pitches %>% 
  mutate(woba = case_when(count == "0-0" ~ .379,
                          count == "0-1" ~ .315,
                          count == "0-2" ~ .23,
                          count == "1-0" ~ .428,
                          count == "1-1" ~ .353,
                          count == "1=2" ~ .26,
                          count == "2-0" ~ .506,
                          count == "2-1" ~ .422,
                          count == "2-2" ~ .314,
                          count == "3-0" ~ .626,
                          count == "3-1" ~ .551,
                          count == "3-2" ~ .436,
                          TRUE ~ 0),
         xwobacon_predictions = ifelse(xwobacon_predictions < 0, 0, 
                                       xwobacon_predictions), 
         woba_diff = xwobacon_predictions - woba,
         bip_rv = woba_diff/1.28358,
         pitch_rv = case_when(count == "0-0" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0354904,
                              count == "0-0" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0497338,
                              count == "0-1" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0308243,
                              count == "0-1" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0599167,
                              count == "0-2" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0203916,
                              count == "0-2" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled") ~ -0.148494,
                              count == "0-2" & pitch_call == "FoulBall" ~ 0,
                              count == "1-0" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0579567,
                              count == "1-0" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0543999,
                              count == "1-1" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0523265,
                              count == "1-1" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0703495,
                              count == "1-2" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.043283,
                              count == "1-2" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled") ~ -0.168886,
                              count == "1-2" & pitch_call == "FoulBall" ~ 0,
                              count == "2-0" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0872121,
                              count == "2-0" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0600301,
                              count == "2-1" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0924636,
                              count == "2-1" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0793929,
                              count == "2-2" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0901355,
                              count == "2-2" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled") ~ -0.212169,
                              count == "2-2" & pitch_call == "FoulBall" ~ 0,
                              count == "3-0" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.0867859,
                              count == "3-0" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.0547786,
                              count == "3-1" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.141564,
                              count == "3-1" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled" | pitch_call == "FoulBall") ~ -0.081721,
                              count == "3-2" & (pitch_call == "BallCalled" | pitch_call == "BallIntentional" | pitch_call == "BallinDirt") ~ 0.223285,
                              count == "3-2" & (pitch_call == "StrikeSwinging" | pitch_call == "StrikeCalled") ~ -0.302305,
                              count == "3-2" & pitch_call == "FoulBall" ~ 0,
                              # pitch_call %in% "HitByPitch" = ????,
                              pitch_call == "HitByPitch" ~ 0.3935,
                              TRUE ~ bip_rv),
         pitch_r = ifelse(pitcher_throws == "Right", 1, 0),
         bat_r = ifelse(batter_side == "Right", 1, 0)) %>% 
  select(plate_loc_height, plate_loc_side, balls, strikes, pitch_r, bat_r, 
         pitch_rv, pitch_date, tagged_pitch_type, player_name, team_name)
