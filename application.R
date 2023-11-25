bb_preds <- read_csv('bb_preds.csv')
fb_preds <- read_csv('fb_preds.csv')
off_preds <- read_csv('off-preds.csv')

final_pitches <- rbind(bb_preds, fb_preds)
final_pitches <- rbind(final_pitches, off_preds)

median_pred <- median(final_pitches$.pred)

pitcher_summary <- final_pitches %>% 
  group_by(player_name, team_name) %>% 
  summarise(n = n(),
            .pred = median(.pred),
            location_plus = 100*(.pred/median_pred),
            location_plus = ifelse(scaled > 0, 200-scaled, 200 + scaled)) %>% 
  filter(n > 249)
