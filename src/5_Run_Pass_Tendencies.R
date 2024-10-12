install.packages("nflreadr")

library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

pbp <- load_participation(2023, include_pbp = TRUE)

names(pbp)

pbp <- pbp |> 
  separate_rows(offense_players, sep = ";") |> 
  select(old_game_id, play_id, posteam, defteam, week, offense_players, offense_personnel, offense_formation,
         rush, pass, wp, down, ydstogo, yardline_100, score_differential, pass_oe)

pbp <- pbp |> 
  filter(rush == 1 | pass == 1)

pbp <- pbp |> 
  mutate(
    `Down & Distance` = case_when(
      down == 1 & ydstogo == 10 ~ '1st & 10',
      down == 1 & ydstogo > 10 ~ '1st and 11+',
      down == 2 & ydstogo == 1 ~ '2nd & 1',
      down == 2 & ydstogo == 2 ~ '2nd & 2',
      down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ '2nd & 3-6',
      down == 2 & ydstogo >= 7 & ydstogo <= 9 ~ '2nd & 7-9',
      down == 2 & ydstogo >= 10 ~ '2nd & 10+',
      down == 3 & ydstogo == 1 ~ '3rd & 1',
      down == 3 & ydstogo == 2 ~ '3rd & 2',
      down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ '3rd & 3-6',
      down == 3 & ydstogo >= 7 & ydstogo <= 9 ~ '3rd & 7-9',
      down == 3 & ydstogo >= 10 ~ '3rd & 10+',
      TRUE ~ '4th Down'
    )
  )

pbp$`Down & Distance` <- factor(pbp$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                  '2nd & 1', '2nd & 2',
                                                                  '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                  '3rd & 1', '3rd & 2', '3rd & 3-6',
                                                                  '3rd & 7-9', '3rd & 10+',
                                                                  '4th Down'))

roster <- load_rosters(2023)

pbp <- left_join(pbp, roster |> select(gsis_id, full_name, position), by = c('offense_players' = 'gsis_id'))
                 

pbp |> 
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |> 
  group_by(full_name) |> 
  summarize(
    pass_rate = mean(pass, na.rm = T),
    plays = n(),
    proe = mean(pass_oe, na.rm = T)
  ) |> 
  filter(pass_rate >= 0.8 | pass_rate <= 0.2, plays >= 50) |> 
  arrange(-pass_rate)

pbp |> 
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |> 
  group_by(full_name, position, down) |> 
  summarize(
    pass_rate = mean(pass, na.rm = T),
    plays = n(),
    proe = mean(pass_oe, na.rm = T)
  ) |> 
  filter(pass_rate >= 0.8 | pass_rate <= 0.2, plays >= 20, down %in% c(1, 2)) |> 
  arrange(-pass_rate)

pbp |> 
  filter(posteam == 'BUF', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2),
         yardline_100 >= 14) |> 
  group_by(play_id) |> 
  mutate(
    KnoxIn = any(full_name == 'Dawson Knox'),
    KincaidIn = any(full_name == 'Dalton Kincaid')
  ) |> slice(1) |> 
  group_by(KnoxIn, KincaidIn) |> 
  summarize(
    pass_rate = mean(pass, na.rm = T),
    plays = n(),
    proe = mean(pass_oe, na.rm = T)
  )

BUF_runpass <- pbp |> 
  filter(posteam == 'BUF', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2),
         position %in% c('HB', 'WR', 'TE', 'FB')) |>
  group_by(full_name, position, posteam) |> 
  summarize(
    snaps_played = n(),
    pass_rate = mean(pass, na.rm = T),
    rush_rate = mean(rush, na.rm = T),
    proe = mean(pass_oe, na.rm = T)/100
  ) |>
  filter(snaps_played >= 50) |> 
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('posteam' = 'team_abbr'))

BUFtbl <- BUF_runpass |> 
  select(full_name, position, team_wordmark, snaps_played, pass_rate, rush_rate, proe) |> 
  ungroup() |> 
  arrange(-snaps_played) |> 
  gt() |> 
  gt_img_rows(team_wordmark) |> 
  fmt_percent(columns = c(pass_rate, rush_rate, proe), decimals = 2) |> 
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Blake Robinson | data: nflreadr") |> 
  cols_label(
    full_name = "Player",
    team_wordmark = "Offense",
    snaps_played = "Snaps",
    pass_rate = "Pass Rate",
    rush_rate = "Rush Rate",
    proe = "Pass Rate over Expected"
  ) |> 
  opt_row_striping() |> 
  tab_header(title = "Bills run/pass rate by player in 2023") |> 
  gt_theme_538()

gtsave(BUFtbl, "BUFrunpasstbl.png")