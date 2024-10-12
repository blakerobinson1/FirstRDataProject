#standard imports
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
install.packages("gtExtras")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

#creates new dataset with 2021/22 data from play by play 
pbp <- load_pbp(2021:2022)

#number of rows, first 10 r
nrow(pbp)
pbp |> head(10)

#prints names of columns, truncated
names(pbp)

#returns pbp data based on selected columns
pbp |> select(posteam, defteam, down, ydstogo, play_type)

#creates new dataset filtering for valid runs and passes only
pbp_rp <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  filter(!is.na(epa))

nrow(pbp_rp)

#returns valid rush plays by Detroit, grouped by player and calculating average epa per play, sorting
pbp_rp |> 
  filter(posteam == "DET", rush == 1, !is.na(rusher_player_name))  |> 
  group_by(rusher_player_name) |> 
  summarize(rushes = n(), 
            epa_rush = mean(epa),
            wpa = sum(wpa)) |> 
  filter(rushes >= 10) |> 
  arrange(-epa_rush)
  
#creates new dataset with: 2021 pass plays grouped by team, includes columns of number of passes and average pass epa
pass_efficiency <- pbp_rp |> 
  filter(season == 2021, pass == 1) |> 
   group_by(posteam) |> 
  summarize(passes = n(),
      pass_epa = mean(epa))

#creates new dataset with: 2021 rush plays grouped by team, columns are number of run plays and average epa
rush_efficiency <- pbp_rp  |> 
  filter(season == 2021, rush == 1) |> 
  group_by(posteam) |> 
  summarize(rushes = n(),
            rush_epa = mean(epa))

#creates new dataset combining pass and rush epas from above
total_eff <- pass_efficiency |> 
  left_join(rush_efficiency, by = "posteam") 

#joins that ds to team logos for plotting
total_eff <- total_eff |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

View(teams_colors_logos)

#creates final plot
total_eff |> 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), linetype = "dashed") + 
  geom_vline(xintercept = mean(total_eff$pass_epa), linetype = "dashed") + 
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() + 
  labs(x = "EPA per Pass",
       y = "EPA per Rush",
       title = "EPA/pass and EPA/rush in 2021",
       subtitle = "Regular season and playoffs included",
       caption = "Blake Robinson | @BlakeRBSN | nflfastR")


