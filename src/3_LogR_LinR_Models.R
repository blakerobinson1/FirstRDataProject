library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)

pbp <- load_pbp(2018:2022)

fourth_downs <- pbp |> 
  filter(down == 4, !is.na(play_type))

fourth_downs |> 
  group_by(play_type) |> 
  tally(sort = T)

fourth_downs <- fourth_downs |> 
  mutate(went_for_it = ifelse(play_type %in% c("pass", "run"), 1, 0))

fourth_downs |> 
  group_by(ydstogo) |> 
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) |> 
  filter(count >= 5) |> 
  ggplot(aes(x = ydstogo, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

fourth_downs |> 
  group_by(yardline_100) |> 
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) |> 
  filter(count >= 5) |> 
  ggplot(aes(x = yardline_100, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

log_fourth <- glm(went_for_it ~ ydstogo + yardline_100 + wp,
                  data = fourth_downs)

log_fourth

summary(log_fourth)

vip(log_fourth)

log_fourth_co <- glm(went_for_it ~ (yardline_100 + ydstogo + wp)^2,
                     data = fourth_downs)

summary(log_fourth_co) 

log_fourth$fitted.values

fourth_downs |> 
  mutate(pred_prob = log_fourth$fitted.values) |> 
  ggplot(aes(x = ydstogo)) +
  geom_line(aes(y = pred_prob), color = "black", size = 2) + 
  geom_point(aes(y = went_for_it, color = ifelse(went_for_it == 1, "darkgreen", "darkred")),
             alpha = 0.8) + 
  scale_color_identity() + 
  theme_minimal() + 
  labs(x = "Yards to go",
       y = "Change Offense will go for it (0-1)")

fourth_downs <- fourth_downs |> 
  mutate(pred_prob = log_fourth$fitted.values) |> 
  mutate(fourth_oe = went_for_it - pred_prob)
 
team_fourth_22 <- fourth_downs |> 
  filter(season == 2022) |> 
  group_by(posteam) |> 
  summarize(count = n(),
            exp_fourth = sum(pred_prob),
            actual_fourth = sum(went_for_it),
            fourths_oe = sum(fourth_oe)) |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

team_fourth_22 |> 
  ggplot(aes(x = exp_fourth, y = actual_fourth)) +
  geom_hline(yintercept = mean(team_fourth_22$actual_fourth), linetype = "dashed") +
  geom_vline(xintercept = mean(team_fourth_22$exp_fourth), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 1.5, alpha = 0.5, se = FALSE) + 
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_minimal() +
  labs(x = "Expected 4th Down Conversion Attempts",
       y = "Actual 4th Down Conversion Attempts",
       title = "Team 4th Down Conversion Attempt Rate vs Expected 2022",
       subtitle = "Based on Logistic Regression model",
       caption = "Blake Robinson | @BlakeRbsn") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

pass_plays <- pbp |> 
  filter(pass == 1) |> 
  filter(!is.na(air_yards), !is.na(down), !is.na(wp), !is.na(ydstogo), !is.na(half_seconds_remaining))

pass_play_model <- pass_plays |> 
  select(air_yards, down, wp, ydstogo, half_seconds_remaining, season) |> 
  mutate(down = as.factor(down))

str(pass_play_model)

colSums(is.na(pass_play_model))

air_yards_lm <- lm(air_yards ~ down + wp + ydstogo + half_seconds_remaining + as.factor(season), data = pass_play_model)

summary(air_yards_lm)

vip(air_yards_lm)

air_yards_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_play_model)) |> 
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_play_model.)

air_yards_projs <- cbind(pass_plays, air_yards_preds)

ayoe_22 <- air_yards_projs |> 
  mutate(ayoe = air_yards - exp_air_yards) |> 
  filter(season == 2022) |> 
  group_by(passer) |> 
  summarize(passes = n(),
            exp_air_yards = mean(exp_air_yards),
            adot = mean(air_yards),
            avg_ayoe = mean(ayoe),
            team = last(posteam)) |> 
  filter(passes >= 140) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

ayoe_22 |> 
  ggplot(aes(x = avg_ayoe, y = fct_reorder(passer, avg_ayoe))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.6) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  geom_text(aes(label = paste0("n=", passes)), x = min(ayoe_22$avg_ayoe) - 0.05, size = 2.5) +
  labs(x = "Average Air Yards over Expected",
       y = "",
       title = "Average Air Yards over Expected, 2022",
       subtitle = "Minimum of 140 passes on the season, generated with linear regression model",
       caption = "Blake Robinson | @BlakeRbsn") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank())
