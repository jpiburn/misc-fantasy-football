library(tidyverse)

seasons <- as.character(2010:2019)


pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

game_df <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/schedules/sched_{x}.rds")
    )
  )
})


# fumbles
fpt_fum <-
pbp %>%
  filter(
    !is.na(fumble_recovery_1_team),
    fumble_lost == 1
  ) %>%
  transmute(
    play_id,
    game_id,
    week,
    dst_team = fumble_recovery_1_team,
    fpt_fumble_recovery = 2,
    fpt_fumble_recovery_td = if_else(return_touchdown == 1, 6, 0, missing = 0)
  )


# ints
fpt_int <-
pbp %>%
  filter(
    interception == 1
  ) %>%
  transmute(
    play_id,
    game_id,
    week,
    dst_team = defteam,
    fpt_interception = 2,
    fpt_interception_td = if_else(return_touchdown == 1, 6, 0, missing = 0)
  )


# blocks
fpt_block <-
pbp %>%
  filter(
    field_goal_result == "blocked" | extra_point_result == "blocked" | punt_blocked == 1
  ) %>%
  transmute(
    play_id,
    game_id,
    week,
    dst_team = defteam,
    fpt_block = 2,
    fpt_block_td = if_else(!is.na(td_team), 6, 0, missing = 0)
  )


# safety/sacks
fpt_safety_sacks <-
pbp %>%
  filter(
    sack == 1 | safety == 1
  ) %>%
  transmute(
    play_id,
    game_id,
    week,
    dst_team = defteam,
    fpt_sack_safety = if_else(sack == 1, 1, 0, missing = 0),
    fpt_sack_safety = if_else(safety == 1, 2, fpt_sack_safety, missing = fpt_sack_safety)
  )


# returns
fpt_returns <-
  pbp %>%
  filter(
    !is.na(return_team),
    return_touchdown == 1
  ) %>%
  transmute(
    play_id,
    game_id,
    week,
    dst_team = td_team,
    fpt_return_td = 6
  )


dst_totals <-
  fpt_fum %>%
  full_join(fpt_int) %>%
  full_join(fpt_block) %>%
  full_join(fpt_safety_sacks) %>%
  full_join(fpt_returns) %>%
  rowwise() %>% # requries dplyr >= 1.0
  mutate(
    play_total = sum(c_across(starts_with("fpt_")), na.rm = TRUE)
  ) %>%
  group_by(game_id, week, dst_team) %>%
  summarise(
    dst_points = sum(play_total)
  )


df <- full_join(game_df, dst_totals) %>%
  mutate(
    dst_team = if_else(dst_team == "LV", "OAK", dst_team),
    points_allowed = if_else(dst_team == home_team, away_score, home_score),
    dst_points_new = dst_points + 10 + (-0.25 * points_allowed)
  )


df %>%
  select(
    week, season, starts_with("dst")
  ) %>%
  group_by(season, dst_team) %>%
  mutate(
    season_total = sum(dst_points),
    season_total_new = sum(dst_points_new)
  ) %>%
  group_by(season) %>%
  mutate(
    season_rank = dense_rank(desc(season_total))
  ) %>%
  ungroup() %>%
  filter(season_rank <= 16) %>%
  group_by(week, season, dst_team) %>%
  tidyr::pivot_longer(cols = contains("points")) %>%
  mutate(
    scoring = if_else(name == "dst_points", "Last Years Scoring", "Proposed New Scoring (+10 baseline, -0.25 per Point Allowed)")
  ) %>%
  ggplot(
    aes(x = value,  fill = scoring)
  ) +  
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~season,ncol =3) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "Comparison of Weekly D/ST scoring Systems",
    subtitle = "Includes only teams that finished end of season top-16 (to remove teams that no one would have played)",
    x = "D/ST Points",
    y = "Density",
    fill = "System",
    caption = "I wonder what Miles will complain about?"
  )