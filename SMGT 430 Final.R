#load in data
nba_pbp <- hoopR::load_nba_pbp()
#add column for opponent team id's
nba_pbp$opponent_team_id <- ifelse(nba_pbp$team_id == nba_pbp$home_team_id, 
                                   nba_pbp$away_team_id, nba_pbp$home_team_id)
#filter for shooting plays
shooting_plays <- nba_pbp[nba_pbp$shooting_play == TRUE, ]
unique(shooting_plays$type_text)
library(dplyr)
#sort shots into 5 types
shooting_plays <- shooting_plays %>%
  mutate(shot_type = case_when(
    grepl("Free Throw", type_text) ~ "Free Throw",
    grepl("Jump Shot", type_text) ~ "Jump Shot",
    grepl("Dunk", type_text) ~ "Dunk",
    grepl("Layup", type_text) ~ "Layup",
    grepl("Hook", type_text) ~ "Hook",
    TRUE ~ "Other"
  ))
#calculate distance from hoop
shooting_plays$x_distance <- abs(25 - shooting_plays$coordinate_x_raw)
shooting_plays$y_distance <- shooting_plays$coordinate_y_raw
shooting_plays$total_distance <- sqrt(shooting_plays$x_distance^2 + shooting_plays$y_distance^2)
#perform logistic regression
shot_quality_model <- glm(scoring_play ~ shot_type + total_distance + x_distance + y_distance, 
                          data = shooting_plays, family = "binomial")
summary(shot_quality_model)
#assign prediction values
shooting_plays$predicted_prob <- predict(shot_quality_model, newdata = shooting_plays, type = "response")
shooting_plays$is_three_pointer <- grepl("three point", shooting_plays$text, ignore.case = TRUE)
shooting_plays$is_free_throw <- grepl("Free Throw", shooting_plays$type_text, ignore.case = TRUE)
shooting_plays <- shooting_plays %>%
  mutate(
    predicted_points = case_when(
      is_three_pointer ~ 3 * predicted_prob,
      is_free_throw ~ 1 * predicted_prob,
      TRUE ~ 2 * predicted_prob
    )
  )
#calculate performance over expectation
shooting_plays$points_vs_expected <- shooting_plays$score_value - shooting_plays$predicted_points
team_points_vs_expected <- shooting_plays %>%
  group_by(team_id) %>%
  summarize(total_points_vs_expected = sum(points_vs_expected))
team_points_vs_expected
team_mapping <- nba_pbp %>%
  select(team_id, home_team_abbrev, home_team_id) %>%
  distinct() %>%
  filter(home_team_id == team_id)
team_points_vs_expected <- left_join(team_points_vs_expected, team_mapping, by = "team_id")
team_points_vs_expected <- team_points_vs_expected[-c(31, 32), -4]
team_points_vs_expected <- team_points_vs_expected %>%
  rename(team = home_team_abbrev)

team_expected_points <- shooting_plays %>%
  group_by(team_id) %>%
  summarize(total_expected_points = sum(predicted_points),
            expected_points_pershot = sum(predicted_points)/n())
team_expected_points
team_expected_points <- left_join(team_expected_points, team_mapping, by = "team_id")
team_expected_points <- team_expected_points[-c(31, 32), -5]
team_expected_points <- team_expected_points %>%
  rename(team = home_team_abbrev)
team_expected_points

team_points <- shooting_plays %>%
  group_by(team_id) %>%
  summarize(total_points = sum(score_value))
team_points <- team_points[-c(31, 32), ]

team_points_and_expected <- left_join(team_expected_points, team_points, by = "team_id")
#get logo urls for plotting
nba_team_box <- hoopR::load_nba_team_box()
team_logos <- nba_team_box %>%
  filter(!is.na(team_logo)) %>%
  distinct(team_id, team_logo)
team_logos <- team_logos[-c(31, 32), ]

team_points_and_expected <- left_join(team_points_and_expected, team_logos, by = "team_id")
team_points_vs_expected <- shooting_plays %>%
  group_by(team_id) %>%
  summarize(total_points_vs_expected = sum(points_vs_expected))
team_points_and_expected <- left_join(team_points_and_expected, team_points_vs_expected, by = "team_id")

read_img <- function(x) readPNG(url(x), native = TRUE)
#plot expected points vs performance over expectation
library(ggplot2)
library(ggimage)
final_plot <- ggplot(team_points_and_expected, aes(x = total_expected_points, y = total_points_vs_expected)) +
  geom_point( size = 3) +
  theme_minimal() +
  labs(title = "Expected Points v Points Over Expectation",
       x = "Expected Points",
       y = "Points Over Expectation") +
  theme(legend.position = "none")

# Add logos
final_plot <- final_plot +
  geom_image(aes(x = total_expected_points, y = total_points_vs_expected, image = team_logo),
             size = 0.08, data = team_points_and_expected)
final_plot
ggsave("~/Downloads/team_POE.png", final_plot, width = 4.25, height = 3)

team_expected_points_allowed <- shooting_plays %>%
  group_by(opponent_team_id) %>%
  summarize(total_expected_points_allowed = sum(predicted_points),
            expected_points_allowed_pershot = sum(predicted_points)/n()) %>%
  rename(team_id = opponent_team_id)
team_expected_points_allowed
team_expected_points_allowed <- left_join(team_expected_points_allowed,
                                          team_mapping, by = "team_id")
team_expected_points_allowed <- team_expected_points_allowed[-c(31, 32), -5]
team_expected_points_allowed <- team_expected_points_allowed %>%
  rename(team = home_team_abbrev)
team_expected_points_allowed

expected_point_diff <- left_join(team_expected_points, team_expected_points_allowed,
                                 by = c("team", "team_id"))
expected_point_diff <- expected_point_diff %>%
  mutate(exp_point_diff = total_expected_points - total_expected_points_allowed)

exp_points_allowed <- expected_point_diff %>%
  select(team_id, total_expected_points_allowed)
exp_points_vs_allowed <- left_join(team_points_and_expected, exp_points_allowed,
                                   by = "team_id")

point_diff_plot <- ggplot(exp_points_vs_allowed, aes(x = total_expected_points, y = total_expected_points_allowed)) +
  geom_point( size = 3) +
  theme_minimal() +
  labs(title = "Expected Points v Expected Points Allowed",
       x = "Expected Points",
       y = "Expected Points Allowed") +
  theme(legend.position = "none")
point_diff_plot <- point_diff_plot +
  geom_image(aes(x = total_expected_points, y = total_expected_points_allowed, image = team_logo),
             size = 0.08, data = exp_points_vs_allowed)

point_diff_plot
ggsave("~/Downloads/team_POEandPOEA.png", point_diff_plot, width = 4.25, height = 3)


library(stringr)
shooting_plays$player <- str_extract(shooting_plays$text, "^\\S+\\s+\\S+")
player_points_vs_expected <- shooting_plays %>%
  group_by(player) %>%
  summarize(total_points_vs_expected = sum(points_vs_expected))
player_points_vs_expected
team_mapping <- nba_pbp %>%
  select(team_id, home_team_abbrev, home_team_id) %>%
  distinct() %>%
  filter(home_team_id == team_id)
team_points_vs_expected <- left_join(team_points_vs_expected, team_mapping, by = "team_id")
team_points_vs_expected <- team_points_vs_expected[-c(31, 32), -4]
team_points_vs_expected <- team_points_vs_expected %>%
  rename(team = home_team_abbrev)

team_points_vs_expected_allowed <- shooting_plays %>%
  group_by(opponent_team_id) %>%
  summarize(total_points_vs_expected_allowed = sum(points_vs_expected)) %>%
  rename(team_id = opponent_team_id)
team_points_vs_expected_allowed
team_points_vs_expected_allowed <- left_join(team_points_vs_expected_allowed, team_mapping, by = "team_id")
team_points_vs_expected_allowed <- team_points_vs_expected_allowed[-c(31, 32), -4]
team_points_vs_expected_allowed <- team_points_vs_expected_allowed %>%
  rename(team = home_team_abbrev)

team_overunderperformance <- left_join(team_points_vs_expected_allowed, team_points_vs_expected, by = c("team_id", "team"))
team_overunderperformance <- team_overunderperformance %>%
  mutate(total_ptsoverexp = total_points_vs_expected - total_points_vs_expected_allowed,
         ptsoverexp_pergame = total_ptsoverexp / 82, 
         offense_POEPG = total_points_vs_expected / 82,
         defense_POEPG = total_points_vs_expected_allowed / 82)

standings <- hoopR::espn_nba_standings(year = 2024)
winpercent <- standings %>%
  select(team_id, winpercent)
team_overunderperformance <- left_join(team_overunderperformance, winpercent, by = "team_id")

expected_point_diff <- left_join(expected_point_diff, winpercent, by = "team_id")

team_box <- hoopR::load_nba_team_box()
first_half_team_box <- subset(team_box, game_date <= "2024-01-18")
second_half_team_box <- subset(team_box, game_date > "2024-01-19")

first_half_win_pct <- first_half_team_box %>%
  group_by(team_abbreviation) %>%
  mutate(wins = sum(team_winner == TRUE),
         losses = sum(team_winner == FALSE),
         first_win_pct = wins / (wins + losses)) %>%
  distinct(team_abbreviation, .keep_all = TRUE) %>%
  select(team_abbreviation, first_win_pct) %>%
  rename(team = team_abbreviation)

second_half_win_pct <- second_half_team_box %>%
  group_by(team_abbreviation) %>%
  filter(team_abbreviation != "WEST" & team_abbreviation != "EAST") %>%
  mutate(wins = sum(team_winner == TRUE),
         losses = sum(team_winner == FALSE),
         second_win_pct = wins / (wins + losses)) %>%
  distinct(team_abbreviation, .keep_all = TRUE) %>%
  select(team_abbreviation, second_win_pct) %>%
  rename(team = team_abbreviation)

first_half_shooting_plays <- subset(shooting_plays, game_id %in% first_half_team_box$game_id)
second_half_shooting_plays <- subset(shooting_plays, game_id %in% second_half_team_box$game_id)

first_half_POE <- first_half_shooting_plays %>%
  group_by(team_id) %>%
  summarise(first_POE = sum(points_vs_expected))

first_half_POE_allowed <- first_half_shooting_plays %>%
  group_by(opponent_team_id) %>%
  summarise(first_POE_allowed = sum(points_vs_expected)) %>%
  rename(team_id = opponent_team_id)

second_half_POE <- second_half_shooting_plays %>%
  group_by(team_id) %>%
  summarise(second_POE = sum(points_vs_expected))

second_half_POE_allowed <- second_half_shooting_plays %>%
  group_by(opponent_team_id) %>%
  summarise(second_POE_allowed = sum(points_vs_expected)) %>%
  rename(team_id = opponent_team_id)

first_half_point_diff <- first_half_team_box %>%
  group_by(team_abbreviation) %>%
  summarise(first_half_point_diff = sum(team_score) - sum(opponent_team_score)) %>%
  rename(team = team_abbreviation)

second_half_point_diff <- second_half_team_box %>%
  group_by(team_abbreviation) %>%
  summarise(second_half_point_diff = sum(team_score) - sum(opponent_team_score)) %>%
  rename(team = team_abbreviation)

team_all <- left_join(team_overunderperformance, first_half_win_pct, by = "team")
team_all <- left_join(team_all, second_half_win_pct, by = "team")
team_all <- left_join(team_all, first_half_POE, by = "team_id")
team_all <- left_join(team_all, first_half_POE_allowed, by = "team_id")
team_all <- left_join(team_all, second_half_POE, by = "team_id")
team_all <- left_join(team_all, second_half_POE_allowed, by = "team_id")
team_all <- left_join(team_all, first_half_point_diff, by = "team")
team_all <- left_join(team_all, second_half_point_diff, by = "team")
team_all <- team_all %>%
  mutate(first_POE_diff = first_POE - first_POE_allowed,
         second_POE_diff = second_POE - second_POE_allowed)

#check correlations
cor(team_overunderperformance$winpercent, team_overunderperformance$total_ptsoverexp)
cor(team_overunderperformance$winpercent, team_overunderperformance$ptsoverexp_pergame)
cor(team_overunderperformance$winpercent, team_overunderperformance$offense_POEPG)
cor(team_overunderperformance$winpercent, team_overunderperformance$defense_POEPG)
cor(expected_point_diff$winpercent, expected_point_diff$exp_point_diff)
cor(expected_point_diff$winpercent, expected_point_diff$total_expected_points)
cor(expected_point_diff$winpercent, expected_point_diff$total_expected_points_allowed)
cor(team_all$first_win_pct, team_all$second_win_pct)
cor(team_all$ptsoverexp_pergame, team_all$second_win_pct)
cor(team_all$first_POE_diff, team_all$second_win_pct)
cor(team_all$first_POE_diff, team_all$second_POE_diff)
cor(team_all$first_half_point_diff, team_all$second_win_pct)
cor(team_all$first_POE_diff, team_all$second_half_point_diff)
cor(team_all$first_half_point_diff, team_all$second_half_point_diff)
cor(team_all$first_POE_diff, team_all$second_win_pct)

plot(team_overunderperformance$ptsoverexp_pergame, team_overunderperformance$winpercent)
plot(team_all$first_POE_diff, team_all$second_win_pct)
plot(team_all$first_win_pct, team_all$second_win_pct)

#scale POEPG for predictions (must be between -1 and 1)
team_overunderperformance$scaled_POEPG <- team_overunderperformance$ptsoverexp_pergame / 10

#function to estimate win probability given matchup
game_probability <- function(team1, team2){
  team_one <- filter(team_overunderperformance, team == team1)
  team_two <- filter(team_overunderperformance, team == team2)
  prob_team1_win <- exp(team_one$scaled_POEPG - team_two$scaled_POEPG) / 
    (1 + exp(team_one$scaled_POEPG - team_two$scaled_POEPG))
  return(prob_team1_win)
}
game_probability("CHI", "MIA")

#function to simulate a series given win probability
simulate_series <- function(team1_prob){
  series <- c(0, 0)
  for (i in seq(1, 7)){
    game <- runif(1)
    if (game <= team1_prob){
      series[1] <- series[1] + 1
    } else{
      series[2] <- series[2] + 1
    }
    if (series[1] == 4 | series[2] == 4){
      return(series)
    }
  }
}

simulate_series(game_probability("DEN", "LAL"))

#function to find most likely series results
simulation_results <- function(team1, team2){
  results <- matrix(0, nrow = 10000, ncol = 2)

  for (i in 1:10000) {
    team1_prob <- game_probability(team1, team2)
    series_result <- simulate_series(team1_prob)
    results[i, ] <- series_result
  }

  results_dataframe <- as.data.frame(results)

  series_results <- results_dataframe %>%
    group_by(V1, V2) %>% 
    count()

  return(series_results)
}
simulation_results("OKC", "BOS")

#merge shooting plays w/play-by-play data so substitutions will match up
nba_pbp <- nba_pbp %>%
  left_join(shooting_plays[, c("predicted_prob", "id", 
                               "predicted_points", "points_vs_expected")], 
            by = "id")
#set all NAs to 0
nba_pbp <- nba_pbp %>%
  mutate(predicted_prob = ifelse(is.na(predicted_prob), 0, predicted_prob),
         predicted_points = ifelse(is.na(predicted_points), 0, predicted_points),
         points_vs_expected = ifelse(is.na(points_vs_expected), 0, points_vs_expected))

nba_player_box <- hoopR::load_nba_player_box()
nba_pbp$is_home_team <- ifelse(nba_pbp$team_id == nba_pbp$home_team_id, 1, 0)
nba_pbp$is_home_team[is.na(nba_pbp$is_home_team)] <- 0

nba_pbp$is_away_team <- ifelse(nba_pbp$team_id == nba_pbp$home_team_id, 0, 1)
nba_pbp$is_away_team[is.na(nba_pbp$is_away_team)] <- 0

unique_game_ids <- unique(nba_pbp$game_id)

#option A: RAPM model for expected points
# Iterate over each unique game ID
for (game_id in unique_game_ids) {
  # Subset the data for the current game ID
  game_data <- nba_pbp[nba_pbp$game_id == game_id, ]
  
  # Calculate cumulative sum of expected points, resetting at start of each game
  game_data$home_expected_score <- cumsum(game_data$is_home_team * game_data$predicted_points)
  game_data$away_expected_score <- cumsum(game_data$is_away_team * game_data$predicted_points)
  
  # Update nba_pbp with the modified data
  nba_pbp[nba_pbp$game_id == game_id, "home_expected_score"] <- game_data$home_expected_score
  nba_pbp[nba_pbp$game_id == game_id, "away_expected_score"] <- game_data$away_expected_score
}

#option B: RAPM model for points over expectation
#for (game_id in unique_game_ids) {
  # Subset the data for the current game ID
  #game_data <- nba_pbp[nba_pbp$game_id == game_id, ]
  
  # Calculate cumulative sum of expected points, resetting at start of each game
  #game_data$home_expected_score <- cumsum(game_data$is_home_team * game_data$points_vs_expected)
  #game_data$away_expected_score <- cumsum(game_data$is_away_team * game_data$points_vs_expected)
  
  # Update nba_pbp with the modified data
  #nba_pbp[nba_pbp$game_id == game_id, "home_expected_score"] <- game_data$home_expected_score
  #nba_pbp[nba_pbp$game_id == game_id, "away_expected_score"] <- game_data$away_expected_score
#}

for (game_id in unique_game_ids) {
  game_data <- nba_pbp[nba_pbp$game_id == game_id, ]
  
  home_final_exp_score <- max(game_data$home_expected_score)
  away_final_exp_score <- max(game_data$away_expected_score)
  # Update team_exp_score and opponent_exp_score based on home_away
  nba_player_box[nba_player_box$game_id == game_id & nba_player_box$home_away == "home", "team_exp_score"] <- home_final_exp_score
  nba_player_box[nba_player_box$game_id == game_id & nba_player_box$home_away == "away", "team_exp_score"] <- away_final_exp_score
  
  nba_player_box[nba_player_box$game_id == game_id & nba_player_box$home_away == "home", "opponent_exp_score"] <- away_final_exp_score
  nba_player_box[nba_player_box$game_id == game_id & nba_player_box$home_away == "away", "opponent_exp_score"] <- home_final_exp_score
}

# Identify starting players from box score                                                        
starters <- nba_player_box |>                                                                    
  dplyr::filter(starter) |>                                                                       
  dplyr::mutate(                                                                                  
    score_diff_final = (1 - 2 * (home_away == "away")) * (team_exp_score - opponent_exp_score)       
  ) |>                                                                                            
  dplyr::select(game_id, home_away, athlete_id, score_diff_final)                                 

# Extract substitutions from play-by-play data                                                    
substitutions <- nba_pbp |>                                                                      
  dplyr::filter(type_text == "Substitution") |>                                                   
  dplyr::mutate(                                                                                  
    time_start = 12 * (qtr - 1) + (11 - clock_minutes) + (60 - clock_seconds) / 60,                
    time_remaining = end_game_seconds_remaining / 60,                                             
    score_diff_start = home_expected_score - away_expected_score                                                    
  ) |>                                                                                            
  dplyr::select(game_id, time_start, time_remaining, score_diff_start, athlete_id_1, athlete_id_2)

# Create a dataframe to hold times when players come on the court (starting with the starters)    
player_on <- starters |>                                                                          
  dplyr::group_by(game_id, home_away) |>                                                          
  dplyr::mutate(index = 1:dplyr::n(), time_start = 0, score_diff_start = 0, time_remaining = NA) |>
  dplyr::ungroup()                                                                                

# Each time a substitution occurs, track the "lineup slot" into which the player enters           
for (i in 1:nrow(substitutions)) {                                                                
  
  new_player_on <- player_on |>                                                                   
    # Find the "lineup slot" based on the outgoing player                                         
    dplyr::filter(                                                                                
      game_id == substitutions$game_id[i],                                                        
      athlete_id == substitutions$athlete_id_2[i]   # this is the player coming off               
    ) |>                                                                                          
    dplyr::slice(dplyr::n()) |>                                                                   
    # Replace the information based on the incoming player, current time and current score        
    dplyr::mutate(                                                                                
      athlete_id = substitutions$athlete_id_1[i],   # this is the player coming on                
      time_start = substitutions$time_start[i],                                                   
      time_remaining = substitutions$time_remaining[i],                                           
      score_diff_start = substitutions$score_diff_start[i]                                        
    )                                                                                             
  
  player_on <- dplyr::bind_rows(player_on, new_player_on)                                         
}                                                                                                 

# If multiple substitutions happen for the same "lineup slot" without the clock time changing,    
# keep only the last player to enter the game in that "lineup slot".                              
player_on_collapsed <- player_on |>                                                               
  dplyr::group_by(                                                                                
    game_id, time_start, time_remaining, score_diff_start, score_diff_final, home_away, index     
  ) |>                                                                                            
  dplyr::summarize(athlete_id = athlete_id[dplyr::n()], .groups = "drop")                         

# Pivot wider to spread lineup slots across the columns instead of being gathered into rows       
lineups <- player_on_collapsed |>                                                                 
  dplyr::mutate(name = glue::glue("{home_away}_{index}")) |>                                      
  dplyr::select(-home_away, -index) |>                                                            
  tidyr::pivot_wider(names_from = name, values_from = athlete_id) |>                              
  # Once a player enters a lineup slot, they stay in it until someone replaces them               
  tidyr::fill(dplyr::matches("^away|^home"), .direction = "down")                                 

# Create a dataframe of athlete data                                                              
athlete <- nba_player_box |>                                                                     
  dplyr::count(athlete_id, athlete_display_name, team_abbreviation) |>                            
  # Make sure we only have one row per athlete_id                                                 
  dplyr::group_by(athlete_id) |>                                                                  
  dplyr::arrange(-n) |>                                                                           
  dplyr::slice(1) |>                                                                              
  dplyr::select(-n) |>                                                                            
  dplyr::ungroup()                                                                                

data <- lineups |>                                                                                
  dplyr::group_by(game_id) |>                                                                     
  dplyr::mutate(                                                                                  
    minutes = dplyr::coalesce(dplyr::lead(time_start, 1) - time_start, time_remaining),           
    score_diff = dplyr::coalesce(dplyr::lead(score_diff_start, 1), score_diff_final) - score_diff_start,
  ) |>                                                                                            
  dplyr::ungroup() |>                                                                             
  dplyr::filter(minutes > 1)                                                                      

data_long <- data |>                                                                              
  dplyr::mutate(stint = 1:dplyr::n()) |>  # label the stint to which this row belongs             
  dplyr::select(game_id, stint, minutes, score_diff, dplyr::matches("^away|^home")) |>            
  tidyr::pivot_longer(cols = c(dplyr::matches("^away|^home")), values_to = "athlete_id")          

head(data_long)                                                                                   

athlete_summary <- data_long |>                                                                   
  dplyr::group_by(athlete_id) |>                                                                  
  dplyr::summarize(                                                                               
    minutes = sum(minutes),                                                                       
    plus_minus = sum((1 - 2 * grepl("away", name)) * score_diff),                                 
    .groups = "drop"                                                                              
  )                                                                                               

X_data <- data_long |>                                                                            
  dplyr::mutate(                                                                                  
    row = stint,                                                                                  
    column = as.numeric(as.factor(athlete_id)),   # convert athlete_id to 1, ..., p               
    value = ifelse(substring(name, 1, 4) == "home", 1, -1)                                        
  )                                                                                               

x <- Matrix::sparseMatrix(                                                                        
  i = X_data$row,                                                                                 
  j = X_data$column,                                                                              
  x = X_data$value                                                                                
)                                                                                                 
y <- data$score_diff / data$minutes                                                               
w <- data$minutes                                                                                 

model_rapm <- glmnet::cv.glmnet(                                                                  
  x = x,                                                                                          
  y = y,                                                                                          
  weights = w,                                                                                    
  alpha = 0,                                                                                      
  standardize = FALSE                                                                             
)                                                                                                 

athlete_coef_rapm <- X_data |>                                                                    
  dplyr::distinct(column, athlete_id) |>                                                          
  dplyr::arrange(column) |>                                                                       
  dplyr::mutate(                                                                                  
    coef = coef(model_rapm, s = "lambda.min")[-1, 1]                                              
  ) |>                                                                                            
  dplyr::select(athlete_id, coef)                                                                 

player_coef <- athlete_summary |>                                                                                
  dplyr::inner_join(athlete_coef_rapm, by = "athlete_id") |>                                      
  dplyr::left_join(athlete, by = "athlete_id") |>                                                                                                     #
  dplyr::arrange(-coef)                                                                           

player_coef

