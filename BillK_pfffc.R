#####
# Libraries
#####
if (!require(R.utils)) install.packages("R.utils")
remotes::install_github("ricardo-bion/ggradar")

library(ggradar)
library(R.utils)
library(jsonlite)
library(tidyverse)
library("rjson")
library(purrr)

metadata <- read.csv("metadata.csv", stringsAsFactors = FALSE)
players <- read.csv("players.csv")
rosters <- read.csv("rosters.csv")

#####
# Team names for analysis
#####
teamA <- "France"
teamB <- "Argentina"

#####
# Filter DFs
#####
# Find the game IDs where teamA or teamB played. 
#For this case study, exclude the game where both teams played (the WC 2022 Final).
game_ids <- metadata %>%
  dplyr::filter(
    (grepl(teamA, awayTeam) | grepl(teamA, homeTeam) |
       grepl(teamB, awayTeam) | grepl(teamB, homeTeam)) &
      !(grepl(teamA, homeTeam) & grepl(teamB, awayTeam) |
          grepl(teamB, homeTeam) & grepl(teamA, awayTeam))
    ) %>%
  select(id)




events_data <- jsonlite::fromJSON("events.json")
events_data_filtered <- events_data %>% filter(gameId %in% game_ids$id)

#####
# Shot Data
#####

shot_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["shootingEvent"]]
})


# Initialize an empty data frame
shot_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  gameClock =  numeric(),
  formattedGameClock = character(),
  playerID = character(),
  nickname = character(),
  shotType = character(),
  saveable = character(),
  shotOutcomeType = numeric(),
  shotNatureType = character(),
  advantageType = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(shot_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(shot_data[[i]])) {
    
    # Extract the current sublist
    current <- shot_data[[i]]
    if (!is.null(current$shooterPlayer$nickname)) {
      # Combine into a temporary data frame
      temp_df <- data.frame(
        gameID = events_data_filtered$gameId[i],
        gameEventID = events_data_filtered$game_event_id[i],
        gameClock = events_data_filtered$gameClock[i],
        formattedGameClock = events_data_filtered$possessionEvents[[i]]["formattedGameClock"],
        playerID = tryCatch(current$shooterPlayer$id, error = function(e) NULL),
        nickname = tryCatch(current$shooterPlayer$nickname, error = function(e) NULL),
        shotType = if (is.null(current$shotType)) NA else current$shotType,
        saveable = if (is.null(current$saveable)) NA else current$saveable,
        shotOutcomeType = if (is.null(current$shotOutcomeType)) NA else current$shotOutcomeType,
        shotNatureType = if (is.null(current$shotNatureType)) NA else current$shotNatureType,
        advantageType = if (is.null(current$advantageType)) NA else current$advantageType,
        stringsAsFactors = FALSE
      )
      
      # Append to the result data frame
      shot_df <- rbind(shot_df, temp_df)
      
    } else {
      
      # Append to the result data frame
      shot_df <- rbind(shot_df, temp_df)
    }
  }
}

pivot_shot <- shot_df %>%
  # Filter out VAR overturned shots and shots in penalty shootouts
  filter(is.na(advantageType) & as.numeric(sub(":.*", "", formattedGameClock)) < 121) %>%
  group_by(gameID, playerID, nickname) %>%
  summarize(
    # Count occurrences
    shotOutcomeType_counts = list(table(shotOutcomeType)),
    shotType_counts = list(table(shotType)),
    .groups = "drop"
  ) %>%
  unnest_wider(shotOutcomeType_counts, names_sep = "_") %>%
  unnest_wider(shotType_counts, names_sep = "_") %>%
  # Rename columns to prefix with "shot_"
  rename_with(~ paste0("shot_", .), -c(1:3)) %>% as.data.frame()


#####
# Cross Data
#####

cross_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["crossEvent"]]
})


# Initialize an empty data frame
cross_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  playerID = character(),
  nickname = character(),
  crossOutcomeType = character(),
  cross_accuracy_type = character(),
  opportunityType = character(),
  shotOutcomeType = numeric(),
  advantageType = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(cross_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(cross_data[[i]])) {
    
    # Extract the current sublist
    current <- cross_data[[i]]
    if (!is.null(current$crosserPlayer$nickname)) {
      # Combine into a temporary data frame
      temp_df <- data.frame(
        gameID = events_data_filtered$gameId[i],
        gameEventID = events_data_filtered$game_event_id[i],
        playerID = tryCatch(current$crosserPlayer$id, error = function(e) NULL),
        nickname = tryCatch(current$crosserPlayer$nickname, error = function(e) NULL),
        crossOutcomeType = if (is.null(current$crossOutcomeType)) NA else current$crossOutcomeType,
        crossAccuracyType = if (is.null(current$cross_accuracy_type)) NA else current$cross_accuracy_type,
        opportunityType = if (is.null(current$opportunityType)) NA else current$opportunityType,
        shotOutcomeType = if (is.null(current$shotOutcomeType)) NA else current$shotOutcomeType,
        advantageType = if (is.null(current$advantageType)) NA else current$advantageType,
        stringsAsFactors = FALSE
      )
      
      # Append to the result data frame
      cross_df <- rbind(cross_df, temp_df)
    
    } else {
      
      # Append to the result data frame
      cross_df <- rbind(cross_df, temp_df)
    }
  }
}

pivot_cross <- cross_df %>%
  # Filter out VAR overturned crosses
  filter(is.na(advantageType)) %>%
  group_by(gameID, playerID, nickname) %>%
  summarize(
    # Count occurrences
    crossOutcomeType_counts = list(table(crossOutcomeType)),
    crossAccuracyType_counts = list(table(crossAccuracyType)),
    opportunityType_counts = list(table(opportunityType)),
    shotOutcomeType_counts = list(table(shotOutcomeType)),
    .groups = "drop"
  ) %>%
  unnest_wider(crossOutcomeType_counts, names_sep = "_") %>%
  unnest_wider(crossAccuracyType_counts, names_sep = "_") %>%
  unnest_wider(opportunityType_counts, names_sep = "_") %>%
  unnest_wider(shotOutcomeType_counts, names_sep = "_") %>%
  rename_with(~ paste0("cross_", .), -c(1:3)) %>% as.data.frame()

#####
# Pass Data
#####

pass_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["passingEvent"]]
})

# Initialize an empty data frame
pass_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  playerID = character(),
  nickname = character(),
  passOutcomeType = character(),
  passType = character(),
  opportunityType = character(),
  advantageType = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(pass_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(pass_data[[i]])) {
    # Extract the current sublist
    current <- pass_data[[i]]
    if (length(current$passerPlayer)>1) {
      # Combine into a temporary data frame
      temp_df <- data.frame(
        gameID = events_data_filtered$gameId[i],
        gameEventID = events_data_filtered$game_event_id[i],
        playerID = tryCatch(current$passerPlayer$id, error = function(e) NULL),
        nickname = tryCatch(current$passerPlayer$nickname, error = function(e) NULL),
        passOutcomeType = if (is.null(current$passOutcomeType)) NA else current$passOutcomeType,
        passType = if (is.null(current$passType)) NA else current$passType,
        opportunityType = if (is.null(current$opportunityType)) NA else current$opportunityType,
        advantageType = if (is.null(current$advantageType)) NA else current$advantageType,
        stringsAsFactors = FALSE
      )
      
      # Append to the result data frame
      pass_df <- rbind(pass_df, temp_df)
      
    } else {
      
    }
  }
}

pivot_pass <- pass_df %>%
  # Filter out VAR overturned crosses
  filter(is.na(advantageType)) %>%
  group_by(gameID, playerID, nickname) %>%
  summarize(
    # Count occurrences
    passOutcomeType_counts = list(table(passOutcomeType)),
    passType_counts = list(table(passType)),
    opportunityType_counts = list(table(opportunityType)),
    .groups = "drop"
  ) %>%
  unnest_wider(passOutcomeType_counts, names_sep = "_") %>%
  unnest_wider(passType_counts, names_sep = "_") %>%
  unnest_wider(opportunityType_counts, names_sep = "_") %>%
  rename_with(~ paste0("pass_", .), -c(1:3)) %>% as.data.frame()

#####
# All grades
#####

grades_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["grades"]]
})

# Initialize an empty data frame
grades_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  playerID = character(),
  nickname = character(),
  gradeLabel = character(),
  gradeStyle = character(),
  gradeType = character(),
  grade = numeric(),
  insertedAt = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(grades_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(grades_data[[i]])) {
    # Iterate through the sublists within each grades_data element
    for (j in seq_along(grades_data[[i]])) {
      # Extract the current sublist
      current <- grades_data[[i]][[j]]
      
      playerGrade <- tryCatch(current[["playerGrade"]], error = function(e) NULL)
      player <- tryCatch(current[["player"]], error = function(e) NULL)
      gradeLabel <- tryCatch(current[["gradeLabel"]], error = function(e) NA)
      gradeStyle <- tryCatch(current[["gradeStyle"]], error = function(e) NA)
      gradeType <- tryCatch(current[["gradeType"]], error = function(e) NA)
      insertedAt <- tryCatch(current[["insertedAt"]], error = function(e) NA)
      
      # Handle cases where playerGrade and player are not available
      if (!is.null(playerGrade) && !is.null(player) && !all(is.na(player))) {
        # Extract player fields
        nicknames <- player[["nickname"]]
        ids <- player[["id"]]
        
        # Ensure playerGrade and player fields have consistent lengths
        if (length(playerGrade) == length(nicknames) && length(playerGrade) == length(ids)) {
          # Combine into a temporary data frame
          temp_df <- data.frame(
            gameID = events_data_filtered$gameId[i],
            gameEventID = events_data_filtered$game_event_id[i],
            playerID = ids,
            nickname = nicknames,
            gradeLabel = rep(gradeLabel, length(playerGrade)),
            gradeStyle = rep(gradeStyle, length(playerGrade)),
            gradeType = rep(gradeType, length(playerGrade)),
            grade = playerGrade,
            stringsAsFactors = FALSE
          )
          
          # Append to the result data frame
          grades_df <- rbind(grades_df, temp_df)
        }
      } else {
        
        # Append to the result data frame
        grades_df <- rbind(grades_df, temp_df)
      }
    }
  }
}


pivot_grades <- grades_df %>%
  group_by(gameID, playerID, nickname, gradeType) %>%
  summarize(total_grade = sum(grade, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = gradeType,
    values_from = total_grade
  ) %>%
  rename_with(~ paste0("grade_", .), -c(1:3))

#####
# Fouls
#####

fouls_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["fouls"]]
})

# Initialize an empty data frame
fouls_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  culprit_player_id = integer(),
  culprit_player_nickname = character(),
  var_culprit_player_id = integer(),
  var_culprit_player_nickname = character(),
  victim_player_id = integer(),
  victim_player_nickname = character(),
  foul_outcome_type = character(),
  var_outcome_type = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(fouls_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(fouls_data[[i]])) {
    # Iterate through the sublists within each grades_data element
    for (j in seq_along(fouls_data[[i]])) {
      # Extract the current sublist
      current <- fouls_data[[i]][[j]]
  
      if(length(current)>0) {
        
      # Combine into a temporary data frame
      temp_df <- data.frame(
        gameID = events_data_filtered$gameId[i],
        gameEventID = events_data_filtered$game_event_id[i],
        culprit_player_id = tryCatch(current$culpritPlayer$id, error = function(e) NA),
        culprit_player_nickname = tryCatch(current$culpritPlayer$nickname, error = function(e) NA),
        var_culprit_player_id =  tryCatch(current$varCulpritPlayer$id, error = function(e) NA),
        var_culprit_player_nickname = tryCatch(current$varCulpritPlayer$nickname, error = function(e) NA),
        victim_player_id = tryCatch(current$victimPlayer$id, error = function(e) NA),
        victim_player_nickname = tryCatch(current$victimPlayer$nickname, error = function(e) NA),
        foul_outcome_type = tryCatch(current$foulOutcomeType, error = function(e) NA),
        var_outcome_type = tryCatch(current$varOutcomeType, error = function(e) NA),
        stringsAsFactors = FALSE
      )
      
      # Append to the result data frame
      fouls_df <- rbind(fouls_df, temp_df)
      }
    }
  } else {}
}


pivot_fouls_culprit <- fouls_df %>%
  # exclude fouls overturned by var
  filter(is.na(var_outcome_type)) %>%
  group_by(gameID, "playerID"=culprit_player_id, "nickname"=culprit_player_nickname, foul_outcome_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = foul_outcome_type,
    values_from = count,
    values_fill = list(count = 0)) %>%
  rename_with(~ paste0("foul_commit_", .), -c(1:3))

pivot_fouls_victim <- fouls_df %>%
  # exclude fouls overturned by var
  filter(is.na(var_outcome_type)) %>%
  group_by(gameID, "playerID"=victim_player_id, "nickname"=victim_player_nickname, foul_outcome_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = foul_outcome_type,
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  rename_with(~ paste0("foul_won_", .), -c(1:3))

#####
# Challenges
#####
challenge_data <- lapply(events_data_filtered$possessionEvents, function(event) {
  event[["challengeEvent"]]
})

# Initialize an empty data frame
challenge_df <- data.frame(
  gameID = integer(),
  gameEventID = character(),
  ball_carrier_player_id = integer(),
  ball_carrier_player_nickname = character(),
  challenger_player_id = integer(),
  challenger_player_nickname = character(),
  challenge_winner_player_id = integer(),
  challenge_winner_player_nickname = character(),
  challenge_type = character(),
  challenge_outcome_type = character(),
  trick_type = character(),
  stringsAsFactors = FALSE
)

# Iterate through all grades_data elements
for (i in seq_along(challenge_data)) {
  # Check if the current grades_data element contains sublists
  if (is.list(challenge_data[[i]])) {
    # Iterate through the sublists within each grades_data element
    for (j in seq_along(challenge_data[[i]])) {
      # Extract the current sublist
      current_df <- challenge_data[[i]]
      current <- current_df[j,]
      if(length(current)>0) {
        
        # Create temporary data frame
        temp_df <- data.frame(
          gameID = events_data_filtered$gameId[i],
          gameEventID = events_data_filtered$game_event_id[i],
          ball_carrier_player_id = tryCatch(current$ballCarrierPlayer$id, error = function(e) NA),
          ball_carrier_player_nickname = tryCatch(current$ballCarrierPlayer$nickname, error = function(e) NA),
          challenger_player_id = tryCatch(current$challengerPlayer$id, error = function(e) NA),
          challenger_player_nickname = tryCatch(current$challengerPlayer$nickname, error = function(e) NA),
          challenge_winner_player_id = tryCatch(current$challengeWinnerPlayer$id, error = function(e) NA),
          challenge_winner_player_nickname = tryCatch(current$challengeWinnerPlayer$nickname, error = function(e) NA),
          challenge_outcome_type = tryCatch(current$challengeOutcomeType, error = function(e) NA),
          challenge_type = tryCatch(current$challengeType, error = function(e) NA),
          trick_type = tryCatch(current$trickType, error = function(e) NA),
          stringsAsFactors = FALSE
        )
        
        # Append to the result data frame
        challenge_df <- rbind(challenge_df, temp_df)
      }
    }
  } else {}
}

challenge_df <- challenge_df %>%
  mutate(ball_carrier_wins_challenge = ifelse(ball_carrier_player_id == challenge_winner_player_id, 1, 0))
challenge_df <- challenge_df %>%
  mutate(challenger_wins_challenge = ifelse(challenger_player_id == challenge_winner_player_id, 1, 0))
challenge_df$trick_type <- tolower(challenge_df$trick_type)

pivot_ball_carrier <- challenge_df %>%
  filter(!is.na(ball_carrier_wins_challenge)) %>%
  group_by(gameID, "playerID"=ball_carrier_player_id, "nickname"=ball_carrier_player_nickname) %>%
  summarize(
    # Count occurrences
    challenge_outcome_counts = list(table(challenge_outcome_type)),
    challenge_type_counts = list(table(challenge_type)),
    trick_type_counts = list(table(trick_type)),
    won_carry_challenges = sum(ball_carrier_wins_challenge),
    lost_carry_challenges = n()-sum(ball_carrier_wins_challenge),
    .groups = "drop"
  ) %>%
  unnest_wider(challenge_outcome_counts, names_sep = "_") %>%
  unnest_wider(challenge_type_counts, names_sep = "_") %>%
  unnest_wider(trick_type_counts, names_sep = "_") %>%
  rename_with(~ paste0("ball_carrier_", .), -c(1:3))

pivot_challenger <- challenge_df %>%
  filter(!is.na(challenger_wins_challenge)) %>%
  group_by(gameID, "playerID"=challenger_player_id, "nickname"=challenger_player_nickname) %>%
  summarize(
    challenge_outcome_counts = list(table(challenge_outcome_type)),
    challenge_type_counts = list(table(challenge_type)),
    won_def_challenges = sum(challenger_wins_challenge),
    lost_def_challenges = n()-sum(challenger_wins_challenge),
    .groups = "drop"
  ) %>%
  unnest_wider(challenge_outcome_counts, names_sep = "_") %>%
  unnest_wider(challenge_type_counts, names_sep = "_") %>%
  rename_with(~ paste0("challenger_", .), -c(1:3))

#####
# Sub Data
#####
playerOn <- events_data_filtered %>% select(gameId, gameClock, playerOn)
playerOn$id <- playerOn$playerOn$id
playerOn <- playerOn[!is.na(playerOn$playerOn$id), ]

playerOff <- events_data_filtered %>% filter(!is.na(playerOn$id) & (playerOn$id != playerOff$id)) %>% select(gameId, gameClock, playerOff)
playerOff <- playerOff[!is.na(playerOff$playerOff), ]
playerOff$id <- playerOff$playerOff$id

gameSubs <- merge(
  playerOn, 
  playerOff, 
  by = c("gameId", "gameClock")
)


#####
# Merge all player data
#####

team_roster <- rosters %>% select(player, team) %>% unique()

# Replace single quotes and extract IDs
team_roster$playerId <- as.numeric(gsub('.*"id":\\s*"(\\d+).*', '\\1', gsub("'", "\"", team_roster$player)))
team_roster$teamId <- as.numeric(gsub('.*"id":\\s*"(\\d+).*', '\\1', gsub("'", "\"", team_roster$team)))
team_roster$teamName <- gsub('.*"name":\\s*"(.*?)".*', '\\1', gsub("'", "\"", team_roster$team))

players_new <- merge(players, team_roster, by.x="id", by.y= "playerId") %>% unique()

players_filtered <- players_new %>% filter(teamName == teamA | teamName == teamB)
players_filtered <- players_filtered %>%
  mutate(fantasy_position = case_when(
    positionGroupType == "GK" ~ "GK",
    positionGroupType == "D" ~ "D",
    positionGroupType == "CM" ~ "M",
    positionGroupType == "M" ~ "M",
    positionGroupType == "CF" ~ "F",
    positionGroupType == "F" ~ "F",
    TRUE ~ NA_character_
  ))

players_filtered_for_stats_table <- players_filtered %>% select(id, fantasy_position, teamId, teamName)

# List of data frames
pivot_list <- list(pivot_shot, pivot_pass, pivot_cross, pivot_ball_carrier, pivot_challenger, pivot_fouls_culprit, pivot_fouls_victim, pivot_grades)

# Join all data frames
player_stats <- pivot_list %>%
  reduce(full_join, by = c("gameID", "playerID", "nickname"))

player_stats <- merge(player_stats, players_filtered_for_stats_table, by.x=c("playerID"), by.y=c("id"))

player_fantasy_stats <- player_stats %>% select(
  gameID, playerID, nickname, teamName, fantasy_position,
  "off_goals_scored"=shot_shotOutcomeType_counts_G,
  "off_shots_saved_on_target"=shot_shotOutcomeType_counts_S,
  "off_shots_goalline_clearance"=shot_shotOutcomeType_counts_L,
  "off_shot_bicycle_kick"=shot_shotType_counts_B,
  "off_pass_completed"=pass_passOutcomeType_counts_C,
  "off_pass_opp_chance_created"=pass_opportunityType_counts_C,
  "off_pass_opp_dangerous"=pass_opportunityType_counts_D,
  "off_pass_opp_half_chance"=pass_opportunityType_counts_H,
  "off_pass_opp_negative_chance"=pass_opportunityType_counts_N,
  "off_pass_opp_negative_dangerous_position"=pass_opportunityType_counts_P,
  "off_cross_completed"=cross_crossOutcomeType_counts_C,
  "off_cross_opp_chance_created"=cross_opportunityType_counts_C,
  "off_cross_opp_dangerous"=cross_opportunityType_counts_D,
  "off_cross_opp_half_chance"=cross_opportunityType_counts_H,
  "off_carry_nutmeg"=ball_carrier_trick_type_counts_nutmeg,
  "off_carry_roulette"=ball_carrier_trick_type_counts_roulette,
  "off_carry_won_challenges"=ball_carrier_won_carry_challenges,
  "off_carry_lost_challenges"=ball_carrier_lost_carry_challenges,
  "off_fouls_won_warning"=foul_won_W,
  "off_fouls_won_yellow_first"=foul_won_Y,
  "off_fouls_won_yellow_second"=foul_won_S,
  "def_challenge_won_def_challenges"=challenger_won_def_challenges,
  "def_challenge_lost_def_challenges"=challenger_lost_def_challenges,
  "def_fouls_commit_warning"=foul_commit_W,
  "def_fouls_commit_yellow_first"=foul_commit_Y,
  "def_fouls_commit_yellow_second"=foul_commit_S,
  "grade_default"=grade_default,
  "grade_sub"=grade_sub,
)

player_fantasy_stats[is.na(player_fantasy_stats)] <- 0
player_fantasy_stats <- player_fantasy_stats %>%
  mutate(across(where(~ inherits(., "table")), ~ as.numeric(as.character(unlist(.)))))


fantasy_point_values_defender = c(
  "off_goals_scored"=5,
  "off_shots_saved_on_target"=0.75,
  "off_shots_goalline_clearance"=1.5,
  "off_shot_bicycle_kick"=.5,
  "off_pass_completed"=0.05,
  "off_pass_opp_chance_created"=1,
  "off_pass_opp_dangerous"=0.6,
  "off_pass_opp_half_chance"=0.3,
  "off_pass_opp_negative_chance"=-0.4,
  "off_pass_opp_negative_dangerous_position"=-0.8,
  "off_cross_completed"=0.1,
  "off_cross_opp_chance_created"=0.3,
  "off_cross_opp_dangerous"=0.2,
  "off_cross_opp_half_chance"=0.1,
  "off_carry_nutmeg"=0.5,
  "off_carry_roulette"=0.5,
  "off_carry_won_challenges"=0.1,
  "off_carry_lost_challenges"=-0.1,
  "off_fouls_won_warning"=0.2,
  "off_fouls_won_yellow_first"=0.4,
  "off_fouls_won_yellow_second"=0.8,
  "def_challenge_won_def_challenges"=0.2,
  "def_challenge_lost_def_challenges"=-0.2,
  "def_fouls_commit_warning"=-0.5,
  "def_fouls_commit_yellow_first"=-1,
  "def_fouls_commit_yellow_second"=-3,
  "grade_default"=0.1,
  "grade_sub"=0.1
)

fantasy_point_values_midfielder = c(
  "off_goals_scored"=5,
  "off_shots_saved_on_target"=0.75,
  "off_shots_goalline_clearance"=1.5,
  "off_shot_bicycle_kick"=.5,
  "off_pass_completed"=0.05,
  "off_pass_opp_chance_created"=1.2,
  "off_pass_opp_dangerous"=0.7,
  "off_pass_opp_half_chance"=0.3,
  "off_pass_opp_negative_chance"=-0.4,
  "off_pass_opp_negative_dangerous_position"=-0.7,
  "off_cross_completed"=0.15,
  "off_cross_opp_chance_created"=0.3,
  "off_cross_opp_dangerous"=0.2,
  "off_cross_opp_half_chance"=0.1,
  "off_carry_nutmeg"=0.5,
  "off_carry_roulette"=0.5,
  "off_carry_won_challenges"=0.15,
  "off_carry_lost_challenges"=-0.1,
  "off_fouls_won_warning"=0.3,
  "off_fouls_won_yellow_first"=0.6,
  "off_fouls_won_yellow_second"=1,
  "def_challenge_won_def_challenges"=0.3,
  "def_challenge_lost_def_challenges"=-0.3,
  "def_fouls_commit_warning"=-0.5,
  "def_fouls_commit_yellow_first"=-1,
  "def_fouls_commit_yellow_second"=-3,
  "grade_default"=0.1,
  "grade_sub"=0.1
)

fantasy_point_values_forward = c(
  "off_goals_scored"=4,
  "off_shots_saved_on_target"=0.5,
  "off_shots_goalline_clearance"=1.5,
  "off_shot_bicycle_kick"=.5,
  "off_pass_completed"=0.05,
  "off_pass_opp_chance_created"=1,
  "off_pass_opp_dangerous"=0.6,
  "off_pass_opp_half_chance"=0.2,
  "off_pass_opp_negative_chance"=-0.3,
  "off_pass_opp_negative_dangerous_position"=-0.6,
  "off_cross_completed"=0.1,
  "off_cross_opp_chance_created"=0.3,
  "off_cross_opp_dangerous"=0.2,
  "off_cross_opp_half_chance"=0.1,
  "off_carry_nutmeg"=0.5,
  "off_carry_roulette"=0.5,
  "off_carry_won_challenges"=0.1,
  "off_carry_lost_challenges"=-0.1,
  "off_fouls_won_warning"=0.3,
  "off_fouls_won_yellow_first"=0.6,
  "off_fouls_won_yellow_second"=1,
  "def_challenge_won_def_challenges"=0.3,
  "def_challenge_lost_def_challenges"=-0.1,
  "def_fouls_commit_warning"=-0.5,
  "def_fouls_commit_yellow_first"=-1,
  "def_fouls_commit_yellow_second"=-3,
  "grade_default"=0.1,
  "grade_sub"=0.1
)

player_fantasy_stats <- player_fantasy_stats %>%
  rowwise() %>%
  mutate(fpt = case_when(
    fantasy_position == "GK" ~ sum(c_across(all_of(names(fantasy_point_values_defender))) *
                                    fantasy_point_values_defender, na.rm = TRUE),
    fantasy_position == "D" ~ sum(c_across(all_of(names(fantasy_point_values_defender))) *
                                    fantasy_point_values_defender, na.rm = TRUE),
    fantasy_position == "M" ~ sum(c_across(all_of(names(fantasy_point_values_midfielder))) *
                                    fantasy_point_values_midfielder, na.rm = TRUE),
    fantasy_position == "F" ~ sum(c_across(all_of(names(fantasy_point_values_forward))) *
                                    fantasy_point_values_forward, na.rm = TRUE),
    TRUE ~ 0
  )) %>%
  ungroup()


#####
# Set player prices
#####
unique(player_fantasy_stats$nickname)
fantasy_prices <- data.frame(
  nickname=c("Julian Alvarez","Jordan Veretout","Nicolás Otamendi","Olivier Giroud","Enzo Fernandez","Nahuel Molina",
            "German Pezzella", "Angel Correa", "Franco Armani", "Emiliano Martínez", "Lionel Messi", "Ousmane Dembele",
            "Antoine Griezmann", "Mattéo Guendouzi", "Raphael Varane", "Geronimo Rulli", "Jules Kounde", "Guido Rodríguez", 
            "William Saliba", "Leandro Paredes", "Kylian Mbappé", "Axel Disasi", "Aurélien Tchouaméni", "Youssouf Fofana",
            "Steve Mandanda", "Eduardo Camavinga", "Randal Kolo Muani", "Exequiel Palacios", "Dayot Upamecano", "Lucas Hernández",
            "Benjamin Pavard", "Kingsley Coman", "Marcus Thuram", "Ibrahima Konaté", "Cristian Romero", "Hugo Lloris",
            "Juan Foyth", "Rodrigo de Paul", "Thiago Almada", "Lisandro Martinez", "Nicolas Tagliafico", "Adrien Rabiot", "Paulo Dybala",
            "Lautaro Martinez","Gonzalo Montiel", "Papu Gómez", "Theo Hernandez", "Marcos Acuña", 
            "Angel Di Maria", "Alexis MacAllister", "Andries Noppert"),
  price=c(6.5, 6.5, 5, 7.5, 6, 5.5,
          4, 4, 5, 6, 10.5,
          8, 8.5, 6, 6, 4.5, 6, 4.5,
          5, 4.5, 11.5, 4.5, 6.5, 5,
          4.5, 7, 7, 6.5, 5.5, 5.5, 
          5, 8, 7, 5.5, 5.5, 6, 
          4.5, 5, 5.5, 6, 5.5, 6.5, 8,
          8, 5, 6.5, 6, 4.5,
          8.5, 7, 4.5))

player_fantasy_stats <- merge(player_fantasy_stats, fantasy_prices, by="nickname")
player_fantasy_stats$fpt_per_price <- round(player_fantasy_stats$fpt/player_fantasy_stats$price,2)

game_round <- metadata %>% select("gameID"=id, "round"=week)
player_fantasy_stats <- merge(player_fantasy_stats, game_round, by="gameID")

player_fantasy_stats_overall <- player_fantasy_stats %>%
  group_by(playerID, nickname, price, "position"=fantasy_position) %>%  # Include price in the grouping
  summarize(
    games_played = n(),
    fpt_total = round(sum(fpt, na.rm = TRUE), 2),  # Sum and round to 2 digits
    fpt_avg = round(mean(fpt, na.rm = TRUE), 2),   # Mean and round to 2 digits
    .groups = "drop"
  ) %>%
  mutate(
    fpt_total_per_price = round(fpt_total / price, 2),  # Calculate per price and round
    fpt_avg_per_price = round(fpt_avg / price, 2)  # Calculate per price and round
  ) %>%
  arrange(desc(fpt_total))  # Sort by fpt_total in descending order


#####
# Charts
#####
# Extract the top 5 players
top_5_players <- player_fantasy_stats_overall %>%
  slice_max(order_by = fpt_total, n = 5) %>%
  pull(playerID)

# Filter the original data for the top 5 players
filtered_stats <- player_fantasy_stats %>%
  filter(playerID %in% top_5_players)
# Find the last round for each player
end_labels <- filtered_stats %>%
  group_by(playerID) %>%
  filter(round == max(round)) %>%
  ungroup()
library(ggrepel)
# Create the line graph
ggplot(filtered_stats, aes(x = round, y = fpt, group = playerID, color = nickname)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  geom_text_repel(data = end_labels, aes(label = nickname), 
                  #direction = "y",  # Adjust label position vertically
                  hjust = 0.5, 
                  vjust = 0.5, 
                  nudge_x = 0.2,    # Nudge labels horizontally to avoid overlap
                  size = 3, 
                  show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(min(filtered_stats$round), max(filtered_stats$round), by = 1)) +  # Show all rounds
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_stats$fpt)), by = 1)) +  # Add horizontal lines at each 1 FPT
  
  labs(
    title = "Fantasy Points by Round for Top 5 Fantasy Players",
    x = "Round",
    y = "Fantasy Points (FPT)",
    color = "Player"
  ) +
  theme_minimal(base_size = 12) +  # Use a minimal base theme
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 18),  # Center and bold title
    legend.position = "none",  # Hide the legend
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.8, color = "black"),  # Add axis lines
    panel.grid.major = element_line(size = 0.2, linetype = "dashed", color = "gray"),
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_color_brewer(palette = "Dark2")  # Use a nicer color palette

#####
# VFM Options
#####
# Extract the top 5 players
top_5_vfm <- player_fantasy_stats_overall %>%
  filter(price<8 & games_played >2) %>%
  slice_max(order_by = fpt_avg, n = 5) %>%
  pull(playerID)

# Filter the original data for the top 5 players
filtered_stats <- player_fantasy_stats %>%
  filter(playerID %in% top_5_vfm)
# Find the last round for each player
end_labels <- filtered_stats %>%
  group_by(playerID) %>%
  filter(round == max(round)) %>%
  ungroup()
# Create the line graph
ggplot(filtered_stats, aes(x = round, y = fpt, group = playerID, color = nickname)) +
  geom_line(size = 0.3) +
  geom_point(size = 2) +
  geom_text_repel(data = end_labels, aes(label = nickname), 
                  #direction = "y",  # Adjust label position vertically
                  hjust = 0.5, 
                  vjust = 0.5, 
                  nudge_x = 0.2,    # Nudge labels horizontally to avoid overlap
                  size = 3, 
                  show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(min(filtered_stats$round), max(filtered_stats$round), by = 1)) +  # Show all rounds
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_stats$fpt)), by = 1)) +  # Add horizontal lines at each 1 FPT
  labs(
    title = "Fantasy Points by Round for Top 5 Value for Money Players",
    x = "Round",
    y = "Fantasy Points (FPT)",
    color = "Player"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 18), 
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.8, color = "black"),  
    panel.grid.major = element_line(size = 0.2, linetype = "dashed", color = "gray"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Dark2")

#####
# bubble chart
#####

ggplot(player_fantasy_stats_overall, aes(x = price, y = fpt_avg, size = games_played, color = position, label = nickname)) +
  geom_point(alpha = 0.4) + 
  geom_text_repel(size = 2.5, show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) + 
  scale_size_continuous(range = c(1, 6)) +  # Adjust bubble size range
  labs(
    title = "Price vs Average FPT by Position up to the Final",
    x = "Price",
    y = "Average FPT",
    size = "Games Played",
    color = "Position"
  ) +
  theme_minimal(base_size = 12) +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

