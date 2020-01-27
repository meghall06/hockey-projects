library(tidyverse)

# Read in the files
# pbp is full season scraped, rapm from Evolving-Hockey

pbp <- read_csv("pbp_201920_CBJ.csv")
rapm <- read_csv("rapm_2020_01_25.csv")

# Filter down the pbp file to 5v4/4v5 events

pbp_ST <- pbp %>%
  filter(game_strength_state %in% c("5v4","4v5") & game_period < 4 &
           (event_type %in% c("GOAL","SHOT","MISS","BLOCK") | event_length > 0))

# Create the lines

pbp_ST <- pbp_ST %>%
  mutate(PP_1 = ifelse(game_strength_state == "5v4", home_on_1, away_on_1),
         PP_2 = ifelse(game_strength_state == "5v4", home_on_2, away_on_2),
         PP_3 = ifelse(game_strength_state == "5v4", home_on_3, away_on_3),
         PP_4 = ifelse(game_strength_state == "5v4", home_on_4, away_on_4),
         PP_5 = ifelse(game_strength_state == "5v4", home_on_5, away_on_5),
         PP_6 = ifelse(game_strength_state == "5v4", home_on_6, away_on_6),
         PP_goalie = ifelse(game_strength_state == "5v4", home_goalie, away_goalie),
         PP_team = ifelse(game_strength_state == "5v4", home_team, away_team),
         PK_1 = ifelse(game_strength_state == "4v5", home_on_1, away_on_1),
         PK_2 = ifelse(game_strength_state == "4v5", home_on_2, away_on_2),
         PK_3 = ifelse(game_strength_state == "4v5", home_on_3, away_on_3),
         PK_4 = ifelse(game_strength_state == "4v5", home_on_4, away_on_4),
         PK_5 = ifelse(game_strength_state == "4v5", home_on_5, away_on_5),
         PK_goalie = ifelse(game_strength_state == "4v5", home_goalie, away_goalie),
         PK_team = ifelse(game_strength_state == "4v5", home_team, away_team))

pbp_ST <- pbp_ST %>%
  unite(PP_line, PP_1:PP_6, sep = "-", remove = FALSE) %>%
  unite(PK_line, PK_1:PK_5, sep = "-", remove = FALSE)

# Create a function to remove the goalies from the lines

create_line <- function(line, goalie) {
  line <- str_replace_all(line, goalie, "")
  line <- str_replace_all(line, c("--" = "-", "^-" = "", "-$" = ""))
}

pbp_ST <- pbp_ST %>%
  mutate(PP_line = create_line(PP_line, PP_goalie),
         PK_line = create_line(PK_line, PK_goalie))

# Pivot to get the PP_players (and remove goalies)

PP_players_long <- pbp_ST %>%
  select(PP_team, game_id, event_index, event_length, PP_goalie, PP_1:PP_6) %>%
  pivot_longer(cols = PP_1:PP_6, names_to = "number", values_to = "player") %>%
  mutate(goalie = ifelse(PP_goalie == player, 1, 0)) %>%
  filter(goalie == 0) %>%
  select(-c(PP_goalie, goalie))

# Change the one inconsistent name
# And group to get a unique list

PP_players_long <- PP_players_long %>%
  mutate(player = ifelse(player == "T.J..TYNAN", "TJ.TYNAN", player))

PP_players <- PP_players_long %>%
  group_by(PP_team, player) %>%
  summarize(n = n())

# Data cleaning for rapm data
# To match name format in pbp

rapm <- rapm %>%
  mutate(player = str_to_upper(Player),
         player = str_replace(player, " ", "."),
         position = ifelse(Position == "D", "D", "F"),
         PP_team = Team) 

# Join in the rapm values and sort by na (the ones that need to be fixed)

PP_players_orig <- PP_players %>%
  left_join(select(rapm, player, position, PP_team), by = c("player", "PP_team"))

PP_players_orig <- PP_players_orig %>%
  arrange(desc(is.na(position)))

# Fix the mismatched names

rapm <- rapm %>%
  mutate(player = ifelse(player == "ERIK.GUSTAFSSON (D)", "ERIK.GUSTAFSSON2",
                                ifelse(player == "COLIN.WHITE (C)", "COLIN.WHITE2",
                                       ifelse(player == "MACKENZIE", "MACKENZIE.MACEACHERN",
                                              ifelse(player == "NICK.SHORE" & PP_team == "TOR", "NICHOLAS.SHORE",
                                                     player)))))

# Try again!

PP_players_fixed <- PP_players %>%
  left_join(select(rapm, player, position, PP_team), by = c("player", "PP_team"))

PP_players_fixed <- PP_players_fixed %>%
  arrange(desc(is.na(position)))

# Join back into the PP_players_long

PP_players_position <- PP_players_long %>%
  left_join(select(PP_players_fixed, PP_team, player, position), by = c("PP_team", "player"))

# Create a new on-ice variable to make pivoting easier
# And translate position so that F = 1 and D = 0

PP_players_position <- PP_players_position %>%
  mutate(position = ifelse(position == "F", 1, 0)) %>%
  group_by(game_id, event_index) %>%
  mutate(count = row_number()) %>%
  select(-c(number))

# Pivot wide to get one row per event (instead of one row per player)

PP_players_position_wide <- PP_players_position %>%
  pivot_wider(names_from = "count", values_from = c("player", "position"))

# Calculate the unit type

PP_players_position_wide <- PP_players_position_wide %>%
  mutate(PP_unit_type = select(., position_1:position_5) %>% rowSums(na.rm = TRUE))

# Calculate the overall percentage, league-wide

overall_4F <- PP_players_position_wide %>%
  group_by(PP_unit_type) %>%
  summarize(TOI = sum(event_length) / 60) %>%
  mutate(percent = TOI / sum(TOI))

# Calculate the percentage by team

team_4F <- PP_players_position_wide %>%
  group_by(PP_unit_type, PP_team) %>%
  summarize(TOI = sum(event_length) / 60)

team_overall_4F <- PP_players_position_wide %>%
  group_by(PP_team) %>%
  summarize(team_TOI = sum(event_length) / 60)

team_4F <- team_4F %>%
  left_join(team_overall_4F) %>%
  mutate(percent = TOI / team_TOI)
