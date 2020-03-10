library(tidyverse)

#Read in .csv with sample game data
game_data <- read_csv("game_data.csv")

#PP example----

#Filter down to 1) power play strength states only and 2) corsi events or events with time only
PP <- game_data %>%
  filter(game_strength_state %in% c("5v4", "4v5") & 
           (event_length > 0 | event_type %in% c("SHOT", "GOAL", "MISS", "BLOCK")))

#Create a new set of PP variables to have that data in the same place  
#The game_strength_state variable always uses the home team's strength state first
PP <- PP %>%
  mutate(PP_1 = ifelse(game_strength_state == "5v4", home_on_1, away_on_1),
         PP_2 = ifelse(game_strength_state == "5v4", home_on_2, away_on_2),
         PP_3 = ifelse(game_strength_state == "5v4", home_on_3, away_on_3),
         PP_4 = ifelse(game_strength_state == "5v4", home_on_4, away_on_4),
         PP_5 = ifelse(game_strength_state == "5v4", home_on_5, away_on_5),
         PP_6 = ifelse(game_strength_state == "5v4", home_on_6, away_on_6),
         PP_goalie = ifelse(game_strength_state == "5v4", home_goalie, away_goalie),
         PP_team = ifelse(game_strength_state == "5v4", home_team, away_team))

#Create a new PP_line variable by concatenating the on-ice players
PP <- PP %>%
  unite(PP_line, PP_1:PP_6, sep = "-", remove = FALSE)

#Create a function to 1) remove the goalie from the power play line and 2) 
create_line <- function(line, goalie) {
  line <- str_replace_all(line, goalie, "")
  line <- str_replace_all(line, c("--" = "-", "^-" = "", "-$" = ""))
}

#Run the function
PP <- PP %>%
  mutate(PP_line = create_line(PP_line, PP_goalie))

#Create a corsi for (CF) variable to identify shot attempts
PP <- PP %>%
  mutate(CF = ifelse(event_type %in% c("GOAL","SHOT","MISS","BLOCK") 
                     & event_team == PP_team, 1, 0))

#Group by power play line, summarize the TOI and corsi for, 
#filter down to power play lines that played at least two 
#minutes, create a corsi for per 60 minutes variable, and sort
PP_group <- PP %>%
  group_by(PP_team, PP_line) %>%
  summarize(TOI = sum(event_length) / 60,
            CF = sum(CF)) %>%
  filter(TOI >= 2) %>%
  mutate(CF_60 = CF * 60 / TOI) %>%
  arrange(desc(CF_60))

#Defenseman example----

#Filter down to 5v5 events with game time only and select only certain variables
D <- game_data %>%
  filter(game_strength_state %in% c("5v5") & event_length > 0) %>%
  select(game_id, event_length, home_team, away_team, home_on_1:home_on_6, away_on_1:away_on_6)

#Pivot player names and create a new team variable
D_pivot <- D %>%
  pivot_longer(cols = home_on_1:away_on_6, names_to = "on_ice", values_to = "player") %>%
  mutate(team = ifelse(str_detect(on_ice, "home"), home_team, away_team))

#Group by game and player and summarize TOI
D_player <- D_pivot %>% 
  group_by(game_id, team, player) %>%
  summarize(TOI = sum(event_length) / 60)

#Read in position data provided from NaturalStatTrick
position_data_NST <- read_csv("position_data_NST.csv")

#Change formatting of Player variable to match what's in our event data
position_data_NST <- position_data_NST %>%
  mutate(Player = str_to_upper(Player),
         Player = str_replace(Player, " ", "."),
         Position = ifelse(Position == "D", "D", 
                           ifelse(Position == "G", "G", "F")))

#Join in the NST position data and sort by na to quickly see any problems  
D_player_join <- D_player %>%
  left_join(select(position_data_NST, Player, Position), by = c("player" = "Player")) %>%
  arrange(desc(is.na(Position)))

#Edit two names in the NST data to match our event data
position_data_NST_edited <- position_data_NST %>%
  mutate(Player = ifelse(Player == "ALEXANDER.KERFOOT", "ALEX.KERFOOT",
                         ifelse(Player == "ALEXANDER.WENNBERG", "ALEX.WENNBERG",
                                Player)))

#Join again
D_player_join <- D_player %>%
  left_join(select(position_data_NST_edited, Player, Position), 
            by = c("player" = "Player")) %>%
  arrange(desc(is.na(Position)))

#Filter by defensemen only and sort by time on ice
D_player_result <- D_player_join %>%
  filter(Position == "D") %>%
  arrange(game_id, team, desc(TOI))

#Will do the same as above, but also create a running 
#count variable and filter down to the top player 
#on each team for each game
D_player_result <- D_player_join %>%
  filter(Position == "D") %>%
  arrange(game_id, team, desc(TOI)) %>%
  group_by(game_id, team) %>%
  mutate(count = row_number()) %>%
  filter(count == 1)
