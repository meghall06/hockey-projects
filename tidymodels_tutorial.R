library(tidymodels)
library(tidyverse)

# Data prep----
# (aka the funnest part)

# Read in files (pbp from Evolving Hockey, bios from Natural Stat Trick)

pbp <- read_csv("pbp_20182019_all.csv")
bios <- read_csv("bios_1819.csv")

# Find player TOI and games played
# To do so, you must pivot the data so there is one row per player
# (instead of one row per event)
# We don't care about the ice time for the goalies (sorry, goalies)
# so they will be filtered out
# We also do some name changes to make things easier later

player_TOI <- pbp %>%
  filter(event_length > 0) %>%
  select(game_id, event_length, home_on_1:away_goalie) %>%
  pivot_longer(home_on_1:away_on_6, names_to = "variable", values_to = "player") %>%
  filter(!(is.na(player)) & player != home_goalie & player != away_goalie) %>%
  mutate(player = case_when(
    player == "COLIN.WHITE2" ~ "COLIN.WHITE",
    player == "ERIK.GUSTAFSSON2" ~ "ERIK.GUSTAFSSON",
    player == "PATRICK.MAROON" ~ "PAT.MAROON",
    TRUE ~ player
  )) %>%
  group_by(player) %>%
  summarize(games = n_distinct(game_id),
            TOI = sum(event_length) / 60)

# Find basic player stats
# To find individual stats, we again need to pivot the data to one row per player
# but we're using the event_players only (not the on ice players)
# You'll notice we're filtering out the shootout (which is game_period 5) because
# those goals don't count
# We'll sum up blocked shots (event_player_2 is the player who blocked the shot,
# event_player_1 is the one who generated it), total points, shots, unblocked shots,
# hits (both give nand received)

player_stats <- pbp %>%
  filter(event_type %in% c("HIT", "BLOCK", "SHOT", "MISS", "GOAL") & game_period < 5) %>%
  select(game_id, event_type, event_player_1:event_player_3) %>%
  pivot_longer(event_player_1:event_player_3, names_to = "number", values_to = "player") %>%
  filter(!(is.na(player))) %>%
  mutate(block = ifelse(event_type == "BLOCK" & number == "event_player_2", 1, 0),
         point = ifelse(event_type == "GOAL", 1, 0),
         shot = ifelse(number == "event_player_1" & event_type %in% c("SHOT", "GOAL"), 1, 0),
         fenwick = ifelse(number == "event_player_1" & event_type %in% c("SHOT", "GOAL", "MISS"), 1, 0),
         hit = ifelse(number == "event_player_1" & event_type == "HIT", 1, 0),
         hit_rec = ifelse(number == "event_player_2" & event_type == "HIT", 1, 0),
         player = case_when(
           player == "COLIN.WHITE2" ~ "COLIN.WHITE",
           player == "ERIK.GUSTAFSSON2" ~ "ERIK.GUSTAFSSON",
           player == "PATRICK.MAROON" ~ "PAT.MAROON",
           TRUE ~ player
         )) %>%
  group_by(player) %>%
  summarize(blocks = sum(block),
            points = sum(point),
            shots = sum(shot),
            fenwick = sum(fenwick),
            hits = sum(hit),
            hits_rec = sum(hit_rec))

# Join stats into TOI data frame and create rates

player_TOI_stats <- player_TOI %>%
  left_join(player_stats, by = "player") %>%
  mutate(points_60 = points * 60 / TOI,
         shots_60 = shots * 60 / TOI,
         fenwick_60 = fenwick * 60 / TOI,
         hits_60 = hits * 60 / TOI,
         hits_rec_60 = hits_rec * 60 / TOI,
         blocks_60 = blocks * 60 / TOI,
         TOI_game = TOI / games) %>%
  select(-c(blocks:hits_rec))

# Clean up the biographical data

bios <- bios %>%
  mutate(player = str_to_upper(Player),
         player = str_replace(player, " ", "."),
         defense = ifelse(Position == "D", "D", "F")) %>%
  rename(height = `Height (in)`,
         weight = `Weight (lbs)`) %>%
  select(player, defense, height, weight) %>%
  mutate(player = str_replace_all(player, "ALEXANDER", "ALEX"),
         player = str_replace_all(player, "ALEXANDRE", "ALEX"),
         player = case_when(
           player == "CHRISTOPHER.TANEV" ~ "CHRIS.TANEV",
           player == "DANNY.O'REGAN" ~ "DANIEL.O'REGAN",
           player == "EVGENII.DADONOV" ~ "EVGENY.DADONOV",
           player == "MATTHEW.BENNING" ~ "MATT.BENNING",
           player == "MITCHELL.MARNER" ~ "MITCH.MARNER",
           TRUE ~ player
         ))

# Join biographical data into stats data
# Filter to only keep players who played at least 20 games

final_data <- player_TOI_stats %>%
  left_join(bios, by = "player") %>%
  filter(games > 19)

# Make some graphs!----

final_data %>%
  ggplot(aes(x = weight, fill = defense)) +
  geom_density(alpha = 0.7, color = NA) +
  scale_fill_manual(values = c("#0d324d", "#a188a6")) +
  labs(
    y = "Density",
    x = "Weight (lbs)",
    fill = NULL,
    title = "Weight by Position",
    subtitle = "2018-19 NHL Season, 20+ Games Played Only",
    caption = "Source: Natural Stat Trick"
  ) +
  meg_theme() + 
  theme(legend.position = c(0.9, 0.9))

final_data %>%
  ggplot(aes(x = points_60, fill = defense)) +
  geom_density(alpha = 0.7, color = NA) +
  scale_fill_manual(values = c("#0d324d", "#a188a6")) +
  labs(
    y = "Density",
    x = "Points Per 60 Minutes",
    fill = NULL,
    title = "Scoring Rate by Position",
    subtitle = "2018-19 NHL Season, 20+ Games Played Only",
    caption = "Source: Evolving Hockey R Scraper"
  ) +
  meg_theme() + 
  theme(legend.position = c(0.9, 0.9))

final_data %>%
  ggplot(aes(x = TOI_game, fill = defense)) +
  geom_density(alpha = 0.7, color = NA) +
  scale_fill_manual(values = c("#0d324d", "#a188a6")) +
  labs(
    y = "Density",
    x = "Average Time on Ice",
    fill = NULL,
    title = "Time on Ice by Position",
    subtitle = "2018-19 NHL Season, 20+ Games Played Only",
    caption = "@MeghanMHall\nData: Evolving Hockey R Scraper"
  ) +
  meg_theme() + 
  theme(legend.position = c(0.9, 0.9))

final_data %>%
  ggplot(aes(x = blocks_60, fill = defense)) +
  geom_density(alpha = 0.7, color = NA) +
  scale_fill_manual(values = c("#0d324d", "#a188a6")) +
  labs(
    y = "Density",
    x = "Blocked Shots Per 60 Minutes",
    fill = NULL,
    title = "Blocked Shot Rate by Position",
    subtitle = "2018-19 NHL Season, 20+ Games Played Only",
    caption = "@MeghanMHall\nData: Evolving Hockey R Scraper"
  ) +
  meg_theme() + 
  theme(legend.position = c(0.9, 0.9))

final_data %>%
  ggplot(aes(defense)) +
  geom_bar() +
  labs(
    y = "Count",
    x = "Position",
    title = "How Many Forwards Compared to Defensemen?",
    subtitle = "2018-19 NHL Season, 20+ Games Played Only",
    caption = "@MeghanMHall\nData: Evolving Hockey R Scraper"
  ) +
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 3) +
  meg_theme()

# Rearrange our model data

model_data <- final_data %>%
  select(player, defense, everything(), -c(games, TOI))

# Set the seed (very useful for reproducible samples!)

set.seed(1234)

# Split into training and testing data

split_data <- initial_split(model_data, prop = 0.6, strata = defense)

# Create our testing and training data sets

training_data <- training(split_data)
testing_data <- testing(split_data)

# Write the recipe for our small TOI_only model

recipe_TOI_only <- training_data %>%
  recipe(defense ~ TOI_game) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

recipe_TOI_only

# Extract our prepped training data 
# and "bake" our testing data

training_baked_TOI <- juice(recipe_TOI_only)

testing_baked_TOI <- recipe_TOI_only %>%
  bake(testing_data) 

# Run the TOI model with our training data----

logistic_glm_TOI <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(defense ~ ., data = training_baked_TOI)

# Find the class predictions from our testing data
# And add back in the true values from testing data

predictions_class_TOI <- logistic_glm_TOI %>%
  predict(new_data = testing_baked_TOI) %>%
  bind_cols(testing_baked_TOI %>% select(defense))

# Find the probability predictions
# And add all together

predictions_TOI <- logistic_glm_TOI %>%
  predict(testing_baked_TOI, type = "prob") %>%
  bind_cols(predictions_class_TOI)

# Create a confusion matrix

predictions_TOI %>%
  conf_mat(defense, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8) +
  meg_theme() +
  theme(panel.grid.major = element_blank()) +
  labs(
    y = "Actual Position",
    x = "Predicted Position",
    fill = NULL,
    title = "Confusion Matrix",
    subtitle = "TOI Only Model"
  )

# Find the accuracy

predictions_TOI %>%
  accuracy(defense, .pred_class) 

# Find the logloss

predictions_TOI %>%
  mn_log_loss(defense, .pred_D)

# Find the area under the ROC curve (AUC)

predictions_TOI %>%
  roc_auc(defense, .pred_D)

# Create a tibble that holds all the evaluation metrics

TOI_metrics <- tibble(
  "log_loss" = 
    mn_log_loss(predictions_TOI, defense, .pred_D) %>%
    select(.estimate),
  "accuracy" = 
    accuracy(predictions_TOI, defense, .pred_class) %>%
    select(.estimate),
  "auc" = 
    roc_auc(predictions_TOI, defense, .pred_D) %>%
    select(.estimate)
) %>%
  unnest(everything()) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(model = "TOI_only")

# Look at the ROC curve

predictions_TOI %>%
  roc_curve(defense, .pred_D) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  meg_theme() +
  labs(
    y = "True Positive Rate (Sensitivity)",
    x = "False Positive Rate",
    fill = NULL,
    title = "ROC Curve",
    subtitle = "TOI Only Model"
  )

# Look at who was predicted the most incorrectly
# (Just for fun)

most_wrong_TOI <- predictions_TOI %>%
  bind_cols(select(testing_data, player, TOI_game)) %>%
  mutate(incorrect = .pred_class != defense) %>%
  filter(incorrect == TRUE) %>%
  mutate(prob_actual = ifelse(defense == "D", .pred_D, .pred_F)) %>%
  arrange(prob_actual)

# Do the same process for our kitchen sink model

recipe_kitchen_sink <- training_data %>%
  recipe(defense ~ weight + height + points_60 + shots_60 + fenwick_60 + hits_60 + hits_rec_60 + blocks_60 + TOI_game) %>%
  step_corr(all_predictors(), threshold = 0.8) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

recipe_kitchen_sink

training_baked_KS <- juice(recipe_kitchen_sink)

testing_baked_KS <- recipe_kitchen_sink %>%
  bake(testing_data) 

# Run the kitchen sink model with our training data----

logistic_glm_KS <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(defense ~ ., data = training_baked_KS)

# Find the class predictions from our testing data
# And add back in the true values from testing data

predictions_class_KS <- logistic_glm_KS %>%
  predict(new_data = testing_baked_KS) %>%
  bind_cols(testing_baked_KS %>% select(defense))

# Find the probability predictions
# And add all together

predictions_KS <- logistic_glm_KS %>%
  predict(testing_baked_KS, type = "prob") %>%
  bind_cols(predictions_class_KS)

# Create a confusion matrix

predictions_KS %>%
  conf_mat(defense, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8) +
  meg_theme() +
  theme(panel.grid.major = element_blank()) +
  labs(
    y = "Actual Position",
    x = "Predicted Position",
    fill = NULL,
    title = "Confusion Matrix",
    subtitle = "Kitchen Sink Model"
  )

# Find the accuracy

predictions_KS %>%
  accuracy(defense, .pred_class) 

# Find the logloss

predictions_KS %>%
  mn_log_loss(defense, .pred_D)

# Find the area under the ROC curve (AUC)

predictions_KS %>%
  roc_auc(defense, .pred_D)

# Look at the ROC curve

predictions_KS %>%
  roc_curve(defense, .pred_D) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  meg_theme() +
  labs(
    y = "True Positive Rate (Sensitivity)",
    x = "False Positive Rate",
    fill = NULL,
    title = "ROC Curve",
    subtitle = "Kitchen Sink Model"
  )

# Create a tibble that holds all the evaluation metrics

KS_metrics <- tibble(
  "log_loss" = 
    mn_log_loss(predictions_KS, defense, .pred_D) %>%
    select(.estimate),
  "accuracy" = 
    accuracy(predictions_KS, defense, .pred_class) %>%
    select(.estimate),
  "auc" = 
    roc_auc(predictions_KS, defense, .pred_D) %>%
    select(.estimate)
) %>%
  unnest(everything()) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(model = "kitchen_sink")

# Compare evaluation metrics----

metrics_compare <- TOI_metrics %>%
  bind_rows(KS_metrics)

metrics_compare %>%
  ggplot(aes(fill = model, y = value, x = metric)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#7A8B99", "#A9DDD6")) +
  meg_theme() +
  labs(
    y = "Value",
    x = "Metric",
    fill = NULL,
    title = "Comparing Our Models",
    subtitle = "Higher is Better: Accuracy and AUC\nLower is Better: Log Loss"
  ) + 
  geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 3, position = position_dodge(width= 0.9)) +
  theme(legend.position = c(0.86, 0.9))
  
# Done----
