setwd("C:/Users/sb17b/OneDrive/Desktop/Profile Writeups")

library(dplyr)
library(readr)
library(zoo)
library(caret)
library(tidyr)


#LOAD IN DATASET

DecadeHitterData <- read.csv("DecadeHitterPredict.csv")





#REMOVE UNNESSECARY COLUMNS

DecadeHitterData <- DecadeHitterData %>%
  select(-c(Season.1, NameASCII, PlayerId, MLBAMID))





# RENAME COLUMNS

DecadeHitterData <- DecadeHitterData %>%
  rename(
    BB_Perc = BB.,
    K_Perc = K.,
    HardHit_Perc = HardHit.,
    Barrel_Perc = Barrel.,
    Chase_Perc = O.Swing...sc.,
    ZContact_Perc = Z.Contact...sc.
  )





# REMOVE 2020 (MAJOR PA DISCREPENCY)

DecadeHitterData <- DecadeHitterData %>% filter(Season != 2020)





#TRANSITION DATA (FORM LEAD, LAG, AND DELTA COLUMNS)

TransitionData <- DecadeHitterData %>%
  arrange(Name, Season) %>%                        
  group_by(Name) %>%                           
  mutate(
    wOBA_next = lead(wOBA, 1),                 # Creates a column identifying the immediate next season's wOBA for a player (think dependent variable)
    wOBA_delta = wOBA - lag(wOBA, 1),          # Creates columns identifying the changes in each metric from the previous season (think independent variable)
    OPS_delta = OPS - lag(OPS, 1),
    K_delta = K_Perc - lag(K_Perc, 1),
    BB_delta = BB_Perc - lag(BB_Perc, 1),
    Barrel_delta = Barrel_Perc - lag(Barrel_Perc, 1),
    HardHit_delta = HardHit_Perc - lag(HardHit_Perc, 1),
    EV_delta = EV - lag(EV, 1),
  ) %>%
  ungroup()





# CREATE THE TRAINING DATA AND THE PREDICTION DATA

# The data we want to use to train the model

train_data <- TransitionData %>%
  filter(Season <= 2024 & !is.na(wOBA_next)) %>%                                    # Remove 2025 season rows, there is not w_OBA next (next season: 2026, has not happened)
  drop_na(all_of(c("wOBA_delta", "OPS_delta", "K_delta",
                   "BB_delta", "Barrel_delta", "HardHit_delta", "EV_delta")))       # Removes any first year data (2015/rookies, players that only have 1 logged season)

# The data we want the trained model to apply to 

predict_data <- TransitionData %>% filter(Season == 2025)





# SET PREDICTORS

# variables that are included for returning players (previous & current metrics and stats, the following wOBA)

predictors_returning <- c("wOBA", "xwOBA", "OPS", "BB_Perc", "K_Perc", "Barrel_Perc", "HardHit_Perc",
                          "EV", "wOBA_delta", "OPS_delta", "K_delta", "BB_delta",
                          "Barrel_delta", "HardHit_delta", "EV_delta")

# variables that are only available for rookies (current season metrics)

predictors_rookie <- c("wOBA", "xwOBA", "OPS", "BB_Perc", "K_Perc",
                       "Barrel_Perc", "HardHit_Perc", "EV")





# TRAIN MODELS

# Model used for returning players (accounts for metric relationships & how changes in metrics affects changes in wOBA)

fit_returning <- train(
  wOBA_next ~ .,                                                            # What we are solving for
  data = train_data %>% select(wOBA_next, all_of(predictors_returning)),    # Use the training dataframe (select wOBA_next as dep var. select all of returning player predictors as ind vars.)
  method = "lm",                                                            # Using linear regression method
  trControl = trainControl(method = "cv", number = 10)                      # Cross variation 10x (standard), adds level of validity and reliability to the relationships the model is reading
)

# Model used for rookie players (*only uses the current metrics and relates to current wOBA*)

fit_rookie <- train(
  wOBA_next ~ .,                                                            # What we are solving for
  data = train_data %>% select(wOBA_next, all_of(predictors_rookie)) %>%    # Use the training dataframe (select wOBA_next as dep var. select all of rookie player predictors as ind vars.)
    drop_na(),
  method = "lm",                                                            # Using linear regression method
  trControl = trainControl(method = "cv", number = 10)                      # Cross variation 10x (standard), adds level of validity and reliability to the relationships the model is reading
)





# PREDICT 2026

# Prediction data for returning players

predict_2026_returning <- predict_data %>%                                        # Data the model is being applied to (2025)
  filter(!is.na(wOBA_delta)) %>%                                                  # Removes players without a previous season wOBA (isolates returning players)
  mutate(
    pred_object = predict(fit_returning$finalModel,                               # Apply the predict function using the trained model for returning players
                          newdata = select(., all_of(predictors_returning)),      # Use the current selected dataframe (predict_data), isolate only the returning player predictors)
                          interval = "prediction", level = 0.95)                  # Return a 95% interval (predicted whole number +- margin of error)
  ) %>%
  mutate(wOBA_predict_2026 = round(pred_object[, "fit"], 3),                      # Name column = the predicted number (fit), rounded to 3 decimal places
         wOBA_lower_95 = round(pred_object[, "lwr"], 3),                          # Name column = lower 95% interval (lwr), rounded to 3 decimal places
         wOBA_upper_95 = round(pred_object[, "upr"], 3)                           # Name column = upper 95% interval (lwr), rounded to 3 decimal places
  )

# 

predict_2026_rookies <- predict_data %>%                                          # Data the model is being applied to (2025)
  filter(is.na(wOBA_delta)) %>%                                                   # Isolates players without a previous season wOBA (rookies)
  mutate(
    pred_object = predict(fit_rookie$finalModel,                                  # Apply the predict function using the trained model for rookies
                          newdata = select(., all_of(predictors_rookie)),         # Use the current selected dataframe (predict_data), isolate only the rookie player predictors)
                          interval = "prediction", level = 0.95)                  # Return a 95% interval (predicted whole number +- margin of error)
  ) %>%
  mutate(wOBA_predict_2026 = round(pred_object[, "fit"], 3),                      # Name column = the predicted number (fit), rounded to 3 decimal places
         wOBA_lower_95 = round(pred_object[, "lwr"], 3),                          # Name column = lower 95% interval (lwr), rounded to 3 decimal places
         wOBA_upper_95 = round(pred_object[, "upr"], 3)                           # Name column = upper 95% interval (lwr), rounded to 3 decimal places
  )





# COMBINE AND ORDER RETURNING AND ROOKIE RESULTS

results <- bind_rows(predict_2026_returning, predict_2026_rookies) %>%
  mutate(
    Rookie = ifelse(is.na(wOBA_delta), "Rookie", NA),                                                             # Creates a column identifying rookies (if wOBA_delta is blank: label Rookie, else leave as NA)
    wOBA = round(wOBA, 3),
    OPS = round(OPS, 3),
    wOBA_predict_2026 = round(wOBA_predict_2026, 3),
    wOBA_difference = round(wOBA_predict_2026 - wOBA, 3)                                                         # Shows largest predicted increase/decreases in wOBA for 2026
  ) %>%
  select(Name, Season, Rookie, OPS, wOBA, wOBA_predict_2026, wOBA_lower_95, wOBA_upper_95, wOBA_difference)





# PRODUCE THE LINEAR IMPACT OF EACH VARIABLE ON THE NEXT SEASONS wOBA
varImp(fit_returning)
