library(vroom)
library(lubridate)
library(tidymodels)

# Read in training data
train <- vroom(file = "new/stat 348/KaggleBikeShare/train.csv")

# Remove singular row with weather category 4
bike <- train %>%
  filter(weather != 4) 

# Feature engineering
recipe <- recipe(count ~ ., data = bike) %>% # include all variables
  step_num2factor(season, levels = c("A", "B", "C", "D")) %>% # change season number to factor levels
  step_date(datetime, features = "dow") %>% # generate weekday from timestamp
  step_time(datetime, features = "hour") %>% # generate hour from timestamp
  step_select(-casual, -registered, -datetime) %>% # remove unwanted variables
  step_select(datetime_dow, datetime_hour, season, holiday, # reorder variables
              workingday, weather, temp, atemp, 
              humidity, windspeed, count)

# Prep recipe for baking
prep <- prep(recipe)

# Bake that ish
bike <- bake(prep, new_data = bike)
