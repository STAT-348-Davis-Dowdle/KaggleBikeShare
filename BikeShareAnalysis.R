library(vroom)
library(lubridate)
library(tidymodels)

# Read in training data
train <- vroom(file = "new/stat 348/KaggleBikeShare/train.csv")

# Remove singular row with weather category 4
train[train$weather==4,5] <- 3
train <- train %>%
  select(-casual, -registered)

# Feature engineering
recipe <- recipe(count ~ ., data = train) %>% # include all variables
  step_num2factor(season, levels = c("A", "B", "C", "D")) %>% # change season number to factor levels
  step_date(datetime, features = "dow") %>% # generate weekday from timestamp
  step_time(datetime, features = "hour") # generate hour from timestamp
  # step_select(datetime_dow, datetime_hour, season, holiday, # reorder variables
  #             workingday, weather, temp, atemp, 
  #             humidity, windspeed)

# Prep recipe for baking
prep <- prep(recipe) %>% juice()

# Bake that ish
baked <- bake(prep, new_data = test)


# Fit Linear Regression
test <- vroom("new/stat 348/KaggleBikeShare/test.csv")

model <- linear_reg() %>%
  set_engine("lm")

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model) %>%
  fit(data = train)

predictions <- predict(workflow, new_data = test)

submission <- data.frame(datetime = test$datetime,
                         count = predictions)

colnames(submission) <- c("datetime", "count")

submission$count <- ifelse(submission$count < 0, 0, submission$count)

write.csv(submission, file = "new/stat 348/KaggleBikeShare/submission.csv", row.names = F)
