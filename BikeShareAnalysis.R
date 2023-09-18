library(vroom)
library(lubridate)
library(tidymodels)
library(poissonreg)

# Read in training data
train <- vroom(file = "new/stat 348/KaggleBikeShare/train.csv")
test <- vroom("new/stat 348/KaggleBikeShare/test.csv")

# Remove singular row with weather category 4
train <- train %>%
  select(-casual, -registered)

# Feature engineering
recipe <- recipe(count~., data=train) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)

# Prep recipe for baking
prep <- prep(recipe)

# Bake that ish
bake(prep, new_data = train) #Make sure recipe work on train
bake(prep, new_data = test)


# Fit Linear Regression
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


# Fit Poisson Regression
pois_model <- poisson_reg() %>%
  set_engine("glm")

pois_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(pois_model) %>%
  fit(data = train)

pois_predictions <- predict(pois_workflow,
                            new_data=test)

pois_submission <- data.frame(datetime = test$datetime,
                              count = pois_predictions)

colnames(pois_submission) <- c("datetime", "count")

write.csv(pois_submission, file = "new/stat 348/KaggleBikeShare/pois_submission.csv", row.names = F)
