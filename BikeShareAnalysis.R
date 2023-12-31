library(vroom)
library(lubridate)
library(tidymodels)
library(poissonreg)
library(glmnet)
library(rpart)
library(ranger)
library(stacks)

# Set working directory
setwd("C:/Users/davis/OneDrive - Brigham Young University/Documents/skool")

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


# Fit Penalized Regression 
logtrain <- train %>%
  mutate(count=log(count))

pen_recipe <- recipe(count~., data=logtrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

pen_model <- linear_reg(penalty=.5, mixture=.5) %>%
  set_engine("glmnet")

pen_workflow <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(pen_model) %>%
  fit(data=logtrain)

pen_predictions <- predict(pen_workflow, test) %>%
  mutate(count=exp(.pred)) %>%
  bind_cols(.,test) %>%
  select(datetime,count) %>%
  mutate(count=ifelse(count<0,0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

write.csv(pen_predictions, file = "new/stat 348/KaggleBikeShare/pen_submission.csv", row.names = F)

# Attempt 2 Penalized Regression (different tuning parameters)
pen_model2 <- linear_reg(penalty=0, mixture=.5) %>%
  set_engine("glmnet")

pen_workflow2 <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(pen_model2) %>%
  fit(data=logtrain)

pen_predictions2 <- predict(pen_workflow2, test) %>%
  mutate(count=exp(.pred)) %>%
  bind_cols(.,test) %>%
  select(datetime,count) %>%
  mutate(count=ifelse(count<0,0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

write.csv(pen_predictions2, file = "new/stat 348/KaggleBikeShare/pen_submission2.csv", row.names = F)


# Fit Cross-Validated Penalized Regression
pencv_model <- linear_reg(penalty=tune(), 
                          mixture=tune()) %>%
  set_engine("glmnet")

pencv_workflow <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(pencv_model)

tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

folds <- vfold_cv(logtrain, v = 5, repeats = 1)

cv_results <- pencv_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid)

collect_metrics(cv_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(data = ., aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

besttune <- cv_results %>%
  select_best("rmse")

final_workflow <- pencv_workflow %>%
  finalize_workflow(besttune) %>%
  fit(data = logtrain)

pencv_predictions <- final_workflow %>%
  predict(new_data = test)

pencv_submission <- data.frame(test$datetime,
                               exp(pencv_predictions))

colnames(pencv_submission) <- c("datetime", "count")

write.csv(pencv_submission, file = "new/stat 348/KaggleBikeShare/pencv_submission.csv", row.names = F)


# Fit Regression Tree
tree_model <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune(),
                            min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_workflow <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(tree_model)

tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

tree_results <- tree_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid)

best_tree_tune <- tree_results %>%
  select_best("rmse")

final_tree_workflow <- tree_workflow %>%
  finalize_workflow(best_tree_tune) %>%
  fit(data = logtrain)

tree_predictions <- final_tree_workflow %>%
  predict(new_data = test)

tree_submission <- data.frame(test$datetime,
                              exp(tree_predictions))

colnames(tree_submission) <- c("datetime", "count")

write.csv(tree_submission, file = "new/stat 348/KaggleBikeShare/tree_submission.csv", row.names = F)


# Fit Random Forest
forest_model <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_workflow <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(forest_model)

tuning_grid <- grid_regular(mtry(range = c(1, 10)),
                            min_n(),
                            levels = 5)

forest_results <- forest_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid)

best_forest_tune <- forest_results %>%
  select_best("rmse")

final_forest_workflow <- forest_workflow %>%
  finalize_workflow(best_forest_tune) %>%
  fit(data = logtrain)

forest_predictions <- final_forest_workflow %>%
  predict(new_data = test)

forest_submission <- data.frame(test$datetime,
                                exp(forest_predictions))

colnames(forest_submission) <- c("datetime", "count")

write.csv(forest_submission, file = "new/stat 348/KaggleBikeShare/forest_submission.csv", row.names = F)


# Fit Stacked Model


folds <- vfold_cv(logtrain, v = 5)

untunedmodel <- control_stack_grid()
tunedmodel <- control_stack_resamples()

pen_model <- linear_reg(penalty=tune(), mixture=tune()) %>%
  set_engine("glmnet")

pen_wf <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(pen_model)

pen_tuning_grid <- grid_regular(penalty(),
                                mixture(),
                                levels = 5)

pen_models <- pen_wf %>%
  tune_grid(resamples = folds,
            grid = pen_tuning_grid,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedmodel)

lin_reg <- linear_reg() %>%
  set_engine("lm")

lin_reg_wf <- workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(pen_recipe)

lin_reg_model <- 
  fit_resamples(lin_reg_wf,
                resamples = folds,
                metrics = metric_set(rmse),
                control = tunedmodel)

tree_tuning_grid <- tuning_grid <- grid_regular(tree_depth(),
                                                cost_complexity(),
                                                min_n(),
                                                levels = 5)

tree_models <- tree_workflow %>%
  tune_grid(resamples = folds,
            grid = tree_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedmodel)

my_stack <- stacks() %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(pen_models) %>%
  add_candidates(tree_models)

stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()

stack_predictions <- stack_mod %>%
  predict(test)

stack_submission <- data.frame(datetime = test$datetime,
                               count = exp(stack_predictions))

colnames(stack_submission) <- c("datetime", "count")

write.csv(stack_submission, file = "new/stat 348/KaggleBikeShare/stack_submission.csv", row.names = F)


# Final Kaggle Score
folds <- vfold_cv(logtrain, v = 5)



forest_model <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_workflow <- workflow() %>%
  add_recipe(pen_recipe) %>%
  add_model(forest_model)

tuning_grid <- grid_regular(mtry(range = c(1, 10)),
                            min_n(),
                            levels = 10)

forest_results <- forest_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid)

best_forest_tune <- forest_results %>%
  select_best("rmse")

final_forest_workflow <- forest_workflow %>%
  finalize_workflow(best_forest_tune) %>%
  fit(data = logtrain)

forest_predictions <- final_forest_workflow %>%
  predict(new_data = test)

forest_submission <- data.frame(test$datetime,
                                exp(forest_predictions))

colnames(forest_submission) <- c("datetime", "count")

write.csv(forest_submission, file = "new/stat 348/KaggleBikeShare/final_submission.csv", row.names = F)


# Final Kaggle Score
final_recipe <- recipe(count~., data=logtrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="year") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

forest_model <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_workflow <- workflow() %>%
  add_recipe(final_recipe) %>%
  add_model(forest_model)

tuning_grid <- grid_regular(mtry(range = c(1, 10)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(logtrain, v = 5)

forest_results <- forest_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid)

best_forest_tune <- forest_results %>%
  select_best("rmse")

final_forest_workflow <- forest_workflow %>%
  finalize_workflow(best_forest_tune) %>%
  fit(data = logtrain)

forest_predictions <- final_forest_workflow %>%
  predict(new_data = test)

forest_submission <- data.frame(test$datetime,
                                exp(forest_predictions))

colnames(forest_submission) <- c("datetime", "count")

write.csv(forest_submission, file = "new/stat 348/KaggleBikeShare/final_submission.csv", row.names = F)
