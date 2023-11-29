library(vroom)

item_train <- vroom("./Item-Demand-Challenge/train.csv")
item_test <- vroom("./Item-Demand-Challenge/test.csv")
#10 stores and 50 items
library(timetk)
library(tidyverse)

graph1 <- item_train %>%
  filter(store == 1, item == 1)
graph2 <- item_train %>%
  filter(store == 5, item == 1)
graph3 <- item_train %>%
  filter(store == 1, item == 20)
graph4 <- item_train %>%
  filter(store == 1, item == 50)

library(patchwork)
a <- graph1 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
b <- graph2 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
c <- graph3 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
d <- graph4 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
a <- a + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 1")
b <- b + xlab("Time") + ylab("Sales") + ggtitle("Store 5, Item 1")
c <- c + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 20")
d <- d + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 50")
(a+b)/(c+d)

###11/15/2023 Fitted a tidymodels recipe for timeseries
library(tidymodels)
library(embed)

item_1_1 <- item_train %>%
  filter(store == 1, item == 1)

ts_recipe <- recipe(sales ~ ., data = item_1_1) %>%
  step_date(date, features = c("doy", "dow", "month")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
prep <- prep(ts_recipe)
baked_tr <- bake(prep, new_data = item_1_1)


##TESTINGTESTINGTESTING
tree_mod <- rand_forest(mtry = tune(),
                        min_n = tune(),
                        trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")


##Workflow
forest_workflow <- workflow() %>%
  add_recipe(ts_recipe) %>%
  add_model(tree_mod)


tuning_grid <- grid_regular(mtry(range = c(1,4)),
                            min_n(),
                            levels = 4)

folds <- vfold_cv(item_1_1, v = 5, repeats = 1)

CV_results <- forest_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

collect_metrics(CV_results) %>%
  filter(mtry == 4, min_n == 40) %>%
  pull(mean)

bestTune <- CV_results %>%
  select_best("smape")

final_wf <- forest_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data = amazon_tr)

##11/17/2023
install.packages("modeltime")

library(modeltime)
library(timetk)

item_1_1 <- item_train %>%
  filter(store == 1, item == 1)

item_1_3 <- item_train %>%
  filter(store == 1, item == 3)

cv_split <- time_series_split(item_1_1, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data = training(cv_split))

cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_1_1) %>%
      plot_modeltime_forecast(.interactive = TRUE)


cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

es_fullfit <- cv_results %>%
  modeltime_refit(data = item_1_1)


test_1_1 <- item_test %>%
  filter(item == 1, store == 1)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_1_1, by="date") %>%
  select(id, sales)


p3 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = item_1_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)


item_1_3 <- item_train %>%
  filter(store == 1, item == 3)

cv_split <- time_series_split(item_1_3, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data = training(cv_split))

cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

p2 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_1_3) %>%
  plot_modeltime_forecast(.interactive = TRUE)


cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

es_fullfit <- cv_results %>%
  modeltime_refit(data = item_1_3)

test_1_3 <- item_test %>%
  filter(item == 3, store == 1)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_1_3, by="date") %>%
  select(id, sales)


p4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = item_1_3) %>%
  plot_modeltime_forecast(.interactive = FALSE)

library(plotly)

plotly::subplot(p1, p2, p3, p4, nrows = 2)

###11-27-23 ARIMA
library(tidymodels)
library(embed)

library(modeltime)
library(timetk)

install.packages("forecast")
library(forecast)

item_1_1 <- item_train %>%
  filter(store == 1, item == 1)

ts_recipe <- recipe(sales ~ ., data = item_1_1) %>%
  step_date(date, features = c("doy", "dow", "month")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
prep <- prep(ts_recipe)
baked_tr <- bake(prep, new_data = item_1_1)

cv_split <- time_series_split(baked_tr, assess="3 months", cumulative = TRUE)

#Create arima model
arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 2,
                         seasonal_differences =  2
                         ) %>%
  set_engine("auto_arima")

#initialize arima workflow
arima_wf <- workflow() %>%
  add_recipe(ts_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

#calibrate
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
#plot calibrations
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#check
p1

#refit the data
arima_fullfit <- cv_results %>%
  modeltime_refit(data = baked_tr)

#subset test data
test_1_1 <- item_test %>%
  filter(item == 1, store == 1)
baked_test <- bake(prep, new_data = test_1_1)

#create predictions
arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = baked_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=baked_test, by="date") %>%
  select(date, sales)

p3 <- arima_fullfit %>%
  modeltime_forecast(new_data = baked_test, actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p3



item_1_3 <- item_train %>%
  filter(store == 1, item == 3)

ts_recipe <- recipe(sales ~ ., data = item_1_3) %>%
  step_date(date, features = c("doy", "dow", "month")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
prep <- prep(ts_recipe)
baked_tr <- bake(prep, new_data = item_1_3)

cv_split <- time_series_split(baked_tr, assess="3 months", cumulative = TRUE)

#Create arima model
arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 2,
                         seasonal_differences =  2
) %>%
  set_engine("auto_arima")

#initialize arima workflow
arima_wf <- workflow() %>%
  add_recipe(ts_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

#calibrate
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
#plot calibrations
p2 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#check
p2

#refit the data
arima_fullfit <- cv_results %>%
  modeltime_refit(data = baked_tr)

#subset test data
test_1_3 <- item_test %>%
  filter(item == 1, store == 3)
baked_test <- bake(prep, new_data = test_1_3)

#create predictions
arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = baked_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=baked_test, by="date") %>%
  select(date, sales)

p4 <- arima_fullfit %>%
  modeltime_forecast(new_data = baked_test, actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p4

library(plotly)

plotly::subplot(p1, p2, p3, p4, nrows = 2)

###11-29-2023 Facebook Prophet Model
library(tidymodels)
library(embed)

library(modeltime)
library(timetk)
library(forecast)

item_1_1 <- item_train %>%
  filter(store == 1, item == 1)

ts_recipe <- recipe(sales ~ ., data = item_1_1) %>%
  step_date(date, features = c("doy", "dow", "month")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
prep <- prep(ts_recipe)
baked_tr <- bake(prep, new_data = item_1_1)

cv_split <- time_series_split(baked_tr, assess="3 months", cumulative = TRUE)

#Create arima model
prophet_model <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(sales ~ date, data = training(cv_split))

#calibrate
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))
#plot calibrations
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#check
p1

#refit the data
prophet_fullfit <- cv_results %>%
  modeltime_refit(data = baked_tr)

#subset test data
test_1_1 <- item_test %>%
  filter(item == 1, store == 1)
baked_test <- bake(prep, new_data = test_1_1)

#create predictions
prophet_preds <- prophet_fullfit %>%
  modeltime_forecast(new_data = baked_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=baked_test, by="date") %>%
  select(date, sales)

p3 <- prophet_fullfit %>%
  modeltime_forecast(new_data = baked_test, actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p3
###
###OTHER ITEM
###
item_1_3 <- item_train %>%
  filter(store == 1, item == 3)

ts_recipe <- recipe(sales ~ ., data = item_1_3) %>%
  step_date(date, features = c("doy", "dow", "month")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
prep <- prep(ts_recipe)
baked_tr <- bake(prep, new_data = item_1_3)

cv_split <- time_series_split(baked_tr, assess="3 months", cumulative = TRUE)

#Create arima model
prophet_model <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(sales ~ date, data = training(cv_split))

#calibrate
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))
#plot calibrations
p2 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#check
p2

#refit the data
prophet_fullfit <- cv_results %>%
  modeltime_refit(data = baked_tr)

#subset test data
test_1_3 <- item_test %>%
  filter(item == 1, store == 1)
baked_test <- bake(prep, new_data = test_1_3)

#create predictions
prophet_preds <- prophet_fullfit %>%
  modeltime_forecast(new_data = baked_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=baked_test, by="date") %>%
  select(date, sales)

p4 <- prophet_fullfit %>%
  modeltime_forecast(new_data = baked_test, actual_data = baked_tr) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p4

library(plotly)

plotly::subplot(p1, p2, p3, p4, nrows = 2)