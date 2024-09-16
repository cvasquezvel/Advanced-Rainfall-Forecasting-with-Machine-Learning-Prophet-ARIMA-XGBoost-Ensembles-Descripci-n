# Cambiar el directorio de trabajo ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargar paquetes ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readxl, dplyr, tidyr, imputeTS, ISOweek,
               neuralprophet, boostime, tidyverse, tidymodels,
               modeltime, timetk, lubridate, modeltime.resample,
               modeltime.ensemble, parallel, modeltime.h2o, timetk)

# Iniciar H2o ----

h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

# Optional - Turn off progress indicators during training runs
h2o.no_progress()

### Cargar datos ----

all_na <- function(x) any(!is.na(x))
# spaceless <- function(x) {colnames(x) <- gsub(c("\\s\\s"), " ", colnames(x));x}

datos_clima <- readxl::read_excel("C:/Users/Dell/Documents/Danper/1. Variables climaticas/2022/Datos clima.xlsx",sheet = "Todos")
data <- datos_clima %>%
  mutate(Fecha = ceiling_date(Date, "week"),
    Semana = isoweek(Fecha),
    Date = as.Date(Date, format = "%Y%m%d"),
    `Temp. Media (°C)` = (`Temp. Maxima (°C)`+`Temp. Minima (°C)`)/2,
    `Lluvia yn` = ifelse(`Lluvia (mm)` > 0, 1, 0)) %>%
  dplyr::select_if(all_na)%>%
  # janitor::clean_names()
  # spaceless() %>%
  dplyr::mutate(Time = round(Time*24,0),
                DiadelAño = as.numeric(format(Date, format = "%j")),
                DiadelMes = as.numeric(format(Date, format = "%d")),
                DiadelaSemana = as.numeric(format(Date, format = "%u")),
                Año = lubridate::year(Date))

# write.csv(clima, "data_clima.csv", row.names = F)

#### Análisis de valores perdidos ----

# Para ver que filas por columna tienen valores perdidos
rowmiss <- which(rowSums(is.na(data)) != 0)
rowmiss

# Para ver el porcentaje de valores perdidos en las filas
per.miss.row = 100 * rowSums(is.na(data[rowmiss, ]))/dim(data)[2]
per.miss.row

visdat::vis_miss(data, warn_large_data = FALSE)

# Para ver que columnas tienen valores perdidos
colmiss <- which(colSums(is.na(data)) != 0)
colmiss

# Para ver el porcentaje de valores perdidos en las columnas
per.miss.col = 100 * colSums(is.na(data[, colmiss]))/dim(data)[1]
per.miss.col

# Aggregation plot
a = VIM::aggr(data, numbers = T)

VIM::aggr(data, numbers = T, sortComb = TRUE, sortVar = TRUE, only.miss = TRUE)

x11()
VIM::aggr(data, numbers = T, sortComb = TRUE, sortVar = TRUE, only.miss = TRUE)

visdat::vis_miss(data, warn_large_data = FALSE)

VIM::matrixplot(data, sortby = "Date",interactive = TRUE)

### Creación de matriz ----

datos <- data

### Modeltime ----

#### Getting Started ----

cores <- parallel::detectCores(logical = TRUE) - 1
parallel_start(cores)

# library(doFuture)
# registerDoFuture()
# n_cores <- parallel::detectCores()-1
# plan(
#   strategy = cluster,
#   workers = parallel::makeCluster(n_cores))

library(benchmarkme)

res <- benchmarkme::benchmark_std(runs = 3)

#### Get Your Data ----

rendimiento_tbl_complete <- datos %>%
  dplyr::filter(Fundo %in% "Agromorin")

rendimiento_tbl <- rendimiento_tbl_complete %>%
    filter(Date <= as.Date("2022-01-01")) %>% 
  recipe()%>% 
  update_role(`Lluvia yn`, new_role = "label") %>%
  update_role(-`Lluvia yn`, new_role = "predictor") %>%
  step_nzv(all_predictors()) %>%
  prep() %>% bake(new_data = NULL)

rendimiento_new_data <- rendimiento_tbl_complete %>%
  filter(Date > as.Date("2022-01-01"))

rendimiento_tbl %>% 
  plot_time_series(Date, `Lluvia yn`, .interactive = TRUE)

#### Train / Test ----

# splits <- rendimiento_tbl %>%
#   time_series_split(date_var    = Fecha,
#                     initial     = "47 months",
#                     assess      = "3 months",
#                     skip        = "3 months",
#                     cumulative = FALSE)

splits <- rendimiento_tbl %>% rsample::initial_time_split(prop = 0.95)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(.value = Kg_Ha,
                           .date_var = Fecha,
                           .interactive = FALSE)

rendimiento_tscv <- training(splits) %>%
  time_series_cv(
    date_var    = Fecha,
    initial     = "21 months",
    assess      = "8 months",
    skip        = "2 months",
    slice_limit = 10,
    cumulative  = TRUE,
    prop = 0.8
  )
# rendimiento_tscv %>% tk_time_series_cv_plan()
rendimiento_tscv %>%
  plot_time_series_cv_plan(Fecha, Kg_Ha, .interactive = FALSE)

#### Features selection ----

model_fit_lasso <- linear_reg(mode = "regression",penalty = 0) %>%
  set_engine("glmnet") %>%
  fit(Kg_Ha ~ ., data = training(splits))
# tidy(model_fit_lasso) %>% as.data.frame()
nopredictors <- tidy(model_fit_lasso) %>% select(term,estimate) %>%
  filter(estimate == 0,
         term != "(Intercept)") %>% as.data.frame() %>%
  select(term) %>% as.vector()
nopredictors

predictors <- tidy(model_fit_lasso) %>% select(term,estimate) %>%
  as.data.frame() %>%
  mutate(estimate = abs(estimate)) %>%
  filter(estimate != 0,
         term != "(Intercept)") %>% 
  arrange(-estimate) %>%
  select(term) %>% as.vector()
predictors

#### Modeling ----

##### Auto ARIMA ----

model_fit_arima <- arima_reg(mode = "regression") %>%
  set_engine("auto_arima") %>%
  fit(Kg_Ha ~ ., training(splits))

model_fit_arima$fit$data$.residuals %>% forecast::checkresiduals()

##### Prophet ----

prophet_reg_model <- prophet_reg(
  mode = "regression",
  growth = tune(),
  changepoint_num = tune(),
  changepoint_range = tune(),
  seasonality_yearly = tune(),
  seasonality_weekly = tune(),
  seasonality_daily = tune(),
  season = tune()
) %>% set_engine("prophet") 

# set.seed(123)
# resamples_kfold <- training(splits) %>%
#   vfold_cv(v = 10)

recipe_spec <- recipe(Kg_Ha ~ ., training(splits)) %>%
  step_timeseries_signature(Fecha) #%>%
# step_rm(Fecha)
# step_rm(contains("am.pm"), contains("hour"), contains("minute"),
#         contains("second"), contains("xts")) %>%
# step_fourier(Fecha, period = 365, K = 5) %>%
# step_dummy(all_nominal())

wflw_spec_tune_prophet <- workflow() %>%
  add_model(prophet_reg_model) %>%
  add_recipe(recipe_spec)

tictoc::tic()
tune_results_prophet <- tune_grid( object = wflw_spec_tune_prophet, 
                                   resamples = rendimiento_tscv,
                                   param_info = parameters(wflw_spec_tune_prophet) %>%
                                     update(
                                       growth = growth(values = c("linear","logistic")),
                                       changepoint_num = changepoint_num(range = c(5L, 45L), 
                                                                         trans = NULL),
                                       changepoint_range = changepoint_range(range = c(0.7, 0.9),
                                                                             trans = NULL),
                                       seasonality_yearly = seasonality_yearly(values = c(TRUE, FALSE)),
                                       seasonality_weekly = seasonality_weekly(values = c(TRUE, FALSE)),
                                       seasonality_daily = seasonality_daily(values = c(TRUE, FALSE)),
                                       season = season(values = c("additive", "multiplicative"))
                                     ),
                                   grid = 100, # 10
                                   control = control_grid(verbose = TRUE,
                                                          allow_par = TRUE)
)
tictoc::toc() # if you want to know how much time is needed for tuning

best_results <- tune_results_prophet %>%
  show_best(metric = 'rmse',n = 10) %>% as.data.frame()
best_results 

model_fit_prophet <- prophet_reg(growth = "linear",
                                 changepoint_num = 15,
                                 changepoint_range = 0.7925948,
                                 seasonality_yearly = TRUE,
                                 seasonality_weekly = TRUE,
                                 seasonality_daily = TRUE,
                                 season = "additive") %>%
  set_engine("prophet", num.threads = cores) %>%
  fit(Kg_Ha ~ ., training(splits))

model_fit_prophet

model_fit_prophet$fit$data$.residuals %>% forecast::checkresiduals()

##### Exponential Smoothing ----

model_fit_ets <- exp_smoothing(mode = "regression") %>%
  set_engine(engine = "ets") %>%
  fit(Kg_Ha ~ ., data = training(splits))

model_fit_ets

model_fit_ets$fit$data$.residuals %>% forecast::checkresiduals()

##### Naive ----

model_fit_naive <- naive_reg(seasonal_period = "auto") %>%
  set_engine("naive", num.threads = cores) %>%
  fit(Kg_Ha ~ ., data = training(splits))

model_fit_naive

##### Seasonal Naive ----

model_fit_snaive <- naive_reg(seasonal_period = "auto") %>%
  set_engine("snaive", num.threads = cores) %>%
  fit(Kg_Ha ~ ., data = training(splits))

model_fit_snaive

#### Machine Learning Models----

##### Preprocessing Recipe ----

recipe_spec <- recipe(Kg_Ha ~ ., training(splits)) %>%
  step_timeseries_signature(Fecha) %>%
  update_role(-Kg_Ha, new_role = "predictor") #%>%
  # step_rm(Fecha)
  # step_rm(contains("am.pm"), contains("hour"), contains("minute"),
  #         contains("second"), contains("xts")) %>%
  # step_fourier(Fecha, period = 365, K = 5) %>%
  # step_dummy(all_nominal())
# recipe_spec %>% prep() %>% juice()

##### Linear Regression ----

model_fit_lm <- linear_reg(mode = "regression") %>%
  set_engine("lm")

workflow_fit_lm <- workflow() %>%
  add_model(model_fit_lm) %>%
  add_formula(Kg_Ha ~ .) %>%
  # add_recipe(recipe_spec %>% step_rm(Fecha)) %>%
  fit(training(splits))

workflow_fit_lm

##### MARS ----

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec_mars <- recipe(Kg_Ha ~ Fecha, data = training(splits)) %>%
  step_date(Fecha, features = "year", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(Fecha)) %>%
  step_normalize(date_num) %>%
  step_rm(Fecha)

workflow_fit_mars <- workflow() %>%
  add_model(model_spec_mars) %>%
  add_formula(Kg_Ha ~ .) %>%
  # add_recipe(recipe_spec_mars %>% step_rm(Fecha)) %>%
  #add_recipe(recipe_spec) %>%
  fit(training(splits))

##### Elastic Net ----

coef_path_values <- c(0, 10^seq(-5, 1, length.out = 7))

fit_ridge <- 
  linear_reg(penalty = 1, mixture = 0) %>% 
  set_engine("glmnet", path_values = coef_path_values) %>% 
  fit(Kg_Ha ~ ., data = training(splits))

# tidy(fit_ridge) %>% as.data.frame

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet", path.values = coef_path_values)

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_formula(Kg_Ha ~ .) %>%
  #add_recipe(recipe_spec %>% step_rm(Fecha)) %>%
  fit(training(splits))

# tidy(workflow_fit_glmnet) %>% as.data.frame

##### Random Forest ----

recipe_spec_rf <- recipe(Kg_Ha ~ ., training(splits)) %>%
  step_rm(Fecha) %>%
  # step_timeseries_signature(Fecha) %>%
  update_role(-Kg_Ha, new_role = "predictor") %>%
  # step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
# step_rm(contains("am.pm"), contains("hour"), contains("minute"),
#         contains("second"), contains("xts")) %>%
# step_fourier(Fecha, period = 365, K = 5) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

###### Tune mode 1 ----

model_tbl <- expand.grid(
  trees = c(5,10,25,50,100,300,500,1000),
  min_n = c(2,5,10,15,25,50,100,150,200,250,300,500),
  mtry = c(2,4,6,8,10)
) %>%
  create_model_grid(
    f_model_spec = rand_forest,
    engine_name  = "randomForest",
    mode         = "regression"
  )
model_tbl

model_list <- model_tbl$.models
model_list

model_wfset <- workflow_set(
  preproc = list(
    recipe_spec
  ),
  models = model_list, 
  cross = TRUE
)
model_wfset

model_parallel_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training(splits),
    control = control_fit_workflowset(
      verbose   = FALSE,
      allow_par = TRUE
    )
  )

model_parallel_tbl

model_parallel_calibrate <- model_parallel_tbl %>%
  modeltime_calibrate(testing(splits))

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

model_parallel_forecast <- model_parallel_calibrate %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl,
    keep_data   = TRUE
  )

model_parallel_forecast %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = TRUE
  )

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  arrange(mae) %>%
  head(10) %>%
  filter(mae %in% min(mae))

model_list[[434]]

model_spec_rf <- rand_forest(mtry = 10,
                             trees = 10,
                             min_n = 100) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

###### Tune mode 2 ----

model_spec_rf_tune <- rand_forest(
  mode           = "regression",
  mtry           = tune(),
  trees          = tune(),
  min_n          = tune()) %>%
  set_engine("randomForest")

wflw_spec_tune_rf <- workflow() %>%
  add_model(model_spec_rf_tune) %>%
  add_recipe(recipe_spec_rf)

resamples_kfold <- rendimiento_tscv

tune_results_rf <- tune_grid(
  object = wflw_spec_tune_rf,
  resamples = resamples_kfold,
  param_info = parameters(wflw_spec_tune_rf),
  grid = 100,
  control = control_grid(verbose = FALSE, allow_par = TRUE))

best_results <- tune_results_rf %>%
  show_best(metric = "rmse", n = 10) %>% as.data.frame()
best_results

workflow_fit_rf <- wflw_spec_tune_rf %>%
  finalize_workflow(parameters = best_results %>% slice(1)) %>%
  fit(training(splits))

workflow_fit_rf %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", 50) + 
  labs(title = "Random forest variable importance") 

##### XGBoost ----

recipe_spec_1 <- recipe(Kg_Ha ~ ., data = training(splits)) %>%
  step_rm(Fecha) %>%
  update_role(-Kg_Ha, new_role = "predictor") %>%
  # update_role(Fecha, new_role = "indicator") %>%
  # step_timeseries_signature(Fecha) %>%
  # step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) #%>%
  # step_normalize(contains("index.num"), ends_with("_year"))

# recipe_spec_1 <- recipe(Kg_Ha ~ ., data = training(splits)) %>%
#   step_timeseries_signature(Fecha) %>%
#   step_rm(Fecha) %>%
#   # step_normalize(Date_index.num) %>%
#   step_zv(all_predictors()) %>%
#   step_dummy(all_nominal_predictors(), one_hot = TRUE)

###### Tune mode 1 ----

model_tbl <- expand.grid(
  learn_rate = as.numeric(c(0.001, 0.010, 0.100, 0.350, 0.500, 0.650, 0.8, 1)),
  min_n = c(25,50,100),
  tree_depth = c(1,2,4,6,8),
  tree = c(2,4,6,8,10)
  ) %>%
  create_model_grid(
    f_model_spec = boost_tree,
    engine_name  = "xgboost",
    mode         = "regression"
  )
model_tbl

model_list <- model_tbl$.models
model_list

model_wfset <- workflow_set(
  preproc = list(
    recipe_spec_1
  ),
  models = model_list, 
  cross = TRUE
)
model_wfset

model_parallel_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training(splits),
    control = control_fit_workflowset(
      verbose   = FALSE,
      allow_par = TRUE
    )
  )

model_parallel_tbl

model_parallel_calibrate <- model_parallel_tbl %>%
  modeltime_calibrate(testing(splits))

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

model_parallel_forecast <- model_parallel_calibrate %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl,
    keep_data   = TRUE
  )

model_parallel_forecast %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = TRUE
  )

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  arrange(mae) %>%
  head(10) %>%
  filter(mae %in% min(mae))

model_list[[286]]

model_spec_xgb <- boost_tree(trees = 6,
                             min_n = 100,
                             tree_depth = 2,
                             learn_rate = 0.65) %>%
  set_engine("xgboost")

workflow_fit_xgb <- workflow() %>%
  add_model(model_spec_xgb) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

###### Tune mode 2 ----

model_spec_xgboost_tune <- boost_tree(
  mode           = "regression",
  mtry           = tune(),
  trees          = tune(),
  min_n          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune()) %>%
  set_engine("xgboost")

wflw_spec_tune_xgboost <- workflow() %>%
  add_model(model_spec_xgboost_tune) %>%
  add_recipe(recipe_spec_1)

resamples_kfold <- rendimiento_tscv

tune_results_xgboost <- tune_grid(
  object = wflw_spec_tune_xgboost,
  resamples = resamples_kfold,
  param_info = parameters(wflw_spec_tune_xgboost) %>%
    update(learn_rate = learn_rate(range = c(0.15, 0.5), trans = NULL)),
  grid = 100,
  control = control_grid(verbose = FALSE, allow_par = TRUE))

best_results <- tune_results_xgboost %>%
  show_best(metric = "rmse", n = 10) %>% as.data.frame()
best_results

workflow_fit_xgb <- wflw_spec_tune_xgboost %>%
  finalize_workflow(parameters = best_results %>% slice(1)) %>%
  fit(training(splits))

workflow_fit_xgb %>%
  extract_fit_parsnip() %>%
  vip(geom = "point",20) + 
  labs(title = "XGBoost variable importance") 

##### Boosted Auto ARIMA ----

recipe_spec_2 <- recipe(Kg_Ha ~ ., data = training(splits)) %>%
  update_role(-Kg_Ha, new_role = "predictor") %>%
  step_timeseries_signature(Fecha) %>%
  #step_rm(Fecha) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

###### Tune mode 1 ----

model_tbl <- expand.grid(
  learn_rate = as.numeric(c(0.001, 0.010, 0.100, 0.350, 0.500, 0.650, 0.8, 1)),
  min_n = c(2,5,15),
  tree_depth = c(1,2,4,6,8,10),
  trees = c(2,4,6,8,10)
) %>%
  create_model_grid(
    f_model_spec = arima_boost,
    engine_name  = "auto_arima_xgboost",
    mode         = "regression"
  )
# model_tbl

model_list <- model_tbl$.models
# model_list

model_wfset <- workflow_set(
  preproc = list(
    recipe_spec_2
  ),
  models = model_list, 
  cross = TRUE
)
model_wfset

model_parallel_tbl <- model_wfset %>%
  modeltime_fit_workflowset(fit = Kg_Ha ~ .,
    data    = training(splits),
    control = control_fit_workflowset(
      verbose   = FALSE,
      allow_par = TRUE
    )
  )

model_parallel_tbl

model_parallel_calibrate <- model_parallel_tbl %>%
  modeltime_calibrate(testing(splits))

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE)

model_parallel_forecast <- model_parallel_calibrate %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl,
    keep_data   = TRUE
  )

model_parallel_forecast %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = TRUE
  )

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  arrange(mae) %>%
  head(10) %>%
  filter(mae %in% min(mae))

model_list[[248]]

model_fit_arima_boosted <- arima_boost(
  mode = "regression",
  trees = 4,
  min_n = 2,
  tree_depth = 8,
  learn_rate = 1
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Kg_Ha ~ .,
      data = training(splits))

model_fit_arima_boosted

###### Tune mode 2 ----

model_spec_arima_tune <- arima_boost(
  mode           = "regression",
  mtry           = tune(),
  trees          = tune(),
  min_n          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune(),
  seasonal_period = "auto",
  ) %>%
  set_engine("auto_arima_xgboost")

wflw_spec_tune_arima <- workflow() %>%
  add_model(model_spec_arima_tune) %>%
  add_recipe(recipe_spec_2)

resamples_kfold <- rendimiento_tscv

tune_results_arima <- tune_grid(
  object = wflw_spec_tune_arima,
  resamples = resamples_kfold,
  param_info = parameters(wflw_spec_tune_arima), #%>%
    #update(learn_rate = learn_rate(range = c(0.1, 1), trans = NULL)),
  grid = 100,
  control = control_grid(verbose = FALSE, allow_par = TRUE))

best_results <- tune_results_arima %>%
  show_best(metric = "rmse", n = 10) %>% as.data.frame()
best_results

model_fit_arima_boosted <- wflw_spec_tune_arima %>%
  finalize_workflow(parameters = best_results %>% slice(1)) %>%
  fit(training(splits))

##### Catboost Auto ARIMA ----

# install.packages('devtools')
# devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.20/catboost-R-Windows-0.20.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
# 
# devtools::install_github("AlbertoAlmuinha/boostime", force = TRUE)

# devtools::install_github("gadenbuie/xaringanExtra")
# xaringanExtra::use_panelset()

model_arima_catboost <- boost_arima() %>%
  set_engine("auto_arima_catboost", verbose = 0) %>%
  fit(Kg_Ha ~ ., data = training(splits))

###### Tune----

model_spec_arima_catboost_tune <- boost_arima(
  mode           = "regression",
  mtry           = tune(),
  trees          = tune(),
  min_n          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune()) %>%
  set_engine("auto_arima_catboost")

wflw_spec_tune_arima_catboost <- workflow() %>%
  add_model(model_spec_arima_catboost_tune) %>%
  add_recipe(recipe_spec)

resamples_kfold <- rendimiento_tscv

tune_results_arima_catboost <- tune_grid(
  object = wflw_spec_tune_arima_catboost,
  resamples = resamples_kfold,
  param_info = parameters(wflw_spec_tune_arima_catboost) %>%
    update(learn_rate = learn_rate(range = c(0.1, 1), trans = NULL)),
  grid = 100,
  control = control_grid(verbose = FALSE, allow_par = TRUE))

best_results <- tune_results_arima_catboost %>%
  show_best(metric = "rmse", n = 10) %>% as.data.frame()
best_results

model_fit_arima_boosted <- wflw_spec_tune_arima_catboost %>%
  finalize_workflow(parameters = best_results %>% slice(1)) %>%
  fit(training(splits))

##### Catboost Prophet ----

model_prophet_catboost <-
  boostime::boost_prophet() %>%
  set_engine("prophet_catboost", verbose = 0)%>%
  fit(Kg_Ha ~ ., data = training(splits))

###### Tune ----

model_spec_prophet_catboost <- boostime::boost_prophet(growth = "linear",
                                                       changepoint_num = tune(),
                                                       changepoint_range = tune(),
                                                       seasonality_yearly = tune(),
                                                       seasonality_daily = tune(),
                                                       seasonality_weekly = tune(),
                                                       season = tune(),
                                                       trees = tune(),
                                                       tree_depth = tune(),
                                                       learn_rate = tune(),
                                                       mtry = tune()) %>%
  set_engine("prophet_catboost", verbose = 0)

model_spec_prophet_catboost

wflw <- workflow() %>%
  add_model(model_spec_prophet_catboost) %>%
  add_recipe(recipe_spec)

wflw

# library(foreach)
# library(doParallel)
# parallel_stop()
# foreach::registerDoSEQ()
# cluster <- makeCluster(2)
# doParallel::registerDoParallel(cluster)
# stopImplicitCluster()


set.seed(1234)
tune_results <- tune_grid(
  object     = wflw,
  resamples  = rendimiento_tscv,
  param_info = parameters(wflw),
  grid       = 5#,
  # control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = "everything")
)

tuned_best <- tune_results %>%
  select_best("rmse")

fin_wflw <- wflw %>%
  finalize_workflow(parameters = tuned_best)

wflw_fit <- fin_wflw %>%
  fit(training(splits))

##### Neural Prophet ----

# Install Development Version 
# remotes::install_github("AlbertoAlmuinha/neuralprophet", force = TRUE)

# Neural Prophet Installation - Run 1st time
# install_nprophet()

model_fit_nprophet <- neural_prophet(
  mode = "regression",
  #freq            = "M",
  growth          = "linear",
  #trend_reg       = 3,
  # learn_rate      = 0.1,
  # changepoint_range = 0.8,
  #seasonality_mode = "additive"
) %>%
  set_engine("prophet") %>%
  fit(Kg_Ha ~ Fecha, training(splits))

# modeltime_table(
#   model_fit_nprophet
# ) %>%
#   modeltime::modeltime_calibrate(new_data = testing(splits)) %>%
#   modeltime::modeltime_forecast(
#     new_data      = testing(splits),
#     actual_data   = rendimiento_tbl,
#     conf_interval = 0.95
#   ) %>%
#   plot_modeltime_forecast(.interactive = TRUE)

#### New Hybrid Models ----

#### Prophet Boost ----

###### Tune mode 1 ----

model_tbl <- expand.grid(
  learn_rate = as.numeric(c(0.001, 0.010, 0.100, 0.350, 0.500, 0.650, 0.8, 1)),
  min_n = c(2,5,15,50,100,250),
  tree_depth = c(1,2,4,6,8,10),
  trees = c(2,4,6,8,10)
) %>%
  create_model_grid(
    f_model_spec = prophet_boost,
    engine_name  = "prophet_xgboost",
    mode         = "regression"
  )
# model_tbl

model_list <- model_tbl$.models
model_list

model_wfset <- workflow_set(
  preproc = list(
    recipe_spec
  ),
  models = model_list, 
  cross = TRUE
)
model_wfset

model_parallel_tbl <- model_wfset %>%
  modeltime_fit_workflowset(fit = Kg_Ha ~ .,
                            data    = training(splits),
                            control = control_fit_workflowset(
                              verbose   = FALSE,
                              allow_par = TRUE
                            )
  )

model_parallel_tbl

model_parallel_calibrate <- model_parallel_tbl %>%
  modeltime_calibrate(testing(splits))

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

model_parallel_forecast <- model_parallel_calibrate %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl,
    keep_data   = TRUE
  )

model_parallel_forecast %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = TRUE
  )

model_parallel_calibrate %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  head(10) %>%
  filter(rmse %in% min(rmse))

model_list[[1031]]

model_spec_prophet_boost <- prophet_boost(
  mode = "regression",
  trees = 8,
  min_n = 15,
  tree_depth = 6,
  learn_rate = 0.8
) %>%
  set_engine("prophet_xgboost") #, yearly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

###### Tune mode 2 ----

model_spec_prophet_boost_tune <- prophet_boost(
  mode           = "regression",
  mtry           = tune(),
  trees          = tune(),
  min_n          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune()) %>%
  set_engine("prophet_xgboost")

wflw_spec_tune_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost_tune) %>%
  add_recipe(recipe_spec_2)

resamples_kfold <- rendimiento_tscv

tune_results_prophet_boost <- tune_grid(
  object = wflw_spec_tune_prophet_boost,
  resamples = resamples_kfold,
  param_info = parameters(wflw_spec_tune_prophet_boost), #%>%
    # update(learn_rate = learn_rate(range = c(0.1, 1), trans = NULL)),
  grid = 100,
  control = control_grid(verbose = FALSE, allow_par = TRUE))

best_results <- tune_results_prophet_boost %>%
  show_best(metric = "rmse", n = 10) %>% as.data.frame()
best_results

workflow_fit_prophet_boost <- wflw_spec_tune_prophet_boost %>%
  finalize_workflow(parameters = best_results %>% slice(1)) %>%
  fit(training(splits))

### The Modeltime Workflow ----

#### Modeltime Table ----

model_table <- modeltime_table(
  model_fit_arima,
  model_fit_naive,
  model_fit_snaive,
  model_fit_prophet,
  model_fit_arima_boosted,
  model_fit_ets,
  workflow_fit_lm,
  #workflow_fit_mars,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_xgb,
  workflow_fit_prophet_boost
  # model_fit_nprophet,
  # model_arima_catboost,
  # model_prophet_catboost
) 

model_table

#### Modeltime fit resamples ----

resamples_fitted <- model_table %>%
  modeltime_fit_resamples(
    resamples = rendimiento_tscv,
    control   = control_resamples(verbose   = FALSE,
                                  allow_par = TRUE)
  )

resamples_fitted

resamples_fitted %>%
  plot_modeltime_resamples(
    .point_size  = 2, 
    .point_alpha = 0.8,
    .interactive = TRUE
  )

resamples_fitted %>%
  modeltime_resample_accuracy(summary_fns = mean) %>%
  table_modeltime_accuracy(.interactive = TRUE)

resamples_fitted %>%
  modeltime_resample_accuracy(summary_fns = mean) %>%
  arrange(rmse) %>%
  head(5)

#### Performance (Training Set) ----

##### Forecast (Training Set) ----

calibration_table_train <- model_table %>%
  modeltime_calibrate(training(splits), quiet = FALSE)

calibration_table_train %>%
  modeltime_forecast(actual_data = training(splits),
                     new_data = training(splits)) %>%
  plot_modeltime_forecast(.interactive = TRUE)

##### Accuracy (Training Set) ----

calibration_table_train

calibration_table_train %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

#### Performance (Testing Set) ----

##### Calibration ----

model_table <- modeltime_table(
  model_fit_arima,
  model_fit_naive,
  model_fit_snaive,
  model_fit_prophet,
  model_fit_arima_boosted,
  #model_fit_ets,
  workflow_fit_lm,
  #workflow_fit_mars,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_xgb,
  workflow_fit_prophet_boost
  # model_fit_nprophet
) 

model_table

calibration_table <- model_table %>%
  modeltime::modeltime_calibrate(actual_data = rendimiento_tbl,
                                 new_data = testing(splits),
                                 quiet = FALSE)

calibration_table

##### Forecast (Testing Set) ----

calibration_table %>%
  modeltime_forecast(actual_data = rendimiento_tbl,
                     new_data = testing(splits)) %>%
  plot_modeltime_forecast(.interactive = TRUE)

forecasting_test <- calibration_table %>%
  modeltime_forecast(actual_data = rendimiento_tbl,
    new_data = testing(splits))

##### Accuracy (Testing Set) ----

calibration_table %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

#### Refit and Forecast Forward ----

future_forecast_tbl <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id %in% c(2,5,6,7,8)) %>%
  
  # Refit and Forecast Forward
  modeltime::modeltime_refit(rendimiento_tbl,
                             control = control_refit(verbose = FALSE,
                                                     allow_par = TRUE)) %>%
  modeltime::modeltime_forecast(actual_data = rendimiento_tbl_complete,
                                new_data = rendimiento_new_data) 

future_forecast_tbl %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

##### Calibration external table ----

calibration_table_external_test <- model_table %>%
  modeltime_calibrate(actual_data = rendimiento_tbl_complete,
                      new_data = rendimiento_new_data) 

calibration_table_external_test

##### External test Accuracy ----

calibration_table_external_test %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

##### External test Plot ----

# calibration_table_external_test %>%
#   modeltime_forecast(actual_data = rendimiento_tbl_complete,
#                      new_data = rendimiento_new_data) %>%
#   plot_modeltime_forecast(.interactive = TRUE)

calibration_table_external_test %>%
  modeltime::modeltime_refit(rendimiento_tbl,
                             control = control_refit(verbose = FALSE,
                                                     allow_par = TRUE))%>%
  modeltime_forecast(actual_data = rendimiento_tbl_complete,
                     new_data = rendimiento_new_data) %>%
  plot_modeltime_forecast(.interactive = TRUE)

calibration_table_external_test %>%
  modeltime::modeltime_refit(rendimiento_tbl,
                             control = control_refit(verbose = FALSE,
                                                     allow_par = TRUE))%>%
  modeltime_forecast(actual_data = rendimiento_tbl_complete,
                     new_data = rendimiento_new_data,
                     conf_interval = 0.8) %>%
  mutate(.value = exp(.value)+1,
         .conf_lo = exp(.conf_lo)+1,
         .conf_hi = exp(.conf_hi)+1) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#### Build Modeltime Ensembles ----

submodel_table <- modeltime_table(
  #model_fit_prophet,
  #workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_xgb
  #workflow_fit_prophet_boost
  #model_fit_arima_boosted
  ) 

submodel_table

submodel_table %>% 
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy(testing(splits)) %>%
  table_modeltime_accuracy(.interactive = TRUE)

submodel_table %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

###### Simple Average Ensemble ----
ensemble_fit_avg <- submodel_table %>%
  ensemble_average(type = "mean")
###### Simple Median Ensemble ----
ensemble_fit_med <- submodel_table %>%
  ensemble_average("median")
##### Higher Loading on Better Models (Test RMSE) ----
ensemble_fit_wt <- submodel_table %>%
  ensemble_weighted(loadings = c(1, 3),
                    scale_loadings = TRUE)

ensemble_models_tbl <- modeltime_table(
  ensemble_fit_avg,
  ensemble_fit_med,
  ensemble_fit_wt
)
ensemble_models_tbl

ensemble_models_tbl %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy(testing(splits)) %>%
  table_modeltime_accuracy(.interactive = TRUE
                           )
ensemble_models_tbl %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = rendimiento_tbl
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#### Modeltime fit resamples ----

# submodel_predictions <- submodel_table %>%
#   modeltime_fit_resamples(
#     resamples = rendimiento_tscv,
#     control = control_resamples(verbose   = TRUE,
#                                 allow_par = TRUE)
#   )

resamples_fitted <- submodel_table %>%
  modeltime_fit_resamples(
    resamples = rendimiento_tscv,
    control   = control_resamples(verbose   = FALSE,
                                  allow_par = TRUE)
  )

resamples_fitted

resamples_fitted %>%
  plot_modeltime_resamples(
    .point_size  = 2, 
    .point_alpha = 0.8,
    .interactive = TRUE
  )

resamples_fitted %>%
  modeltime_resample_accuracy(summary_fns = mean) %>%
  table_modeltime_accuracy(.interactive = TRUE)

resamples_fitted %>%
  modeltime_resample_accuracy(summary_fns = mean) %>%
  arrange(rmse) %>%
  head(5)

##### With Metalearner Tuning ----
  
ensemble_fit_glmnet <- resamples_fitted %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet"),
    grid = 100,
    control = control_grid(allow_par = TRUE,verbose = FALSE)
  )
ensemble_fit_glmnet

#### Auto ML ----

Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")

h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

recipe_spec <- recipe(Kg_Ha ~ ., data = training(splits)) %>%
  step_timeseries_signature(Fecha) 
# train_tbl <- training(splits) %>% bake(prep(recipe_spec), .)
# test_tbl  <- testing(splits) %>% bake(prep(recipe_spec), .)

model_spec_automl <- automl_reg(mode = 'regression') %>%
  set_engine(
    engine                     = 'h2o',
    max_runtime_secs           = 5, 
    max_runtime_secs_per_model = 3,
    max_models                 = 3,
    nfolds                     = 5,
    exclude_algos              = c("DeepLearning"),
    verbosity                  = FALSE#,
    # seed                       = 786
  ) 
model_spec_automl

model_fitted_automl <- model_spec_automl %>%
  fit(Kg_Ha ~ ., data = training(splits))

model_fitted_automl

#### Model table ----

ensemble_models_tbl <- modeltime_table(
  ensemble_fit_avg,
  ensemble_fit_med,
  ensemble_fit_wt,
  ensemble_fit_glmnet
)

#### Performance (Training Set) ----

##### Forecast (Training Set) ----

calibration_table_train <- ensemble_models_tbl %>%
  modeltime_calibrate(training(splits), quiet = FALSE)

calibration_table_train %>%
  modeltime_forecast(actual_data = training(splits),
                     new_data = training(splits)) %>%
  plot_modeltime_forecast(.interactive = TRUE)

##### Accuracy (Training Set) ----

calibration_table_train

calibration_table_train %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

#### Performance (Testing Set) ----

##### Calibration ----

calibration_table <- ensemble_models_tbl %>%
  modeltime::modeltime_calibrate(actual_data = rendimiento_tbl,
                                 new_data = testing(splits),
                                 quiet = FALSE)

calibration_table

##### Forecast (Testing Set) ----

calibration_table %>%
  modeltime_forecast(actual_data = rendimiento_tbl,
                     new_data = testing(splits)) %>%
  plot_modeltime_forecast(.interactive = TRUE)

forecasting_test <- calibration_table %>%
  modeltime_forecast(actual_data = rendimiento_tbl,
                     new_data = testing(splits))

calibration_table %>%
  modeltime_forecast(actual_data = rendimiento_tbl,
                     new_data = testing(splits)) %>%
  # mutate(.value = exp(.value)+1,
  #        .conf_lo = exp(.conf_lo)+1,
  #        .conf_hi = exp(.conf_hi)+1) %>%
  plot_modeltime_forecast(.interactive = TRUE)

##### Accuracy (Testing Set) ----

calibration_table %>%
  modeltime_accuracy() %>%
  arrange(mape) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

#### Refit and Forecast Forward ----

##### Model table ----

ensemble_models_tbl <- modeltime_table(
  ensemble_fit_avg,
  ensemble_fit_med,
  ensemble_fit_wt,
  ensemble_fit_glmnet
)

##### Calibration table ----

calibration_table_external_test <- ensemble_models_tbl %>%
  modeltime_calibrate(actual_data = rendimiento_tbl_complete,
                      new_data = rendimiento_new_data,
                      quiet = FALSE) 

calibration_table_external_test

##### Forecast with refiting ----

future_forecast_tbl <- calibration_table_external_test %>%
  modeltime::modeltime_refit(rendimiento_tbl,
                             resamples = rendimiento_tscv,
                             control = control_refit(verbose = FALSE,
                                                     allow_par = TRUE)) %>%
  modeltime::modeltime_forecast(actual_data = rendimiento_tbl_complete,
                                new_data = rendimiento_new_data) 

##### Forecast plot ----

future_forecast_tbl %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

##### Forecast Accuracy Table ----

calibration_table_external_test %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>% 
  table_modeltime_accuracy(.interactive = TRUE)

##### Forecast plot final ----

calibration_table_external_test %>%
  modeltime::modeltime_refit(rendimiento_tbl,
                             resamples = rendimiento_tscv,
                             control = control_refit(verbose = FALSE,
                                                     allow_par = TRUE)) %>%
  modeltime_forecast(actual_data = rendimiento_tbl_complete,
                     new_data = rendimiento_new_data,
                     conf_interval = 0.8) %>%
  mutate(.value = exp(.value)+1,
         .conf_lo = exp(.conf_lo)+1,
         .conf_hi = exp(.conf_hi)+1) %>%
  plot_modeltime_forecast(.interactive = TRUE)

#### Proyección campaña 2022 - I ----

newdata <- data %>%
  filter(Año %in% c(2019,2020,2021)) %>%
  dplyr::mutate(`Temp. Exterior (°C)` = na_mean(`Temp. Exterior (°C)`, option="median"),
                `Temp. Maxima (°C)` = na_mean(`Temp. Maxima (°C)`, option="median"),
                `Temp. Minima (°C)` = na_mean(`Temp. Minima (°C)`, option="median"),
                `Temp. Media (°C)` = na_mean(`Temp. Media (°C)`, option="median"),
                `Humedad Externa (%)` = na_mean(`Humedad Externa (%)`, option="median"),
                `Punto Rocio (°C)` = na_mean(`Punto Rocio (°C)`, option="median"),
                `Viento Durante un Intervalo` = na_mean(`Viento Durante un Intervalo`, option="median"),
                `Velocida Max (km/hr)` = na_mean(`Velocida Max (km/hr)`, option="median"),
                `Factor enfria por Viento (°C)` = na_mean(`Factor enfria por Viento (°C)`, option="median"),
                `Indice de Calor` = na_mean(`Indice de Calor`, option="median"),
                `Indice de Tem. Hum. Viento` = na_mean(`Indice de Tem. Hum. Viento`, option="median"),
                `Presion Atmosfera (mm)` = na_mean(`Presion Atmosfera (mm)`, option="median"),
                `Radiacion Solar (w/m2)` = na_mean(`Radiacion Solar (w/m2)`, option="median"),
                `Energia solar (w/m2)` = na_mean(`Energia solar (w/m2)`, option="median"),
                `Alta Radiacion sol. (w/m2)` = na_mean(`Alta Radiacion sol. (w/m2)`, option="median"),
                `Indice Rayos UV` = na_mean(`Indice Rayos UV`, option="median"),
                `Dosis UV` = na_mean(`Dosis UV`, option="median"),
                `Alta Radiacion UV` = na_mean(`Alta Radiacion UV`, option="median"),
                `Grados Caloricos dia (°C)` = na_mean(`Grados Caloricos dia (°C)`, option="median"),
                `Grados Frios (°C)` = na_mean(`Grados Frios (°C)`, option="median"),
                `Temp. Interna (°C)` = na_mean(`Temp. Interna (°C)`, option="median"),
                `Hum. Interna (%)` = na_mean(`Hum. Interna (%)`, option="median"),
                `Rocio` = na_mean(`Rocio`, option="median"),
                `Calor` = na_mean(`Calor`, option="median"),
                `Evapotranspiracion (mm)` = na_mean(`Calor`, option="median"),
                #`Estadistica recepcion paquetes anemometro` = na_mean(`Estadistica recepcion paquetes anemometro`, option="median"),
                #`Recepcion datos por conjunto integ. sensores (%)` =  na_mean(`Recepcion datos por conjunto integ. sensores (%)`, option="median"),
                `Horas Calor` = na_mean(`Horas Calor`, option="median"),
                `Horas Frio` = na_mean(`Horas Frio`, option="median")) %>%
  # mutate(Date = ceiling_date(Fecha, "week"))%>%
  dplyr::group_by(DiadelAño) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)%>%
  dplyr::ungroup()%>%
  # dplyr::mutate(Año = year(Date),
  #               Semana = isoweek(Date)) %>%
  dplyr::select(-`Estadistica recepcion paquetes anemometro`,
                -`Recepcion datos por conjunto integ. sensores (%)`,
                #-DiadelAño,
                -DiadelMes,
                -DiadelaSemana,
                -Año,- Semana, -Kg_Total) %>%
  dplyr::mutate(#WorkDay = ifelse(Kg_Ha == 0, 0, 1),
                Kg_Ha = log(Kg_Ha+1)) %>%
  left_join(data %>% 
              select(Año,DiadelAño,Kg_Ha) %>%
              filter(Año %in% 2021) %>%
              dplyr::mutate(WorkDay = ifelse(Kg_Ha == 0, 0, 1)) %>%
              select(DiadelAño, WorkDay),
            by = "DiadelAño") %>%
  mutate(Año = 2022) %>% 
  mutate(Fecha = as.Date("2022-01-01")+(DiadelAño-1)) %>%
  filter(Fecha >= "2022-06-15",
         !DiadelAño %in% 366,
         !is.na(WorkDay))

future_forecast_tbl <- calibration_table %>%
  # Remove ARIMA model with low accuracy
  #filter(.model_id %in% c(2,5,6,7,8)) %>%
  
  # Refit and Forecast Forward
  modeltime::modeltime_refit(rendimiento_tbl,
                             resamples = rendimiento_tscv) %>%
  modeltime::modeltime_forecast(actual_data = rendimiento_tbl_complete,
                                new_data = newdata,
                                conf_interval = 0.8) %>%
  mutate(.value = exp(.value)-1,
         .conf_lo = exp(.conf_lo)-1,
         .conf_hi = exp(.conf_hi)-1)

future_forecast_tbl %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

proyeccion <- future_forecast_tbl %>% 
  select(-.model_id,-.key) %>%
  filter(!.model_desc %in% "ACTUAL") %>%
  pivot_wider(values_from = c(".value",".conf_lo",".conf_hi"),
              names_from = .model_desc) %>%
  rename(Fecha = .index) %>%
  left_join(newdata, by = "Fecha")

write.csv(proyeccion, "proyeccion.csv", row.names = F)
