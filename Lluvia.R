# Cambiar el directorio de trabajo ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargar paquetes ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readxl, dplyr, tidyr, imputeTS, ISOweek,
               tidyverse, tidymodels, timetk, lubridate, parallel,
               tictoc, data.table, outliers, caret,
               caTools, pROC, isofor, R.matlab, h2o)

# Iniciar H2o ----

h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

h2o.init()

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
    `Temp. Media (?C)` = (`Temp. Maxima (?C)`+`Temp. Minima (?C)`)/2,
    `Lluvia yn` = factor(ifelse(`Lluvia (mm)` > 0, 1, 0), levels = c(0,1),
                         labels = c("No","Si"))) %>%
  dplyr::select_if(all_na)%>%
  # janitor::clean_names()
  # spaceless() %>%
  dplyr::mutate(#Time = round(Time*24,0),
                DiadelA?o = as.numeric(format(Date, format = "%j")),
                DiadelMes = as.numeric(format(Date, format = "%d")),
                DiadelaSemana = as.numeric(format(Date, format = "%u")),
                #Semana = as.numeric(format(Date, format = "%W")),
                Mes = as.numeric(format(Date, format = "%m")),
                A?o = lubridate::year(Date)) %>%
  dplyr::group_by(Fundo,Date,Time) %>%
  dplyr::mutate(`Dias Grado` = (`Temp. Exterior (?C)` - 10)/24,
                `Dias Grado` = ifelse(`Dias Grado` < 0, 0, `Dias Grado`)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Fundo,Date) %>%
  dplyr::mutate(`Horas Calor` = sum(`Temp. Minima (?C)`>=24, na.rm = TRUE)/n()*24,
                `Horas Frio` = sum(`Temp. Maxima (?C)`<=15, na.rm = TRUE)/n()*24,
                `Dias Grado` = sum(`Dias Grado`)/n()*24,
                `Dias Grado` = ifelse(`Dias Grado` < 0, 0, `Dias Grado`)) %>%
  dplyr::ungroup() %>%
  tidyr::replace_na(list(`Dias Grado` = 0)) %>%
  group_by(Fundo, Date) %>%
  dplyr::mutate(`Horas Calor cumsum` = cumsum(`Horas Calor`),
                `Horas Frio cumsum` = cumsum(`Horas Frio`),
                `Radiacion Solar (w/m2) cumsum` = cumsum(`Radiacion Solar (w/m2)`),
                `Dias Grado cumsum` = cumsum(`Dias Grado`),
                `Evapotranspiracion (mm) cumsum` = cumsum(`Evapotranspiracion (mm)`),
                `Lluvia (mm) cumsum` = cumsum(`Lluvia (mm)`)) %>%
  ungroup()

contrasts(data$`Lluvia yn`)

table(data$`Lluvia yn`)
prop.table(table(data$`Lluvia yn`))*100

# write.csv(clima, "data_clima.csv", row.names = F)

### Creaci?n de matriz ----

datos <- data %>%
  # select(-`Lluvia (mm) cumsum`,
  #        -`Lluvia (mm)`,
  #        -`Alta Dir. Viento`,
  #        -`Dir. Viento`,
  #        -`Estadistica recepcion paquetes anemometro`,
  #        -`Duracion intervalo archivo`,
  #        -`Recepcion datos por conjunto integ. sensores (%)`,
  #        -`Intervalo toma datos (min)`,
  #        -Fecha,
  #        -Fundo,
  #        -Date,
  #        -Datetime)
select(Time,
       `Lluvia yn`,
       `Temp. Maxima (?C)`,
       `Temp. Minima (?C)`,
       `Humedad Externa (%)`,
       `Radiacion Solar (w/m2)`,
       `Presion Atmosfera (mm)`,
       `Horas Calor cumsum`,
       `Horas Frio cumsum`,
       `Radiacion Solar (w/m2) cumsum`,
       `Dias Grado cumsum`)


#### An?lisis de valores perdidos ----

# Para ver que filas por columna tienen valores perdidos
rowmiss <- which(rowSums(is.na(datos)) != 0)
rowmiss

# Para ver el porcentaje de valores perdidos en las filas
per.miss.row = 100 * rowSums(is.na(datos[rowmiss, ]))/dim(datos)[2]
per.miss.row

visdat::vis_miss(datos, warn_large_data = FALSE)

# Para ver que columnas tienen valores perdidos
colmiss <- which(colSums(is.na(datos)) != 0)
colmiss

# Para ver el porcentaje de valores perdidos en las columnas
per.miss.col = 100 * colSums(is.na(datos[, colmiss]))/dim(datos)[1]
per.miss.col

# Aggregation plot
a = VIM::aggr(datos, numbers = T)

VIM::aggr(datos, numbers = T, sortComb = TRUE, sortVar = TRUE, only.miss = TRUE)

x11()
VIM::aggr(datos, numbers = T, sortComb = TRUE, sortVar = TRUE, only.miss = TRUE)

visdat::vis_miss(datos, warn_large_data = FALSE)

VIM::matrixplot(datos, sortby = "Date",interactive = TRUE)

VIM::countInf(data$`Temp. Maxima (?C)`)
VIM::countInf(data$`Temp. Minima (?C)`)
VIM::countInf(data$`Horas Calor cumsum`)
VIM::countInf(data$`Horas Frio cumsum`)
VIM::countInf(data$`Radiacion Solar (w/m2) cumsum`)
VIM::countInf(data$`Dias Grado cumsum`)
VIM::countInf(data$`Evapotranspiracion (mm) cumsum`)
VIM::countInf(data$`Lluvia (mm) cumsum`)

#### Getting Started ----

cores <- parallel::detectCores(logical = TRUE) - 1
modeltime::parallel_start(cores)

# library(doFuture)
# registerDoFuture()
# n_cores <- parallel::detectCores()-1
# plan(
#   strategy = cluster,
#   workers = parallel::makeCluster(n_cores))

# library(benchmarkme)

# res <- benchmarkme::benchmark_std(runs = 3)

#### Train / Test ----

datos.a <- datos %>% as.data.table() %>% 
  recipe()%>% 
  update_role(`Lluvia yn`, new_role = "label") %>%
  update_role(-`Lluvia yn`, new_role = "predictor") %>%
  step_nzv(all_predictors()) %>%
  prep() %>% bake(new_data = NULL) %>% 
  na.omit() %>%
  rename(`Temp. Maxima` = `Temp. Maxima (?C)`,
         `Temp. Minima` = `Temp. Minima (?C)`,
         `Humedad Externa` = `Humedad Externa (%)`,
         `Presion Atmosfera` = `Presion Atmosfera (mm)`,
         `Radiacion Solar cumsum` = `Radiacion Solar (w/m2) cumsum`)

write.csv(x = datos.a, file = "Lluvia procesado.csv", row.names = F)

set.seed(2021) 
index         <- createDataPartition(datos.a$`Lluvia yn`, 
                                     p=0.8, 
                                     list=FALSE)
train   <- datos.a[ index, ]
testing <- datos.a[-index, ]

# Verificando que se mantenga la proporci?n original
round(prop.table(table(datos.a$`Lluvia yn`))*100,2)
round(prop.table(table(train$`Lluvia yn`))*100,2)
round(prop.table(table(testing$`Lluvia yn`))*100,2)

#----------------------------------------#
# 1. Entrenando con Regresi?n Log?stica  #
#----------------------------------------#

# 1.1 Modelo log?stico con todas las variables ----------------
modelo_rl  <- glm(`Lluvia yn` ~ ., 
                  family=binomial,
                  data=train)

modelo_rl

# Prediciendo la probabilidad
testing$proba.pred <- predict(modelo_rl,testing,type="response")
head(testing$proba.pred)

# Prediciendo la clase con punto de corte (umbral) igual a 0.5
testing$clase.pred <- factor(ifelse(testing$proba.pred >= 0.5, 
                                    1,0))
head(testing$clase.pred)

#------------------------------------------#
# 2. Entrenar con Detecci?n de Anomal?as   #
#      Algoritmo Isolation Forest          #
#------------------------------------------#

# 2.1 Modelo con Isolation Forest -----------------------------
set.seed(2021)
modelo_iso_forest <- iForest(train %>%
                               # select(`Lluvia yn`,
                               #        `Temp. Maxima (?C)`,
                               #        `Temp. Minima (?C)`,
                               #        `Humedad Externa (%)`,
                               #        `Presion Atmosfera (mm)`,
                               #        # DiadelA?o
                               #        ) %>%
                               select(-`Lluvia yn`), # No considerar al target
                             nt  = 100,   # n?mero de ?rboles
                             phi = 1000)  # Tama?o de la submuestra 
# para construir cada ?rbol

# Determinar el Anomaly Score 
testing$iso_score <- predict(modelo_iso_forest,testing%>%
                               select(-`Lluvia yn`))
head(testing$iso_score)
summary(testing$iso_score)

ggplot(testing) + aes(x = iso_score) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(testing$iso_score,seq(0,1,0.05)),
    color      = "red",
    linetype   = "dashed") +
  labs(title = "Distribuci?n de los scores del Isolation Forest") +
  theme_bw()


# Boxplot del Anomaly Score vs. Target
ggplot(testing, aes(`Lluvia yn`,iso_score)) + 
  geom_boxplot(fill=c("cadetblue","firebrick1")) 

# Calcular el umbral m?s alto para el iso_score
high_iso <- quantile(testing$iso_score, probs = 0.95)  
high_iso

# Determinar la clase predicha usando el Binary Isolation Score
testing$binary_iso2 <- as.numeric(testing$iso_score >= high_iso)
head(testing$binary_iso2)
tail(testing$binary_iso2)

summary(testing$binary_iso2)

testing$binary_iso2         <- factor(testing$binary_iso2)
levels(testing$binary_iso2) <- c(0,1)

#-----------------------------------#
# 3. Mejorando indicadores usando   #
#     punto de corte óptimo        #
#     Algoritmo Isolation Forest    #
#-----------------------------------#

# 3.1 Obteniendo el umbral ?ptimo en Isolation Forest ---------
modelroc1 <- roc(testing$`Lluvia yn`,testing$iso_score) # Clase Real vs Proba predicha
plot(modelroc1, 
     print.auc=TRUE, 
     print.thres=TRUE,
     auc.polygon=TRUE,
     col="blue",
     auc.polygon.col="lightblue",
     xlab="1 - Especificidad", 
     ylab="Sensibilidad")

umbral1 <- pROC::coords(modelroc1, "best")$threshold
umbral1

# Cambiando el punto de corte (umbral ?ptimo)
testing$binary_iso3 <- as.numeric(testing$iso_score >= umbral1)
head(testing$binary_iso3)
tail(testing$binary_iso3)

summary(testing$binary_iso3)

testing$binary_iso3         <- factor(testing$binary_iso3)
levels(testing$binary_iso3) <- c(0,1)

#---------------------------------#
# 4. Mejorando indicadores usando #
#    punto de corte ?ptimo        #
#    Regresi?n Log?stica          #
#---------------------------------#

# 4.1 Obteniendo el umbral ?ptimo en Regresi?n Log?stica ------
modelroc2 <- roc(testing$`Lluvia yn`,testing$proba.pred) # Clase Real vs Proba predicha
plot(modelroc2, 
     print.auc=TRUE, 
     print.thres=TRUE,
     auc.polygon=TRUE,
     col="blue",
     auc.polygon.col="lightblue",
     xlab="1 - Especificidad", 
     ylab="Sensibilidad")

umbral2 <- pROC::coords(modelroc2, "best")$threshold
umbral2

# Cambiando el punto de corte (umbral ?ptimo)
testing$clase.pred.u <- as.numeric(testing$proba.pred >= umbral2)
head(testing$clase.pred.u)
tail(testing$clase.pred.u)

summary(testing$clase.pred.u)

testing$clase.pred.u         <- factor(testing$clase.pred.u)
levels(testing$clase.pred.u) <- c(0,1)

# 5. Comparando los cuatro modelos ----------------------------

result1 <- caret::confusionMatrix(factor(testing$clase.pred,levels = c(0,1)),
                                  testing$`Lluvia yn`,
                                  positive = "1")

colAUC(testing$proba.pred,testing$`Lluvia yn`,plotROC = TRUE) -> auc1

result2 <- caret::confusionMatrix(testing$binary_iso2,
                                  testing$`Lluvia yn`,
                                  positive = "1")

colAUC(testing$iso_score,testing$`Lluvia yn`,plotROC = TRUE) -> auc2

result3 <- caret::confusionMatrix(testing$binary_iso3,
                                  testing$`Lluvia yn`,
                                  positive="1")

colAUC(testing$iso_score,testing$`Lluvia yn`,plotROC = TRUE) -> auc3

result4 <- caret::confusionMatrix(testing$clase.pred.u,
                                  testing$`Lluvia yn`,
                                  positive="1")

colAUC(testing$proba.pred,testing$`Lluvia yn`,plotROC = TRUE) -> auc4

modelos  <- c("Log?stica",
              "Logistica-umbral ?ptimo",
              "IsoFor-umbral 5%",
              "IsoFor-umbral ?ptimo")

umbrales <- c(0.5,umbral2,high_iso,umbral1)

sensibilidad  <- c(result1$byClass["Sensitivity"],
                   result4$byClass["Sensitivity"],
                   result2$byClass["Sensitivity"],
                   result3$byClass["Sensitivity"])

especificidad <- c(result1$byClass["Specificity"],
                   result4$byClass["Specificity"],
                   result2$byClass["Specificity"],
                   result3$byClass["Specificity"])

accuracy      <- c(result1$overall["Accuracy"],
                   result4$overall["Accuracy"],
                   result2$overall["Accuracy"],
                   result3$overall["Accuracy"])

acc_bal       <- c(result1$byClass["Balanced Accuracy"],
                   result4$byClass["Balanced Accuracy"],
                   result2$byClass["Balanced Accuracy"],
                   result3$byClass["Balanced Accuracy"])

auc           <- c(auc1, auc4, auc2, auc3)

comparacion <- data.frame(modelos,
                          umbrales,
                          sensibilidad,
                          especificidad,
                          accuracy,
                          acc_bal,
                          auc)

comparacion

## Tidymodels

RNGkind(sample.kind = "Rounding")
set.seed(123)
data_split <- initial_split(datos.a, strata = "Lluvia yn", prop = 0.8)
datos_train <- training(data_split) 
datos_test <- testing(data_split)

# Check same distribution of output
bind_rows(
  as.data.frame(round(prop.table(table(datos_train$`Lluvia yn`)),4)) %>% 
    mutate(Data = "Train"),
  as.data.frame(round(prop.table(table(datos_test$`Lluvia yn`)),4)) %>%
    mutate(Data = "Test")
) %>%
  spread(Var1, Freq) %>%
  knitr::kable(style = "pandoc",
        align = c('c','c','c'))

library(tidymodels)
library(parsnip)
library(rsample)
library(mlbench)

# set.seed(1989)
# invisible(
#   feature_select_ranger <-
#     train(`Lluvia yn` ~ .,
#           data = datos_train,
#           method = "ranger",
#           metric = "ROC",
#           importance = 'impurity',
#           preProcess = c("center", "scale"),
#           tuneLength = 3,
#           trControl = 
#             trainControl(
#               verboseIter = FALSE,
#               savePredictions = TRUE,
#               summaryFunction = twoClassSummary,
#               classProbs = TRUE,
#               index = createFolds(datos_train[, "Lluvia yn"], k = 5)
#             )
#     )
# )

# view importance
# varImp(feature_select_ranger) 

# rm(feature_select_ranger)

# predictores <- predictors(feature_select_ranger)

# datos_train <- datos_train %>% select(`Lluvia yn`,
#                       `Dias Grado cumsum`,
#                         Time,
#                         `Temp. Maxima (?C)`,
#                       `Temp. Minima (?C)`,
#                         `Humedad Externa (%)`,
#                         `Radiacion Solar (w/m2) cumsum`)
          
recipe <-
  datos_train %>%
  recipe(`Lluvia yn` ~ .) %>%
  #step_rm(Time) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  #step_normalize(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  prep()

# Apply processing to test and training data
df_baked_train <- recipe %>% bake(datos_train) # Preprocessed training
df_baked_test <- recipe %>% bake(datos_test) # Preprocessed testing

set.seed(1989)
l_cv <- vfold_cv(datos_train, v = 5, strata = "Lluvia yn") # Cross validation

# Set the model engine
mod <-
  rand_forest(mode = "classification",
              mtry = tune(),
              trees = tune(),
              min_n = tune()) %>%
  set_engine("ranger") 

wflw_spec_tune_rf <- workflow() %>%
  add_model(mod) %>%
  add_recipe(recipe)

# Build initial model with varying parameters and cross validation
set.seed(1989)
mod_results_tbl <- 
  tune_grid(
    #model     = mod,
    #formula   = `Lluvia yn` ~ .,
    object = wflw_spec_tune_rf,
    resamples = l_cv,
    grid      = grid_random(parameters(mtry(c(1, 22)), 
                                       trees(), 
                                       min_n()), 
                            size = 50),
    metrics   = metric_set(roc_auc),
    control   = control_grid()
  )

# Store the parameters
df_parameter <- mod_results_tbl %>% select_best("roc_auc")








# random forest - 5 fold CV
mod_rf <-
  list(parameters = df_parameter,
       df = map2_df(.x = l_cv$splits,
                    .y = l_cv$id,
                    function (split = .x, fold = .y)
                    {
                      # Split the data into analysis and assessment tables
                      df_analysis <- analysis(split)
                      df_assessment <- assessment(split)
                      
                      # Build the model
                      mod_2 <-
                        rand_forest(mode = "classification",
                                    mtry = as.numeric(df_parameter["mtry"]),
                                    trees = as.numeric(df_parameter["trees"]),
                                    min_n = as.numeric(df_parameter["min_n"])
                        ) %>%
                        set_engine("ranger") %>%
                        fit(exited ~ ., data = df_analysis)
                      
                      # Summarise Predictions
                      table <-
                        tibble(fold = fold,
                               truth = df_assessment$exited,
                               .pred_Churn = 
                                 predict(mod_2,
                                         new_data = df_assessment,
                                         type = "prob")[[".pred_No"]],
                               .pred_Remain = 
                                 predict(mod_2,
                                         new_data = df_assessment,
                                         type = "prob")[[".pred_Si"]],
                               .pred_Class = 
                                 predict(mod_2, new_data = df_assessment) %>%
                                 unlist() %>%
                                 as.character()
                        ) %>%
                        mutate(.pred_Class = factor(.pred_Class))
                    })
  )

rm(mod, mod_results_tbl, df_parameter) # Clear memory

library(h2o)
# Start H2O on localhost, port 54321, with 4g of memory using all CPUs
localH2O <- h2o.init(ip = "localhost", port = 54321, nthreads= -1, 
                     max_mem_size = "4g")

# Apply processing to test and training data
# df_baked_train <- df_baked_train %>% as.h2o() # Preprocessed training
# df_baked_test <- df_baked_test %>% as.h2o() # Preprocessed testing

train.hex = as.h2o(localH2O, df_baked_train %>% as.data.frame())

# df_baked_train <- df_baked_train[-1,]
# df_baked_test <- df_baked_test[-1,]
# 
# df_baked_train[,"Lluvia yn"] <- as.factor(df_baked_train[,"Lluvia yn"])

y <- "Lluvia yn"
x <- setdiff(names(df_baked_train), y)

default_rf <- h2o.randomForest(x = x, y = y, 
                               training_frame = df_baked_train, 
                               stopping_rounds = 5, 
                               stopping_tolerance = 0.001, 
                               stopping_metric = "AUC", 
                               seed = 29, 
                               balance_classes = FALSE, 
                               nfolds = 10)


results_cross_validation <- function(h2o_model) {
  h2o_model@model$cross_validation_metrics_summary %>% 
    as.data.frame() %>% 
    select(-mean, -sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    mutate_all(as.numeric) %>% 
    select(Accuracy = accuracy, 
           AUC = auc, 
           Precision = precision, 
           Specificity = specificity, 
           Recall = recall, 
           Logloss = logloss) %>% 
    return()
}

# Use function: 
results_cross_validation(default_rf) -> ket_qua_default

# Model Performance by Graph: 
theme_set(theme_minimal())

plot_results <- function(df_results) {
  df_results %>% 
    gather(Metrics, Values) %>% 
    ggplot(aes(Metrics, Values, fill = Metrics, color = Metrics)) +
    geom_boxplot(alpha = 0.3, show.legend = FALSE) + 
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +    
    scale_y_continuous(labels = scales::percent) + 
    facet_wrap(~ Metrics, scales = "free") + 
    labs(title = "Model Performance by Some Criteria Selected", y = NULL)
}

plot_results(ket_qua_default) +
  labs(subtitle = "Model: Random Forest (h2o package)")