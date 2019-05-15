library(xgboost)
library(randomForest)
library(caret)
library(tidyverse)
library(mice)
library(doParallel)
library(corrplot)
library(furrr) # for parallelization map
library(naniar)

source("analysis/zllw_helpers.R")

# ---- read in data----


# should have 12898 obs
zllw_model <- read_csv("data/zllw_model_ready_04_27_2019.csv")


zllw_model$type <- as.factor(zllw_model$type)
zllw_model$zip <- as.factor(zllw_model$zip)
zllw_model$boro <- as.factor(zllw_model$boro)
zllw_model$NTAName <- as.factor(zllw_model$NTAName)
zllw_model$BoroCD <- as.factor(zllw_model$BoroCD)
zllw_model$beds <- as.factor(zllw_model$beds)
zllw_model$baths <- as.factor(zllw_model$baths)
zllw_model$baths_ngbr_mean <- as.factor(zllw_model$baths_ngbr_mean)
zllw_model$beds_ngbr_mean <- as.factor(zllw_model$beds_ngbr_mean)
glimpse(zllw_model) # 12898

vis_miss(zllw_model)



# ---- RF For Dataset Selection - START HERE remove ALL NAs, no imputation, GIS----

# first, let's see how no imputation for tax assessed value and feature engineering
# performs on our cleaned dataset

# baseline

zllw_na <- na.omit(zllw_model )
zllw_na_clean1 <- select(zllw_na, -c( "NTAName", "BoroCD", contains("ngbr"), pop, med_income, poverty,
                                      dist_midtown))

# all gis
zllw_na_clean2 <- zllw_na

# all interactions with GIS
prop_vars <- zllw_model %>% select(-contains("mean"),
                                   -last_sold_price_log,
                                   -contains("NTA"),
                                   -contains("BoroCD"),
                                   -lat,
                                   -lon,
                                   -zip,
                                   -period_built,
                                   -tax_assess_year,
                                   -poverty,
                                   -med_income,
                                   -type, 
                                   -age,
                                   -pop) %>% names()

prop_vars
inter_vars<- model.matrix(data = model.frame(~ ., zllw_na[, prop_vars]), ~ .^2)[,-1] %>% 
  as_tibble() %>%
  # so we take only the intneractions
  select(contains(":")) 

# combine interactions with original data 
inter_zllw <- model.matrix(data = model.frame(~ ., zllw_na), ~ .)[, -1] %>% 
  cbind(inter_vars) %>% as_tibble() 
names(inter_zllw)
inter_zllw$last_sold_price_log <- zllw_na$last_sold_price_log
# sanity check
nrow(zllw_na) == nrow(inter_zllw) # looks good
zllw_na_clean3 <- inter_zllw


datasets <- list(zllw_na_clean1, 
                 zllw_na_clean2,
                 zllw_na_clean3)
meta <- c("Baseline Features",
          "All GIS", 
          "All Inter + GIS")

# dimensions of each
datasets %>% map(~ dim(.x))


# for each dataset, we will use an out of bag random forest on the training dataset using default 
# parameters, ie. ntree = 500 and m (variables to be split) = number of variables/3

# use parallel processing with map to speed up
future::plan(multiprocess)


system.time(rf_models <- c(1:3) %>% 
  map(function(this_dataset){
    
    dataset <- datasets[[this_dataset]]
    dataset_name <- meta[[this_dataset]]
    
    print(this_dataset)
    print(dataset_name)
    print(ncol(dataset))
  # }))
    set.seed(1111)
    trainIndex <- createDataPartition(y = dataset$last_sold_price_log, list = FALSE, p = 0.80)
    
    x <- dataset%>% select(-last_sold_price_log)
    y <- dataset$last_sold_price_log
    
    train_x  <- model.matrix.lm( ~ ., data = x[trainIndex, ], na.action = "na.pass")[, -1]
    test_x <- model.matrix.lm( ~ ., data = x[-trainIndex, ], na.action = "na.pass")[, -1]
    # sanity check
    dim(train_x); dim(test_x)
    
    train_y <- y[trainIndex]
    test_y <- y[-trainIndex]
    
    # sanity check
    length(train_y);length(test_y)
    
    
    histo <- ggplot() +
      geom_density(data = data.frame(train = train_y), aes(x = train, fill = "train"), alpha = 0.5) +
      geom_density(data = data.frame(test = test_y), aes(x = test, fill = "test"), alpha = 0.5) +
      scale_fill_brewer(palette = "Set1") +
      ggtitle(paste0("Dataset ",this_dataset, ": ", dataset_name))
    # these should all be the same
    
    print(histo)
    
    # return(histo)
    
    
    # training the model
    set.seed(1)
    start_time <- Sys.time()
    system.time(base_rf <- randomForest(x = train_x,
                                        y = train_y,
                                        importance = TRUE))
    
    end_time <- Sys.time()
    
    run_time <- end_time - start_time
    # return(base_rf)
    
    
    # fitted values for training set
    rf_fit <- predict(base_rf, train_x)
    # visualizing fitted vs observed values for training set
    tibble(pred = rf_fit, #zllw_rf$predicted,
           obs = train_y) %>%
      scttrCor(aes(x = pred, y = obs)) + 
      geom_point(alpha = 0.4, size = 0.1)
    
    # performance metrics
    train_metrics <- postResample(obs = train_y, rf_fit) # 
    
    # predicted values for training set
    rf_preds <- predict(base_rf, test_x) 
    
    # performance metrics
    test_metrics <- postResample(obs = test_y, rf_preds) 
    
    
    # visualizing predicted vs observed values for testing set
    tibble(pred = rf_preds, obs = test_y) %>%
      scttrCor(aes(x = pred, y = obs)) + 
      geom_point(alpha = 0.6, size = 0.1)
    
  
    # saving metrics
    res <- data.frame(train = train_metrics,
               test = test_metrics) %>%
      rownames_to_column("metric")
    
    
    
    res$dataset <- dataset_name
    res$duration_sec <- run_time
    
    
    res_list <- list(
      res = res,
      model = base_rf
    )
    
    class(res_list) <- append(class(res_list), "model_results")
    return(res_list)
    
    
  }))

# saveRDS(rf_models, "rf_models.RDS")
rf_models <- readRDS("rf_models.RDS")

rf_models_metrics <- c(1:3) %>% 
  map_dfr(~ (rf_models[[.x]]$res))  %>%
  as_tibble() 

rf_models_metrics
  # select(-duration_sec) %>% 
  # group_by(metric)
  # mutate(relative_change_to_baseline = )

# beds and baths as factors
# A tibble: 9 x 5
# metric    train  test dataset           duration_sec
# <chr>     <dbl> <dbl> <chr>                    <dbl>
#   1 RMSE     0.142  0.314 Baseline Features        58.3 
# 2 Rsquared 0.968  0.837 Baseline Features        58.3 
# 3 MAE      0.0921 0.194 Baseline Features        58.3 
# 4 RMSE     0.127  0.320 All GIS                   5.97
# 5 Rsquared 0.975  0.830 All GIS                   5.97
# 6 MAE      0.0807 0.194 All GIS                   5.97
# 7 RMSE     0.122  0.317 All Inter + GIS           8.52
# 8 Rsquared 0.976  0.833 All Inter + GIS           8.52
# 9 MAE      0.0780 0.195 All Inter + GIS           8.52
# rf_models_bb_fact <- rf_models

# visualizing the results
c(1:3) %>% 
  map_dfr(~ (rf_models[[.x]]$res)) %>%
  filter(metric != "MAE") %>% 
  gather(key = "set", value = "statistic", c(test, train)) %>% 
  group_by(set, metric) %>% 
  mutate(mean_metric = mean(statistic)) %>% 
  # reorder some stuff for visualization
  ungroup() %>%
  mutate(set = factor(set, levels = c("train", "test")),
         dataset = factor(dataset, levels = c("Baseline Features", "All GIS", "All Inter + GIS"))) %>%
  
  ggplot(aes(x = dataset, y = statistic, group = set)) + 
  geom_point(aes(colour = set)) +
  geom_label(aes(label = round(statistic, 3), vjust = -0.5, hjust = 0.75)) + 
  stat_summary(fun.y = sum, geom = "line", aes(colour = set)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(size = 16)) +
  facet_wrap( ~ metric, scales = "free") +
  scale_y_continuous(    expand = expand_scale(mult = c(0.1, 0.2))) + 
  labs(title = "Dataset Selection - Performance Metrics for Random Forest", y = "", x = "Dataset") +
  scale_colour_brewer(palette = "Set1", name = "", labels = c("Validation", "Training")) 


 



# ---- OLS----- 

# split data into test and training for OLS, following same 80/20 scheme for RF
{
  set.seed(1111)
  trainIndex <- createDataPartition(y = zllw_na$last_sold_price_log, list = FALSE, p = 0.80)
  
  x <- zllw_na %>% select(-last_sold_price_log)
  y <- zllw_na$last_sold_price_log

  train_x  <- model.matrix.lm( ~ ., data = x[trainIndex, ], na.action = "na.pass")[, -1]
  test_x <- model.matrix.lm( ~ ., data = x[-trainIndex, ], na.action = "na.pass")[, -1]
  # sanity check
  dim(train_x); dim(test_x)
  
  train_y <- y[trainIndex]
  test_y <- y[-trainIndex]
  
  # sanity check
  length(train_y);length(test_y)
  
  
  ggplot() +
    geom_density(data = data.frame(train = train_y), aes(x = train, fill = "train"), alpha = 0.5) +
    geom_density(data = data.frame(test = test_y), aes(x = test, fill = "test"), alpha = 0.5) +
    scale_fill_brewer(palette = "Set1") 
  
  rm(x, y)
}
nrow(train_x)
nrow(test_x)


# we will useonly tax assessed value, as that had the highest correlation with sales price
zllw_ols <- lm(data = data.frame(y = train_y, x = train_x[, "tax_assess_value_log"]), y ~ .)
zllw_ols
# fitted and predicted values
ols_fit <- predict(zllw_ols, data.frame(x = train_x[, "tax_assess_value_log"]))
ols_pred <- predict(zllw_ols, data.frame(x = test_x[, "tax_assess_value_log"]))

Metrics::ape(exp(train_y), exp(ols_fit)) %>% median() * 100 # 35.5
Metrics::ape(exp(test_y), exp(ols_pred)) %>% median() * 100 # 35.3

ols_fit_metrics <- postResample(train_y, ols_fit)
ols_val_metrics <- postResample(test_y, ols_pred)

# visualizing fitted and predicted vs observed values
tibble(pred = ols_fit, 
       obs = train_y) %>%
  scttrCor(aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.4, size = 0.1)

tibble(pred = ols_pred, 
       obs = test_y) %>%
  scttrCor(aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.4, size = 0.1)

# ols metrics
ols_baseline <- data.frame(
  metric =c("RMSE", "R2", "MAE"),
  "train" = ols_fit_metrics, 
  "test" = ols_val_metrics,
  model = "OLS")
ols_baseline

# ---- XGBoost Parameter Search using Baseline + GIS Features ----

# ---- retain gis and NAs ----
zllw_model
x <- zllw_model%>% select(-last_sold_price_log)
y <- zllw_model$last_sold_price_log

{
  set.seed(1111)
  trainIndex <- createDataPartition(y = y, list = FALSE, p = 0.8)
  
  
  train_x  <- model.matrix.lm( ~ ., data = x[trainIndex, ], na.action = "na.pass")[, -1]
  test_x <- model.matrix.lm( ~ ., data = x[-trainIndex, ], na.action = "na.pass")[, -1]
  
  # sanity check
  print(nrow(train_x) == nrow(x[trainIndex,]))
  print(nrow(test_x) == nrow(x[-trainIndex,]))
  
  
  train_y <- y[trainIndex]
  test_y <- y[-trainIndex]
  
  # sanity check
  # print(paste(length(train_y), length(test_y)))
  rm(x, y)
  
  
  # checking distribution of data
  ggplot() +
    geom_density(data = data.frame(train = train_y), aes(x = train, fill = "train"), alpha = 0.5) +
    geom_density(data = data.frame(test = test_y), aes(x = test, fill = "test"), alpha = 0.5) +
    scale_fill_brewer(palette = "Set1")
}


# creating sparse matrix for xgboost
train_dmat <-xgb.DMatrix(train_x, label = train_y)
test_dmat <- xgb.DMatrix(test_x, label = test_y)



train_x
ncol(train_x)

# initialize parameters for caret
caret_ctrl <- trainControl(  method = "cv",
                             number = 5,
                             verboseIter = TRUE,
                             allowParallel = TRUE,
                             search = "grid"#,
                             # summaryFunction = maeSummary
                             )
set.seed(1993-11-22)
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

default_params <- list(objective = "reg:linear",
                       booster = "gbtree",
                       eta = 0.1, #default = 0.3
                       gamma = 0 ,
                       max_depth = 6, #default=6
                       min_child_weight = 1, #default=1
                       subsample = 1,
                       colsample_bytree = 1# default = 1
)

# registerDoParallel(cores = 4)

system.time(
  xgbcv <- xgb.cv( params = default_params, # for finding optimal number of rounds at default parameters
                   data = train_dmat,
                   nrounds = 999,
                   nfold = 5, 
                   showsd = T, 
                   stratified = T,
                   print_every_n = 40, 
                   early_stopping_rounds = 20, 
                   maximize = F,
                   
                   seed = (1993-11-22))
)

# 2mins 26s
# stopImplicitCluster() 

xgbcv$best_iteration   # 308

# Then we will identify the best hyperparameter values using
# 5 fold cross validation with the optimal number of rounds.


# old 
# xgb_grid = expand.grid( nrounds = xgbcv$best_iteration,
#                         eta = c(0.2, 0.1, 0.01),
#                         max_depth = c(4, 6, 8),
#                         gamma = 0,
#                         colsample_bytree = c(1),
#                         min_child_weight=c(1, 3, 5),
#                         subsample= 1
# )
# new, does poorly eta = 0.01
xgb_grid = expand.grid( nrounds = xgbcv$best_iteration,
                        eta = c(0.1, 0.05),
                        max_depth = c(4, 6, 8),
                        gamma = 0,
                        colsample_bytree = c(0.6, 0.8, 1),
                        min_child_weight=c(1, 3, 5),
                        subsample= 1
)
# FOR NTA, CDISTRICT, SELECT INTERS.
# eta = 0.01 performs very poorly relative to 0.05 and 0.1 across#
# min_child weight and max tree depth

# eta = c(0.1, 0.05, 0.01)
# colsample_bytree = c(1),
xgb_grid
set.seed(1993-11-22)

# registerDoParallel(cores = 4)

system.time(xgb_caret <- train(x = train_dmat,
                               y = train_y,
                               method ='xgbTree', 
                               trControl = caret_ctrl,
                               metric = "RMSE",
                               tuneGrid = xgb_grid)
)

# stopImplicitCluster() # took 80mins?
# saveRDS(xgb_caret, "xgb_gis_no_inter.RDS")
# xgb_caret <- readRDS("xgb_gis_no_inter.RDS")
plot(xgb_caret)
# the optimal parameters are..

xgb_caret$bestTune

xgb_caret$results[which.min(xgb_caret$results$MAE), ]


# search for optimal # of roundns again using new parameters

optimal_params <- list(
  objective = "reg:linear",
  booster = "gbtree",
  eta = 0.05, #default = 0.3
  gamma = 0 ,
  max_depth = 8, #default=6
  min_child_weight = 5, #default=1
  subsample = 1,
  colsample_bytree = 0.8# default = 1
)
# 
xgbcv <- xgb.cv( params = optimal_params,  # tuned parameters using optimal parameters
                 data = train_dmat,
                 nrounds = 999,
                 nfold = 5,
                 showsd = T,
                 stratified = T,
                 print_every_n = 40,
                 early_stopping_rounds = 20,
                 maximize = F,
                 seed = (1993-11-22))
# 
xgbcv$best_iteration # 282

# train the final model using the hyperparameters and optimal roundns

set.seed(1)
xgb_mod <- xgb.train(data = train_dmat,
                     params = optimal_params, 
                     nrounds = 282, #xgbcv$best_iteration , 
                     seed = (1993-11-22))


xgb_mod 
# saveRDS(xgb_mod, "final_xgb_model.RDS")
# fitted values for training

xgb_fit <- predict(xgb_mod, train_dmat)

# training metrics
xgb_fit_metrics <- postResample(xgb_fit, train_y) 

# visualizing fitted vs observed
tibble(pred = xgb_fit, obs = train_y) %>%
  scttrCor(aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.6, size = 0.1)

# predicted values for validation

xgb_pred <- predict(xgb_mod, test_dmat)

# validation metrics

xgb_val_metrics <- postResample(xgb_pred, test_y) 

xgb_val_metrics
# RMSE  Rsquared       MAE 
# 0.2970905 0.8564407 0.1960853 
# visualizing predicted vs observed values

tibble(pred = xgb_pred, obs = test_y) %>%
  scttrCor(aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.6, size = 0.1)

# median absolute percentage error
Metrics::ape(exp(test_y), exp(xgb_pred)) %>% median() * 100
# putting it together
xgb_metrics <- tibble(train = xgb_fit_metrics, 
           test = xgb_val_metrics,
           metric = c("RMSE", "R2", "MAE"),
           model = c("XGBoost"))

xgb_metrics
# ---- XGBoost Variable Importance ----
imp_mat <- xgb.importance(model = xgb_mod) %>% as_tibble()

imp_mat %>%
  .[1:20,] %>%
  ggplot(aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) + 
  geom_col(colour = "black", show.legend = FALSE) + 
  coord_flip() +
  scale_fill_viridis_c(begin = 0.1) +
  labs(title = "XGBoost Variable Importance - Baseline Features + GIS",
       x = "Variables") +
  theme(plot.title = element_text(size = 14))

# ---- final model comparison ---- 
all_metrics <- bind_rows(ols_baseline,
          rf_models_metrics %>% 
              filter(dataset == "All GIS") %>%
              select(metric:test) %>% 
              mutate(model = "RF",
                     metric = ifelse(metric == "Rsquared", "R2", metric)),
          xgb_metrics) %>%
  gather(key = "set", value = "statistic", c(test, train)) %>% 
  mutate(metric = factor(metric, levels = c("MAE", "RMSE", "R2"))) 

all_metrics 

(0.29709048/0.31839758) * 100 - 100 # 6.69% increase in accuracy for xgboost, relative to rf
(0.85644067/0.83216491) * 100 - 100 # 2.917% increase in R2
# relative performance to OLS
all_metrics %>% as_tibble() %>% 
  filter(metric != "MAE", set == "test") %>% 
  group_by(set, metric) %>%
  mutate(relative_performance = (statistic/statistic[model == "OLS"] * 100) - 100)  %>%
  ungroup() %>% 
  select(-c(set, statistic)) %>% 
  spread(model, relative_performance)
all_metrics
all_metrics %>% 
  filter(metric != "MAE") %>% 
  ggplot(aes(x = model, y = statistic, group = set)) + 
  geom_point(aes(colour = set)) +
  ggrepel::geom_label_repel(aes(label = round(statistic, 3))) + 
  stat_summary(fun.y = sum, geom = "line", aes(colour = set)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(size = 16)) +
  facet_wrap( ~ metric, scales = "free")  +
  scale_y_continuous(    expand = expand_scale(mult = c(0.1, 0.2)), limits = c(0, 1)) +
  
  scale_colour_brewer(palette = "Set1", name = "", labels = c("Validation", "Training")) +
  labs(title = "Model Performance Comparisons between Baseline OLS, Random Forest, and Tuned XGBoost",
       x = "Model", y = "Metric",
       subtitle = "Data: 12898 Properties Sold in NYC from Zillow, split into 80/20 training, validation set")





