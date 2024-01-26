---
title: H2O & R
author: ''
date: '2024-01-25'
slug: h2o-r
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-25T17:55:19-03:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---


## Introduction

This post provides a concise guide to applying H2O to the Bank Telemarketing dataset, familiar from our previous discussion. Our goal is to demonstrate how H2O's tools can be effectively used for predictive modeling. Remember to install Java for H2O to function correctly. Note that closing your R session will also end the H2O session.


### Libraries and Data Set Up

Load necessary libraries and the Bank Telemarketing dataset from the UCI repository. We remove the 'duration' feature to prevent look-ahead bias in our predictions and convert character variables to factors.


```r
library(tidyverse)         
library(rsample)
library(h2o)                
```


```r
# Load the data
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip"
download.file(data_url, destfile = "bank.zip")
unzip("bank.zip", files = "bank.csv", exdir = "data")
bank_data <- read_csv2("data/bank.csv")
# Drop the duration feature as to avoid look ahead bias in our predictions
bank_data <- bank_data %>% mutate(across(where(is.character),as.factor))%>% 
    select(-duration)
```

### Data Preparation
Split the dataset into training and test sets, ensuring a stratified split based on the target variable 'y' for balanced representation.


```r
set.seed(123)
data_split <- initial_split(bank_data, prop = 0.75, strata = y)
train_data <- training(data_split)
test_data <- testing(data_split)
```

### H2O Initialization

Initialize an H2O instance, optimizing its performance by using all available cores and setting the maximum memory. Adjust the memory setting based on your dataset size and system capabilities. For best performance, the allocated memory should be 4x the size of your data, but never more than the total amount of memory on your computer.


```r
# -1 uses all cores 
h2o.init(nthreads = -1, max_mem_size = "16g")
```

```r
# Check you cluster setting, make sure the choice of cores and memory size is correct
h2o.clusterInfo()
```





```r
# Clear out anything old
h2o.removeAll()
```





Transfer the training and test datasets from R to the H2O environment. Use `h2o.ls()` to verify the its done.


```r
bank_train <- as.h2o(train_data)
bank_test <- as.h2o(test_data)
```

You should see two objects corresponding to the train and test sets.


```r
h2o.ls()
```

```
##                     key
## 1  test_data_sid_85e5_3
## 2 train_data_sid_85e5_1
```
### Model Setup

Define the target and predictor variables for model building. Here we demonstrate training a Random Forest model with 1,000 trees, using 10-fold cross-validation and stratified sampling for robust model evaluation.


```r
y <- "y"
x <- setdiff(names(bank_data), y)
```



```r
rf_model <- h2o.randomForest(y = y,
                           x = x,
                           training_frame = bank_train,
                           model_id = "rf_model",
                           nfolds = 10,
                           fold_assignment = "Stratified",
                           seed = 123,
                           max_runtime_secs = 240,
                           ntrees = 1000,
                           mtries = 4, #default value = sqrt(nºpredictors)
                           sample_rate = 0.632, #sample size per tree
                           min_rows = 1, #Minimum observations in a terminal node
                           max_depth = 20 #Maximum tree depth
                           )
```

View summaries of the Random Forest model, including key performance metrics like AUC (73.19%).


```r
summary(rf_model)
```

```r
h2o.performance(rf_model, 
                xval = TRUE)
h2o.confusionMatrix( rf_model, xval = TRUE)

h2o.gainsLift(rf_model, xval = TRUE)
```

Create a graph of variable importance.


```r
h2o.varimp_plot(rf_model)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

Evaluate the model's performance on the test data to assess out-sample metrics.







### Tuning the Model

In the initial model, default tuning parameters were used. Now, explore different combinations of 'mtries' and other parameters to optimize model performance.



```r
rf_params <- list(mtries = c(3, 4, 7, 10), 
                  ntrees = 1000,
                  sample_rate = 0.632, #sample size per tree
                  min_rows = 1, 
                  max_depth = 20)

rf_mod_tune <- h2o.grid(
  "randomForest",
  y = y,
  x = x,
  training_frame = bank_train,
  grid_id = "rf_tuning",
  nfolds = 10, 
  fold_assignment = "Stratified",
  seed = 123,
  max_runtime_secs = 60,
  hyper_params = rf_params
  )
```

Next, we sort the results by AUC and examine the results from best model. Not sure why the default combination (mtry=4) is slightly different our previous rf_model.


```r
rf_perf <- h2o.getGrid(
  grid_id = "rf_tuning",
  sort_by = "auc"
)

best_model <- rf_perf@summary_table %>% 
    as.tibble() %>%
    mutate(id=row_number()) %>% 
    arrange(desc(auc)) %>% 
    slice(1)%>% 
    pull(id)
```

Use `h2o.getModel()` to choose the best model


```r
best_rf <- h2o.getModel(rf_perf@model_ids[[best_model]])
```



Evaluate the best model on the test data.


```r
h2o.performance(model = best_rf,
                newdata = bank_test)
```

### Auto ML Function
Utilize H2O's AutoML for automated model selection and training, specifying constraints like the number of models and runtime.



```r
aml <- h2o.automl(
  y = y,
  x = x,
  training_frame = bank_train,
  nfolds = 10, 
  seed = 123,
  max_runtime_secs_per_model = 60,
  max_models = 10
)
```

Take a look at the leaderboard


```r
print(aml@leaderboard, 
      n = 10)
```

Extract the best model


```r
best_mod <- as.vector(aml@leaderboard$model_id)[1]
```

Save this model to use later. AUC 75.19%


```r
best_auto <- h2o.getModel(best_mod)
```


Variable Importance plot.


```r
h2o.varimp_plot(best_auto)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-33-1.png" width="672" />

Asses the performance on the test data. AUC 75.93%


```r
auto_pred <- h2o.performance(best_auto,
                         newdata = bank_test)
```



### Closing the H2O Cluster

Finally, shut down the H2O cluster to free up resources.


```r
h2o.shutdown()
```
