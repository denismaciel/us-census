Modeling
================
Denis Maciel
06/03/2019

``` r
library(tidyverse)
library(here)

col_string <- glue::glue("
|   91 distinct values for attribute #0 (age) continuous
|    9 distinct values for attribute #1 (class of worker) nominal
|   52 distinct values for attribute #2 (detailed industry recode) nominal
|   47 distinct values for attribute #3 (detailed occupation recode) nominal
|   17 distinct values for attribute #4 (education) nominal
| 1240 distinct values for attribute #5 (wage per hour) continuous
|    3 distinct values for attribute #6 (enroll in edu inst last wk) nominal
|    7 distinct values for attribute #7 (marital stat) nominal
|   24 distinct values for attribute #8 (major industry code) nominal
|   15 distinct values for attribute #9 (major occupation code) nominal
|    5 distinct values for attribute #10 (race) nominal
|   10 distinct values for attribute #11 (hispanic origin) nominal
|    2 distinct values for attribute #12 (sex) nominal
|    3 distinct values for attribute #13 (member of a labor union) nominal
|    6 distinct values for attribute #14 (reason for unemployment) nominal
|    8 distinct values for attribute #15 (full or part time employment stat) nominal
|  132 distinct values for attribute #16 (capital gains) continuous
|  113 distinct values for attribute #17 (capital losses) continuous
| 1478 distinct values for attribute #18 (dividends from stocks) continuous
|    6 distinct values for attribute #19 (tax filer stat) nominal
|    6 distinct values for attribute #20 (region of previous residence) nominal
|   51 distinct values for attribute #21 (state of previous residence) nominal
|   38 distinct values for attribute #22 (detailed household and family stat) nominal
|    8 distinct values for attribute #23 (detailed household summary in household) nominal
|    (instance weight)
|   10 distinct values for attribute #24 (migration code-change in msa) nominal
|    9 distinct values for attribute #25 (migration code-change in reg) nominal
|   10 distinct values for attribute #26 (migration code-move within reg) nominal
|    3 distinct values for attribute #27 (live in this house 1 year ago) nominal
|    4 distinct values for attribute #28 (migration prev res in sunbelt) nominal
|    7 distinct values for attribute #29 (num persons worked for employer) continuous
|    5 distinct values for attribute #30 (family members under 18) nominal
|   43 distinct values for attribute #31 (country of birth father) nominal
|   43 distinct values for attribute #32 (country of birth mother) nominal
|   43 distinct values for attribute #33 (country of birth self) nominal
|    5 distinct values for attribute #34 (citizenship) nominal
|    3 distinct values for attribute #35 (own business or self employed) nominal
|    3 distinct values for attribute #36 (fill inc questionnaire for veteran's admin) nominal
|    3 distinct values for attribute #37 (veterans benefits) nominal
|   53 distinct values for attribute #38 (weeks worked in year) continuous
|    2 distinct values for attribute #39 (year) nominal")

col_names <- col_string %>%
  str_split("\\n") %>% 
  .[[1]] %>% 
  str_extract("\\((.*?)\\)") %>% 
  str_remove_all("\\(|\\)|'") %>% 
  str_replace_all(" ", "_")

col_names <- c(col_names, "income")

us_census_raw <- readr::read_csv(here("us_census_full/census_income_learn.csv"), 
                                 col_names = col_names, 
                                 na = "?") 
us_census <- us_census_raw %>% 
  mutate(id = row_number(),
         income = ifelse(income == "- 50000.", "under 50k", "over 50k"))
```

Remove children from data set, since they are all low income.

``` r
us_census <- us_census %>% 
  filter(age > 16)
```

## A note on terminology and the methodology:

  - We will split the initial data set into two.
  - Train and test refer to the initial split. We won’t be looking at
    the test at all during the whole analysis.
  - In order to train the models, we will be using cross-validation,
    i.e. the train set will be divided into subsamples.
  - In the subsamples:
      - *Analysis set* is the data the model is trained on.
      - *Assessment set* is the data over which the model’s accuracy is
        measured.
  - The model specification that has the best performance will then be
    fitted again using the entirety of train and evaluated on test.
  - Finally, we will load in what we call `unseen_data`. In our context,
    this data set should be seen as when our model is deployed to
    production. We also assess our model on it.

<!-- end list -->

``` r
# # Use percentage of data to speed up training
# us_census <- us_census %>%
#   dplyr::sample_frac(0.15)

library(recipes)

initial_split <- rsample::initial_split(us_census)

train <- rsample::training(initial_split)
test <- rsample::testing(initial_split)
```

## Downsampling

`income`, our target variable, is very imbalanced. To account for that,
we will downsample the class `under 50k`, so that both classes end up
with the same amount of observations in the training set.

This is not a universally valid approach. The reader can rerun the model
without the downsampling set by setting the flag variable `DOWNSAMPLE`
below to `FALSE`.

``` r
DOWNSAMPLE <- TRUE
```

``` r
if (DOWNSAMPLE) {
  N_HIGH_INCOMES <- train %>% 
    filter(income == "over 50k") %>% 
    nrow()
  
  train <- train %>% 
    nest(-income) %>% 
    mutate(sampled_income = map(data, ~ sample_n(.x, N_HIGH_INCOMES))) %>% 
    select(income, sampled_income) %>% 
    unnest()
  
  assertthat::assert_that(N_HIGH_INCOMES * 2 == nrow(train), msg = "Unexpeted number of rows in upsample data set")
}
```

    ## [1] TRUE

Here we create our design matrix. We apply the steps discussed in the
“Exploratory Data Analysis part”. The good thing about creating such a
design matrix is that we can reapply it to bring new data (here: `test`
and `unseen_data`) into format required by our model.

Prepare the design matrix and create cross-validation dataframe.

``` r
library(recipes)
library(purrr)

rec <-  train %>% 
  select(-id,
         -year,
         -starts_with("migration_"), # too many missings
         -ends_with("_recode"),
         -instance_weight) %>% 
  recipe(income ~ .) %>% 
  step_modeimpute(all_nominal()) %>% 
  step_meanimpute(all_numeric()) %>% 
  step_other(all_predictors(), -all_numeric(), threshold = 0.10) %>% 
  step_discretize(own_business_or_self_employed, veterans_benefits) %>% 
  step_mutate(wage_per_hour = wage_per_hour + 0.1,
              capital_losses = capital_losses + 0.1, 
              dividends_from_stocks = dividends_from_stocks + 0.1, 
              capital_gains = dividends_from_stocks + 0.1) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_log(wage_per_hour, capital_losses, dividends_from_stocks, capital_gains)

df <- rsample::vfold_cv(train, v = 5)

# Prepper takes a split object and trains the model matrix on analysis set
df <- df %>% 
  # Prepare the model matrix: compute median, mode, transform into dummies...
  mutate(prepped = map(splits, ~ recipes::prepper(recipe = rec, .x))) %>% 
  mutate(
    train_processed = map2(splits, prepped, function(.x, .y) {
      analysis_set <- rsample::analysis(.x)
      processed <- bake(.y, new_data = analysis_set)
      return(processed)
    }),
    test_processed = map2(splits, prepped, function(.x, .y) {
      assessment_set <- rsample::assessment(.x)
      processed <- bake(.y, new_data = assessment_set)
      return(processed)
    }))
```

### Model

  - Specify two models: a boosted tree and a logistic regression.
  - Train the two models in each one of the analaysis set coming from
    each one of the 5 splits.

<!-- end list -->

``` r
library(parsnip)

mod_spec_xgboost <- boost_tree(mode = "classification", trees = 50) %>%
  set_engine("C5.0")

mod_spec_logit <- parsnip::logistic_reg(mode = "classification") %>%
  set_engine("glm")

models <- tribble(~model_type, ~model_spec,
                  "logit", mod_spec_logit,
                  "xgboost", mod_spec_xgboost)

df <- df %>% 
  crossing(models) %>% 
  mutate(model_trained = map2(train_processed, model_spec, function(.x, .y) {
    parsnip::fit(object = .y,
                 formula = income ~ .,
                 data = .x)
  }))
```

## Model evaluation

  - Compute predictions on the assessment sets.
  - Assess models with accuracy and area under the curve.

<!-- end list -->

``` r
predict_income_class <- function(test_data, model) {
  parsnip::predict_class(model, new_data = test_data)
}

predict_income_prob <- function(test_data, model) {
  parsnip::predict_classprob(model, new_data = test_data) %>% 
    pull(`under 50k`)
}

df <- df %>% 
  mutate(pred_class = map2(test_processed, model_trained, predict_income_class),
         pred_prob = map2(test_processed, model_trained, predict_income_prob),
         true = map(test_processed, "income"))

model_metrics <- df %>% 
  select(id, model_type, pred_class, pred_prob, true) %>% 
  unnest()
```

### Accuracy

  - If the datasets have been downsampled, accuracy more of a meaningful
    metric, because there is no imbalance. If not, a high accuracy can
    be achieved by a naive model, which predicts all respondents to be
    low income, due to the imbalance of the class.

<!-- end list -->

``` r
accuracy <- model_metrics %>% 
  mutate(is_correct = pred_class == true) %>% 
  group_by(id, model_type) %>% 
  summarise(pp_correct = sum(is_correct)/n()) 

# Nicer Formatting
accuracy %>% 
  spread(model_type, pp_correct) %>% 
  mutate(diff = logit - xgboost)
```

    ## # A tibble: 5 x 4
    ## # Groups:   id [5]
    ##   id    logit xgboost    diff
    ##   <chr> <dbl>   <dbl>   <dbl>
    ## 1 Fold1 0.818   0.840 -0.0220
    ## 2 Fold2 0.823   0.841 -0.0183
    ## 3 Fold3 0.824   0.836 -0.0115
    ## 4 Fold4 0.814   0.827 -0.0126
    ## 5 Fold5 0.810   0.834 -0.0234

### Area under the curve

  - AUC is a better metric because it accounts for eventual imablances
    in the dataset.

<!-- end list -->

``` r
auc <- model_metrics %>% 
  nest(-id, -model_type, .key = "pred_true") %>% 
  mutate(auc = map(pred_true, ~ yardstick::roc_auc(.x, true, pred_prob))) %>% 
  select(-pred_true) %>% 
  unnest() 

# Nicer formatting
auc %>% 
  spread(model_type, .estimate) %>% 
  mutate(diff = logit - xgboost)
```

    ## # A tibble: 5 x 6
    ##   id    .metric .estimator logit xgboost    diff
    ##   <chr> <chr>   <chr>      <dbl>   <dbl>   <dbl>
    ## 1 Fold1 roc_auc binary     0.904   0.918 -0.0143
    ## 2 Fold2 roc_auc binary     0.900   0.914 -0.0133
    ## 3 Fold3 roc_auc binary     0.901   0.912 -0.0109
    ## 4 Fold4 roc_auc binary     0.896   0.908 -0.0120
    ## 5 Fold5 roc_auc binary     0.898   0.911 -0.0125

### ROC curves

  - How do logit and boosted tree differ in their predictions?

<!-- end list -->

``` r
library(patchwork)

p <- model_metrics %>% 
  nest(- model_type) %>% 
  mutate(plot = map2(data, model_type,  function(.x, title) {
    .x %>% 
      group_by(id) %>% 
      yardstick::roc_curve(true, pred_prob) %>% 
      autoplot() +
      labs(subtitle = glue::glue("ROC for {title}"))
  })) %>% 
  pull(plot) %>% 
  reduce(`/`) # patchwork does its magic by reassinging the "/" operator for ggplot objects
p
```

![](modeling_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Best Model

  - Extract the best perfoming model from our collections of models.
  - Retrain the best model using on whole train dataset.
  - Assess model performance on test dataset.

<!-- end list -->

``` r
best_model_metric <- auc %>% 
  filter(.metric == "roc_auc") %>% 
  filter(.estimate == max(.estimate))

best_model <- df %>% 
  right_join(best_model_metric, by = c("id", "model_type")) %>% 
  pull(model_trained) %>% 
  .[[1]]

best_model_spec <- df %>% 
  right_join(best_model_metric, by = c("id", "model_type")) %>% 
  pull(model_spec) %>% 
  .[[1]]
```

``` r
prepped_rec <- recipes::prep(rec, data = train)
train_preprocessed <- recipes::juice(prepped_rec)

best_model_on_train <- best_model_spec %>% 
  fit(income ~ ., data = train_preprocessed)

test_processed <- bake(prepped_rec, new_data = test)

prediction <- predict_classprob(best_model_on_train, new_data = test_processed)

auc_test <- bind_cols(prediction, test_processed['income']) %>% 
  yardstick::roc_auc(income, `under 50k`)

glue::glue("
The best model has had an AUC of:
    * {scales::percent(best_model_metric$.estimate)} on cross-validation.
    * {scales::percent(auc_test$.estimate)} on test data.
")
```

    ## The best model has had an AUC of:
    ##     * 91.8% on cross-validation.
    ##     * 90.8% on test data.

## Unseen data

Now let’s test our model on the unseen data.

  - Read in data.
  - Take out all respondents under 16.
  - Predict on unseen
  - Add children back with prediction `under 50k`
  - Check area under the
curve

<!-- end list -->

``` r
unseen_data_raw <- readr::read_csv(here("us_census_full/census_income_test.csv"), 
                             col_names = col_names, 
                             na = "?") 

unseen_data <- unseen_data_raw %>% 
  mutate(id = row_number(),
         income = ifelse(income == "- 50000.", "under 50k", "over 50k"))

# Take children out
unseen_data_children <- unseen_data %>% 
  filter(age <= 16)

unseen_data <- unseen_data %>% filter(age > 16)
```

``` r
unseen_data_processed <- bake(prepped_rec, new_data = unseen_data)

prediction_unseen <- parsnip::predict_classprob(best_model_on_train,
                                                new_data = unseen_data_processed) %>% 
  bind_cols(unseen_data_processed['income'])

# Add prediction for children: all are low income
prediction_unseen_children <- unseen_data_children %>%
  transmute(income = factor(income, levels =  c("over 50k", "under 50k")),
            `over 50k` = 0, 
            `under 50k` = 1)

# Add children back
prediction_unseen_all <- bind_rows(prediction_unseen, prediction_unseen_children)

auc_unseen <- prediction_unseen_all %>% 
  yardstick::roc_auc(income, `under 50k`)

auc_unseen
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.937

## Further Exploration

  - Grid search for model tuning
  - Deeper dive into the features:
      - better feature selection
      - feature engineering

<!-- end list -->

``` r
knitr::knit_exit()
```
