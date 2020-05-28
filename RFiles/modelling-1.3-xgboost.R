library(tidyverse)
library(tidymodels)
library(themis)

set.seed(1)
load("Data/folds_and_recipes.RData")

# xgboost features many hyperparameters
# and is much more computationally expensive than the previous two models

# for these reasons we will use Bayesian grid search instead of using a grid of a wide range of possible values for every hyperparameter

# tidymodels a low-level ml framework, so building an xgboost in steps would take ages

xgb_mod <- boost_tree(
  trees = 1000,
  # relatively small dataset
  mtry = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(upsamp_rec_1_to_1)

# difficult to test all hyperparameter combinations
#instead we'll use different parameter combinations based on a hypercube

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 40
)

doParallel::registerDoParallel()

xgb_res <- xgb_wf %>%
  tune_grid(
    resamples = train_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, j_index,
                         accuracy,
                         sensitivity, specificity)
  )


xgb_res %>% 
  collect_metrics() %>% 
  pivot_wider(id_cols = 1:6,names_from = ".metric", values_from = "mean") %>% 
  ggplot(aes(x = j_index, y = roc_auc))+
  geom_point()

# nae bad -- 50% J index and 80% roc auc

best_xgb_hypers <- xgb_res %>% 
  select_best("j_index")



final_xgb_wf <- finalize_workflow(xgb_wf, 
                                  parameters = best_xgb_hypers)

final_xgb_res <- final_xgb_wf %>% 
  fit_resamples(
    resamples = train_folds,
    control = control_grid(save_pred = TRUE)
  )

final_xgb_preds <- final_xgb_res %>% 
  collect_predictions()

save.image("Output/xgb_output.RData")






