library(tidyverse)
library(tidymodels)
library(themis)

set.seed(1)
load("Data/folds_and_recipes.RData")


rbf_mod <- svm_rbf(cost = tune(),
                   rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

rbf_wf <- workflow() %>% 
  add_model(rbf_mod) %>% 
  add_recipe(upsamp_rec_1_to_1)

doParallel::registerDoParallel()

rbf_res <- rbf_wf %>%
  tune_bayes(
    resamples = train_folds,
    
    initial = 5,
    iter = 50,
    # How to measure performance?
    metrics = metric_set(roc_auc, j_index),
    control = control_bayes(no_improve = 30, verbose = TRUE)
  )

rbf_res %>% 
  collect_metrics() %>% 
  pivot_wider(id_cols = 1:2,
              names_from = .metric,
              values_from = mean) %>% 
  ggplot(aes(j_index, roc_auc))+
  geom_point()

# J 48% roc auc approx 81%

best_rbf_hypers <- rbf_res %>% 
  select_best("j_index")

save.image("Output/rbf_output.RData")

