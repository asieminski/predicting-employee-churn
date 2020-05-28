library(tidyverse)
library(tidymodels)
library(themis)

set.seed(1)

load("Data/folds_and_recipes.RData")

gen_rec %>% 
  prep() %>% 
  juice() %>% 
  colnames() %>% 
  length() # there are 41 predictors

rand_for_mod <- rand_forest(trees = 41*10, #variables times 10
            mtry = tune(),
            min_n = tune()
            ) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rand_for_grid <- grid_regular(finalize(mtry(), train), 
             min_n(), 
             levels = 10)

create_rand_for_res <- function(recipe){

workflow() %>% 
    add_recipe(recipe) %>% 
    add_model(rand_for_mod) %>% 
    tune_grid(
      grid = rand_for_grid,
      resamples = train_folds,
      metrics = metric_set(roc_auc, j_index,
                           accuracy,
                           sensitivity, specificity),
      control = control_grid(save_pred = TRUE)
    )
}  

inspect_rand_for_res <- function(res){
  res %>% 
    collect_metrics() %>% 
    pivot_wider(id_cols = c("mtry", "min_n"), 
                names_from = ".metric", 
                values_from = "mean") %>% 
    ggplot(aes(x = j_index, y = roc_auc))+
    geom_point()
}

doParallel::registerDoParallel()

rand_for_res_5_to_1 <- create_rand_for_res(gen_rec)

inspect_rand_for_res(rand_for_res_5_to_1)
#approx 78.5% roc auc and  19% J index


rand_for_res_3_to_1 <- create_rand_for_res(upsamp_rec_3_to_1)

inspect_rand_for_res(rand_for_res_3_to_1)
#approx 76% roc_auc and 30% j index


rand_for_res_1_to_1 <- create_rand_for_res(upsamp_rec_1_to_1)

inspect_rand_for_res(rand_for_res_1_to_1)
#approx 78% roc_auc and 40% j index
<<<<<<< HEAD


# Upsampling in 1 to 1 proportion increases performance 
# for both tree-based and linear models; 
# we will only use that preprocessing technique onwards

best_rand_for_hypers <- rand_for_res_1_to_1 %>% 
  select_best("j_index")

final_rand_for_wf <- finalize_workflow(rand_for_wf, best_rand_for_hypers)

final_rand_for_res <- final_rand_for_wf %>% 
  fit_resamples(
    resamples = train_folds,
    control = control_grid(save_pred = TRUE)
  )

final_rand_for_preds <- final_rand_for_res %>% 
  collect_predictions()

save.image("Output/rand_for_output.RData")
=======
save.image("Output/rand_for_output.RData")

# Upsampling in 1 to 1 proportion increases performance 
# for both tree-based and linear models;
# we will only use that preprocessing technique onwards







>>>>>>> cad808dffc48ca560e9953dcb8eecf3ce7fc0642
