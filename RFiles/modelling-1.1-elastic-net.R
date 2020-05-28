library(tidyverse)
library(tidymodels)
library(themis)

set.seed(1)

load("Data/folds_and_recipes.RData")

#adding interactions to the formula
add_interactions <- function(recipe){
  step_interact(recipe,
                terms = ~ Age:TrainingTimesLastYear + Age:JobSatisfaction +
                                 MaritalStatus_Married:BusinessTravel + 
                                 Education:JobLevel) }

elas_net_mod <- logistic_reg(
  mixture = tune(),#0 is ridge (square beta, works best when most variables are useful) 1 is lasso (abs beta)
  penalty = tune(), #lambda, penalty size
) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")


elas_net_grid <- grid_regular(mixture(), 
                              penalty(), 
                              levels = 10)

###

doParallel::registerDoParallel()

create_elas_net_res <- function(recipe){

workflow() %>%
add_model(elas_net_mod) %>% 
add_recipe(add_interactions(recipe)) %>% 
  tune_grid(resamples = train_folds,
            grid = elas_net_grid,
            metrics = metric_set(roc_auc, j_index, sensitivity, specificity)
  )

}  

elas_net_res_5_to_1 <- create_elas_net_res(recipe = gen_rec)

inspect_elas_net_res <- function(res){
res %>% 
  collect_metrics() %>% 
  pivot_wider(id_cols = c("penalty", "mixture"), 
              names_from = ".metric", 
              values_from = "mean") %>% 
  arrange(-j_index) %>% 
  arrange(-roc_auc) %>% 
  top_n(20) %>% 
  ggplot(aes(x = j_index, y = roc_auc, label1 = mixture,
         label2 = penalty))+
  geom_point()
  }

inspect_elas_net_res(elas_net_res_5_to_1)

# approx. 81% roc auc and 33% j_index 

elas_net_res_3_to_1 <- create_elas_net_res(recipe = upsamp_rec_3_to_1) 
inspect_elas_net_res(elas_net_res_3_to_1)

#approx 80% roc and 45% j_index

elas_net_res_1_to_1 <- create_elas_net_res(recipe = upsamp_rec_1_to_1)
inspect_elas_net_res(elas_net_res_1_to_1)

#80-81% roc and 48-50% j_index

best_elas_net_hypers <- select_best(elas_net_res_1_to_1,metric = "j_index")



elas_net_wf <- workflow() %>% 
  add_model(elas_net_mod) %>% 
  add_recipe(upsamp_rec_1_to_1)

final_elas_net_wf <- finalize_workflow(elas_net_wf, best_elas_net_hypers)

final_elas_net_res <- final_elas_net_wf %>% 
  fit_resamples(
    resamples = train_folds,
    control = control_grid(save_pred = TRUE)
  )

final_elas_net_preds <- final_elas_net_res %>% 
  collect_predictions()

save.image("Output/elastic_net_outputs.RData")




