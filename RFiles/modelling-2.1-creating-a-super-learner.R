library(tidymodels)
library(tidyverse)

load("Output/elastic_net_outputs.RData")
load("Output/rand_for_output.RData")
load("Output/rbf_output.RData")
load("Output/xgb_output.RData")

pred_tables <- str_detect(ls(), pattern = "final.*preds", negate = T)
model_wfs <- str_detect(ls(), pattern = "final.*wf", negate = T)

rm(list = ls()[pred_tables & model_wfs])

data_split <- readRDS("Data/data_split.RDS")

### Creating a training set 

table_of_predictions <- tibble(elas_net = final_elas_net_preds$.pred_Yes,
       rand_for = final_rand_for_preds$.pred_Yes,
       rbf = final_rbf_preds$.pred_Yes,
       xgb = final_xgb_preds$.pred_Yes,
       Attrition = final_elas_net_preds$Attrition)

### Creating a testing 

predicting_on_test <- tibble(
  wf = list(
    final_elas_net_wf,
    final_rand_for_wf,
    final_rbf_wf,
    final_xgb_wf
  ),
  data_split = list(data_split),
  wf_id = c("elas_net", "rand_for", "rbf", "xgb")
)

predicting_fitted <- predicting_on_test %>% 
  mutate(last_fit = map2(.x = wf, .y = data_split,
                         .f = last_fit)) 


list_of_pred_tibbles <- lapply(predicting_fitted$last_fit, FUN = pluck, ".predictions")

final_preds_on_test <- predicting_fitted %>% 
  mutate(list_of_pred_tibbles = list_of_pred_tibbles) %>%
  unnest(list_of_pred_tibbles) %>% 
  unnest(list_of_pred_tibbles) %>% 
  pivot_wider(id_cols = c(".row", "Attrition"),
                          values_from = .pred_Yes, 
                          names_from = wf_id)

### Training a super learner

sup_glm_mod <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

sup_glm_rec <- recipe(Attrition ~ elas_net + rand_for + rbf + xgb, 
                      data = table_of_predictions)


sup_glm_wf <- workflow() %>% 
  add_model(sup_glm_mod) %>% 
  add_recipe(sup_glm_rec)

sup_glm_fitted <- fit(sup_glm_wf, data = table_of_predictions)

### Checking performance

vec_class <- predict(sup_glm_fitted, 
        new_data = final_preds_on_test) %>% 
  pluck(1)

dv <- final_preds_on_test$Attrition  

j_index_vec(dv, vec_class)  

vec_prob <- predict(sup_glm_fitted, 
                     new_data = final_preds_on_test,
                    type = "prob") %>% 
  pluck(2)

roc_auc_vec(dv, vec_prob)

table(dv, vec_class)
  
  
  
