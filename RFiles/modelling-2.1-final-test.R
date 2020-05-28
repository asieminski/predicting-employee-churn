library(tidymodels)
library(tidyverse)

#glmnet's accuracy was almost the same as accuracy of

load("Output/elastic_net_outputs.RData")

data_split <- readRDS("Data/data_split.RDS")

final_fit <- last_fit(final_elas_net_wf, 
         split = data_split, 
         metrics = metric_set(sensitivity,
                              specificity,
                              accuracy,
                              j_index,
                              roc_auc))
final_model_fitted <- fit(final_elas_net_wf, data = train)
vip(final_model_fitted)

coefficients <- final_fit %>% 
pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  tidy() %>% 
  filter(lambda > 0.073 & lambda < 0.08) %>% #take the closest lambda to our penalty param = 0.07742637
  arrange(-estimate)

coefficients %>% 
  mutate(absolute_beta = abs(estimate),
         term = reorder(factor(term), absolute_beta)) %>% 
  filter(term != "(Intercept)") %>% 
ggplot(aes(y = term, x = absolute_beta, fill = estimate < 0 ))+
  geom_col()


  
	