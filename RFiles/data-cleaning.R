library(tidyverse)

#Importing data
data_raw <- read_csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

ord_preds <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance", "BusinessTravel")

#All character variables are actually ordinal and nominal variables, so I will convert them appropriately:
data_var_type_changed <- data_raw %>% 
  mutate_if(.predicate = is.character, .funs = as.factor) %>% 
  mutate_at(.vars = "BusinessTravel", .funs = factor, 
            ordered = T, 
            levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently")) %>% 
  #these variables are likely survey responses and should be ordinal instead of numeric:
  mutate_at(.vars = ord_preds, .funs = as.ordered)

skimr::skim(data_var_type_changed)
#notice irrelevant variables and class imbalance


#irrelevant variables removed
data_tidied <- data_var_type_changed %>% 
  select(-Over18, -EmployeeCount, -StandardHours)


saveRDS(data_tidied, "Data/data_tidied.RDS")
