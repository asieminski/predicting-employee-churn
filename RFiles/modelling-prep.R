
library(rsample)
library(recipes)
library(themis)

set.seed(1)

train <- readRDS("Data/train.RDS")

train_folds <- vfold_cv(train, v = 5, repeats = 1)

# There is a class imbalance and little data 
# We might need to use subsampling techniques from the themis package
# All synthetic oversampling techniques work only for categorical data

#The models built will have the following requirements:
#Elastic Net:CS, NZV
#Random Forest:None
#XGboost:None
#SVM RBF:CS



unord_preds <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus", "OverTime", "IncomeGroup")

ord_preds <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance", "BusinessTravel")

# ratio is higher than five to one
summary(train$Attrition)

colnames(train)

gen_rec <- recipe(Attrition ~ ., data = train) %>%
  step_rm("EmployeeNumber", 
          #Id is irrelevant to modelling 
          "HourlyRate", "DailyRate", 
          #Same information as MonthlyRate
          "TotalWorkingYears" 
          #high collinearity
          ) %>% 
  step_ordinalscore("Education", "EnvironmentSatisfaction", 
                    "JobInvolvement", "JobLevel", "JobSatisfaction", 
                    "PerformanceRating", "RelationshipSatisfaction", 
                    "StockOptionLevel", "WorkLifeBalance", "BusinessTravel") %>% 
  #converts ordinal variables to integers
  step_normalize(all_numeric()) %>% 
  #centres and scales
  step_pca("YearsAtCompany", 
           "YearsInCurrentRole", 
           "YearsWithCurrManager",
           #collinear variables
           prefix = "CombinedTenureVar") %>% 
  step_dummy("Department", "EducationField", 
             "Gender", "JobRole", 
             "MaritalStatus", "OverTime")
  #creates dummy variables
  
upsamp_rec_1_to_1 <- gen_rec %>% 
  step_upsample("Attrition", over_ratio = 1)

upsamp_rec_3_to_1 <- gen_rec %>% 
  step_upsample("Attrition", over_ratio = 0.5)


rm(list = c("ord_preds", "unord_preds"))

save.image("Data/folds_and_recipes.RData")

