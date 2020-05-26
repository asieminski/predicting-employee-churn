library(tidyverse)
library(tidymodels)
library(themis)

set.seed(1)
load("Data/folds_and_recipes.RData")

# xgboost features many hyperparameters 
# and is much more computationally expensive than the previous two models

# for these reasons we will use Bayesian grid search instead of using a grid of a wide range of possible values for every hyperparameter

# tidymodels a low-level ml framework, so building an xgboost in steps would take ages





