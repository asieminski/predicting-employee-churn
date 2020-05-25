library(rsample)
library(tidyverse)
library(ggridges)
library(ggalluvial)
library(ggcorrplot)

data_tidied <- readRDS("Data/data_tidied.RDS")

set.seed(1)
# splitting data so that we can only explore the training set
data_split <- initial_split(data_tidied, prop = 0.75, strata = "Attrition")

train <- training(data_split)
test <- testing(data_split)

#save data splits
saveRDS(train, "Data/train.RDS")
saveRDS(test, "Data/test.RDS")
saveRDS(data_split, "Data/data_split.RDS")

train %>% 
  pivot_longer(c("MonthlyIncome", "MonthlyRate"),
               names_to = "IncomeVariables", 
               values_to = "Value") %>% 
  ggplot(aes(y = IncomeVariables, x = Value))+
  ggridges::geom_density_ridges()+
  geom_boxplot(alpha= 0.4)


train %>% 
  select_if(is.ordered) %>% 
  select(-BusinessTravel, 
         -StockOptionLevel, 
         -JobLevel, 
         -Education,
         -PerformanceRating) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(y = name, fill = value))+
  geom_bar(position = "stack")+
  labs(title = "Distribution of survey responses")+
  ylab("Constructs")+
  xlab("")


train %>% 
  pivot_longer(contains("years"),
               names_to = "YearVariables", 
               values_to = "Value") %>% 
  ggplot(aes(y = YearVariables, x = Value))+
  ggridges::geom_density_ridges()



train %>% 
  count(Gender, 
        MaritalStatus, 
        JobLevel, 
        Attrition) %>% 
  ggplot(aes(axis1 = JobLevel, 
             axis2 = Gender, 
             axis3 = MaritalStatus,
             y = n)) +
  scale_x_discrete(limits = c("JobLevel", "Gender", "MaritalStatus"), 
                   expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Attrition)) +
  geom_stratum() + 
  geom_text(stat = "stratum", infer.label = TRUE) +
  theme_minimal() +
  ggtitle("Employees at the company",
          "stratified by demographics and attrition")



train %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE, 
             hc.method = "complete",
             method = "square")

train %>% 
  select_if(is.ordered) %>% 
  select(-BusinessTravel, 
         -StockOptionLevel, 
         -JobLevel, 
         - Education,
         -PerformanceRating) %>% 
  mutate_all(as.numeric) %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE, 
             hc.method = "complete",
             method = "square")
# uncorrelated responses on these topics are odd
# maybe the survey questions were unreliable and/or invalid ways to measure these constructs?


train %>% 
  group_by(JobLevel) %>% 
  summarise(MedianMonthlyRate = median(MonthlyRate)) %>% 
  ggplot(aes(x = JobLevel, y = MedianMonthlyRate))+
  geom_col()

#These job levels make no sense
#we would need to ask why do they not correlate with income








