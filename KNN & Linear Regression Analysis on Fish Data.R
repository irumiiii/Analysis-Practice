#Libraries
library(tidyverse)
library(tidymodels)

#QSAR study - Dataset predicts acute aquatic toxicity toward a fish species from 6 computated molecular descriptors
#We are interested in predicting LC50, the concentration of the compound that is lethal to 50% of the fish population
#expressed on a log scale (mol/L). Higher LC50 = less toxic, lower LC50 = more toxic
fish_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00504/qsar_fish_toxicity.csv"
fish <- read.csv(fish_url, sep = ";", header = FALSE)
colnames(fish) <- c("CIC0","SM1_Dz","GATS1i","NdsCH","NdssC","MLOGP","LC50")

#CICO - Information index (describes structural complexity of molecule)
#SM1_Dz - 2D matrix based descriptor of atomic properties
#GATS1i - relates to ionized potential distribution across molecule
#NdsCH - count of specific carbon atom type/bonding pattern
#NdssC - count of another specific carbon atom type/bonding pattern
#MLOGP - Molecular lipophilicity (how fat soluble vs water soluble the molecul is)

#Visualizing MLOGP vs LC50 
ggplot(fish, aes(x = MLOGP, y = LC50)) + 
  geom_point() + 
  labs(x = "Molecular Lipophilicity", y = "Lethal Compound", title = "Molecular Lipophilicity vs LC50")
#We see a positive correlation of MLOGP vs LC50, meaning low MLOGP is MORE toxic to fish

#I will try to predict LC50 using MLOGP using both KNN and linear regression
#KNN Regression
#Split data into training and testing sets
fish_split <- initial_split(fish, prop = 0.75, strata = "LC50")
fish_train <- training(fish_split) 
fish_test <- testing(fish_split)

#Preprocessing data 
fish_recipe_knn <- recipe(LC50 ~ MLOGP, data = fish_train) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())

#Finding best K
#Set Model Specification
fish_model_knn <- nearest_neighbor(weight = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")

#Perform 10 fold cross-validation
fish_vfold_10 <- vfold_cv(fish_train, v = 10, strata = LC50)

#Set candidate K values
k_vals <- tibble(neighbors = seq(from = 1, to = 81, by = 10))

#Workflow KNN Regression
knn_workflow <- workflow() |>
  add_recipe(fish_recipe_knn) |>
  add_model(fish_model_knn)

#Performing cross-validation
fish_results_knn <- knn_workflow |>
  tune_grid(resamples = fish_vfold_10, grid = k_vals) |>
  collect_metrics()

#Finding K with smallest RMSE
smallest_RMSE <- fish_results_knn |>
  filter(.metric == "rmse") |>
  slice_min(mean, n = 1)

#Best K narrowed to 71, so try K values from 61~81
new_k_vals <- tibble(neighbors = seq(from = 61, to = 81, by = 2)) 
fish_newresults_knn <- knn_workflow |>
  tune_grid(resamples = fish_vfold_10, grid = new_k_vals) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  slice_min(mean, n = 1)
#still K = 71

#New KNN model specification using K = 71
fish_model_71 <- nearest_neighbor(weight = "rectangular", neighbors = 71) |>
  set_engine("kknn") |>
  set_mode("regression")

#New workflow
fish_workflow <- workflow() |>
  add_recipe(fish_recipe_knn) |>
  add_model(fish_model_71) |>
  fit(data = fish_train)

#Predict Testing Data
