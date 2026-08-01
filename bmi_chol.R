library(tidyverse)
library(tidymodels)
library(NHANE)
library(janitor)

#Data Reading/Data Wrangling
#Select for BMI, TotChol (total cholesterol), BPSysAve (average systolic blood pressure), Height and Weight
nhanes_data <- select(NHANES, BMI, TotChol, BPSysAve, Height, Weight)
#Filter for non NA values
nhanes_data <- filter(nhanes_data, !is.na(BMI), !is.na(TotChol), !is.na(BPSysAve), !is.na(Height), !is.na(Weight))
#Clean the column names
nhanes_data <- nhanes_data |>
  clean_names()

#Visualizing BMI Distribution
bmi_dist <- ggplot(nhanes_data, aes(bmi)) + 
  geom_histogram(binwidth = 3) + 
  labs(x = "Body Mass Index (BMI)", title = "Population BMI Distribution", caption = "NHANE data") + 
  theme(text = element_text(size = 15))

#Visualizing Total Cholesterol Distribution
tot_chol_dist <- ggplot(nhanes_data, aes(tot_chol)) + 
  geom_histogram(binwidth = 0.5) + 
  labs(x = "Total Cholesterol", title = "Population Total Cholesterol Distribution", caption = "NHANE data") + 
  theme(text = element_text(size = 15))

#Mean, Median and Standard Deviation of Population Total Cholesterol Distribution
tot_chol_stat <- nhanes_data |>
  summarize(mean_cholesterol = mean(tot_chol),
            median_cholesterol = median(tot_chol),
            sd_cholesterol = sd(tot_chol))

#Generate Random Sample of 100 Observations 
sample_1 <- nhanes_data |>
  rep_sample_n(100)

#Sample Distribution Visualization
sample_1_dist <- ggplot(sample_1, aes(tot_chol)) + 
  geom_histogram(binwidth = 0.3) + 
  labs(x = "Total Cholesterol", title = "Sample Total Cholesterol Distribution (n = 100)", caption = "NHANE data") + 
  theme(text = element_text(size = 10))

#Generate 1000 samples holding 100 observations
samples <- rep_sample_n(nhanes_data, size = 100, reps = 1000)

#Get the average total cholesterol and average BMI of all samples grouped by sample
avg_chol_bmi <- samples |>
  group_by(replicate) |>
  summarize(mean_tot_cholesterol = mean(tot_chol),
            mean_bmi = mean(bmi)) 

#Sampling Distribution (n = 100) of total cholesterol and BMI
tot_chol_dist100 <- ggplot(avg_chol_bmi, aes(mean_tot_cholesterol)) + 
  geom_histogram(binwidth = 0.05) + 
  labs(x = "Total Cholesterol", title = "Sampling Distribution of Total Cholesterol (n = 100)")

tot_bmi_dist100 <- ggplot(avg_chol_bmi, aes(mean_bmi)) + 
  geom_histogram(binwidth = 0.5) + 
  labs(x = "Body Mass Index (BMI)", title = "Sampling Distribution of BMI (n = 100)")





