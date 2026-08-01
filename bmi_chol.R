library(tidyverse)
library(tidymodels)
library(NHANES)
library(janitor)
library(cowplot)

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

#Bootstrapping Practice (total cholesterol)
#First, generate a single sample from the population (n = 500)
tot_chol_sample <- nhanes_data |>
  rep_sample_n(500) |>
  ungroup() |>
  select(tot_chol)

#Bootstrap from that  (lets try 10, 100, 1000 bootstrap samples of size 50)
boot_10 <- tot_chol_sample |>
  rep_sample_n(size = 500, replace = TRUE, reps = 10)
boot_100 <- tot_chol_sample |>
  rep_sample_n(size = 500, replace = TRUE, reps = 100) 
boot_1000 <- tot_chol_sample |>
  rep_sample_n(size = 500, replace = TRUE, reps = 1000)

#Calculating Mean Total Cholesterol of each bootstrap sample
boot_10_means <- boot_10 |>
  group_by(replicate) |>
  summarize(mean_cholsterol = mean(tot_chol))
boot_100_means <- boot_100 |>
  group_by(replicate) |>
  summarize(mean_cholesterol = mean(tot_chol)) 
boot_1000_means <- boot_1000 |>
  group_by(replicate) |>
  summarize(mean_cholesterol = mean(tot_chol))

#Histogram Visualization
boot_10_dist <- ggplot(boot_10_means, aes(mean_cholsterol)) + 
  geom_histogram(binwidth = 0.03) + 
  labs(x = "Total Cholesterol", title = "Point Estimates (mean) of Boostrap Sample (n = 10)", caption = "NHANES data") + 
  theme(text = element_text(size = 10)) 

boot_100_dist <- ggplot(boot_100_means, aes(mean_cholesterol)) + 
  geom_histogram(binwidth = 0.03) + 
  labs(x = "Total Cholesterol", title = "Point Estimates (mean) of Boostrap Sample (n = 100)", caption = "NHANES data") + 
  theme(text = element_text(size = 10))

boot_1000_dist <- ggplot(boot_1000_means, aes(mean_cholesterol)) + 
  geom_histogram(binwidth = 0.03) + 
  labs(x = "Total Cholesterol", title = "Point Estimates (mean) of Boostrap Sample (n = 1000)", caption = "NHANES data") + 
  theme(text = element_text(size = 10))

plots <- plot_grid(boot_10_dist,
                    boot_100_dist,
                    boot_1000_dist,
                    nrow = 3)

#Getting bootstrap confidence interval (95%), (n = 1000)
boot_1000_means |>
  select(mean_cholesterol) |>
  pull() |>
  quantile(c(0.025, 0.975))
#With 95% confidence, the true population mean for total cholesterol sits between 4.8608~5.0611

#True population mean is 4.90. Indeed, it sits within the confidence levels
nhanes_data |>
  summarize(mean = mean(tot_chol))



