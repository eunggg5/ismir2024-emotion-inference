# master_script.R

# clear environment
rm(list = ls())

# Set working directory
setwd("/Users/user/workspace/ismir2024-emotion-inference") # change the path

# Run the setup first
source("setup.R")

# Run all sections sequentially
source("data_preparation.R")
source("discrete_LME.R")
source("discrete_cross_validation.R")
source("dynamic_LME_total.R")
source("dynamic_LME_joy_happiness.R")
source("dynamic_LME_sadness.R")
source("dynamic_LME_anger.R")

# Everything is now executed in sequence
print("All analyses completed.")
