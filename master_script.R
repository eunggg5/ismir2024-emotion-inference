# master_script.R

# Set working directory
setwd("/Users/user/workspace/ismir2024-emotion-inference")

# Run the setup first
source("setup.R")

# Run all sections sequentially
source("listeners_preparation.R")
# source("dynamic_LME_total.R")
# source("dynamic_LME_joy_happiness.R")
# source("dynamic_LME_sadness.R")
# source("dynamic_LME_anger.R")
# source("discrete_LME.R")
# source("discrete_cross_validation.R")

# Everything is now executed in sequence
print("All analyses completed.")