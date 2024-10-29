################################################################################################################
# setup.R: Load libraries, set working directory, and import data
################################################################################################################

# R version: 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.6.1

# ggplot2 version: 3.4.2
# scico version: 1.5.0
# lme4 version: 1.1.33
# lmerTest version: 3.1.3
# psych version: 2.3.6
# car version: 3.1.2
# caret version: 6.0.94
# sjPlot version: 2.8.15
# effects version: 4.2.2

################################################################################################################
# Load necessary libraries
library(ggplot2)
library(scico)
library(lme4)
library(lmerTest)
library(psych)
library(car)
library(caret)
library(sjPlot)
library(effects)

################################################################################################################
# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-oheunji97@gmail.com/내 드라이브/Document/2KAIST/2024/conference/ISMIR")

################################################################################################################
# Import composers' (targets) and listeners' (observers) data

# Targets (composers) dataframe
df_t <- read.csv("target_rate_2Hz.csv")

# Listeners (observers) dataframe
df_o <- read.csv("observer_rate_2Hz.csv")

# Remove unnecessary data points from df_t
df_t2 <- df_t[-which(df_t$id == "eat01" & df_t$emotion == "sad"),]
df_t2 <- df_t2[-which(df_t$id == "eat14" & df_t$emotion == "joy"),]
df_t2 <- df_t2[-which(df_t$id == "eat06" & df_t$emotion == "anger"),]
df_t2 <- df_t2[-which(df_t$id == "eat05"),]

# Summary statistics for df_t2
summary(df_t2)

# Remove an observer's data due to technical issues
df_o$id <- as.factor(df_o$id)
df_o <- df_o[-which(df_o$id == "eao28"),]

# Summary statistics for df_o
summary(df_o)

################################################################################################################
# Prepare participant ID lists

# Target (composer) IDs
id_t <- c("eat01", "eat16", "eat17", "eat06", "eat02", "eat14", "eat09")

# Observer (listener) IDs
id_o <- unique(as.character(df_o$id))

################################################################################################################
# Import MIR (Music Information Retrieval) features

# Load 2D time-series MIR features
rms <- read.csv("mir_features_240322/mir_rms.csv")
flatness <- read.csv("mir_features_240322/mir_flatness.csv")
zerocross <- read.csv("mir_features_240322/mir_zerocross.csv")
centroid <- read.csv("mir_features_240322/mir_centroid.csv")
rolloff <- read.csv("mir_features_240322/mir_rolloff.csv")

# Create a list of MIR feature names
mir <- c("rms", "flatness", "zerocross", "centroid", "rolloff")

# (Commented out: Optional MIR features)
# chroma_cens <- read.csv("mir_features_240322/mir_chroma_cens.csv")
# chroma_cqt <- read.csv("mir_features_240322/mir_chroma_cqt.csv")
# chroma_stft <- read.csv("mir_features_240322/mir_chroma_stft.csv")
# chroma_tonnetz <- read.csv("mir_features_240322/mir_chroma_tonnetz.csv")
# chroma_vqt <- read.csv("mir_features_240322/mir_chroma_vqt.csv")
# contrast <- read.csv("mir_features_240322/mir_contrast.csv")
# mfcc <- read.csv("mir_features_240322/mir_mfcc.csv")
# tempo <- read.csv("mir_features_240322/mir_tempo.csv")
# dynamic_tempo <- read.csv("mir_features_240322/mir_dynamic_tempo.csv")

################################################################################################################
# Version information for reproducibility
print("R and package version information:")

# Print R version
print(R.version.string)

# Print the operating system information
print(paste("Operating system:", sessionInfo()$running))

# Print versions of loaded libraries
print(paste("ggplot2 version:", packageVersion("ggplot2")))
print(paste("scico version:", packageVersion("scico")))
print(paste("lme4 version:", packageVersion("lme4")))
print(paste("lmerTest version:", packageVersion("lmerTest")))
print(paste("psych version:", packageVersion("psych")))
print(paste("car version:", packageVersion("car")))
print(paste("caret version:", packageVersion("caret")))
print(paste("sjPlot version:", packageVersion("sjPlot")))
print(paste("effects version:", packageVersion("effects")))

################################################################################################################
# Confirmation message
print("Setup complete.")
