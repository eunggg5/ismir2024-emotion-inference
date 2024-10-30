# Load required libraries
library(Hmisc)
library(corrplot)
library(plotly)

# Set working directory
setwd("/Users/user/workspace/ismir2024-emotion-inference")

# Load and prepare data
eat <- read.csv("target_rate.csv")
eat <- subset(eat, select = -X) # Remove unnecessary column

# Convert relevant columns to factors
eat$id <- as.factor(eat$id)
eat$emotion <- as.factor(eat$emotion)
eat$type <- as.factor(eat$type)

# Define variables for easier referencing
emotions <- c("joy", "sad", "anger")
types <- c("music", "social")
id_t <- unique(eat$id)

# Add 'pn' column for positive/negative classification based on 'emotion'
eat$pn <- ifelse(eat$emotion == "joy", "positive", "negative")
eat$pn <- as.factor(eat$pn)

# Quick summary and standard deviation calculations for 'sad' emotion
summary(eat[eat$emotion == "sad", ])
sd(eat[eat$emotion == "sad", "dominance"])

# Select relevant columns for 3D plot
df_eat <- eat[, c("id", "emotion", "valence", "arousal", "dominance")]

# 3D AVD (Arousal-Valence-Dominance) Model Visualization
fig <- plot_ly(df_eat,
  x = ~valence, y = ~arousal, z = ~dominance,
  mode = "markers",
  symbol = ~emotion, symbols = c("square", "cross", "circle"),
  color = ~emotion, colors = c("#fb4f4f", "#fbc93d", "#6cc0e5")
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(
        title = "<b> Valence </b>",
        titlefont = list(size = 20),
        tickfont = list(size = 15)
      ),
      yaxis = list(
        title = "<b> Arousal </b>",
        titlefont = list(size = 20),
        tickfont = list(size = 15)
      ),
      zaxis = list(
        title = "<b> Dominance </b>",
        titlefont = list(size = 20),
        tickfont = list(size = 15)
      )
    ),
    legend = list(
      title = list(
        text = "<b> Emotion </b>",
        font = list(size = 16)
      )
    )
  )

# Display the 3D plot
fig
