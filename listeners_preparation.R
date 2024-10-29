######################################################################################################
# listeners_preparation.R: Prepare representative data for listeners' emotional ratings
######################################################################################################

print("Preparing representative listeners' ratings...")

# Define the emotions of interest
emotions <- c("joy", "sad", "anger")

# Iterate over each target id and emotion
for (idt in id_t) {
  for (emo in emotions) {
    
    # Skip specific combinations of target and emotion not used in experiments
    if ((idt == "eat01" & emo == "sad") | 
        (idt == "eat14" & emo == "joy") | 
        (idt == "eat06" & emo == "anger")) {
      next
    }
    
    # Prepare target's ratings
    target_scores <- gsub("\\[|\\]", "", df_t[df_t$id == idt & df_t$emotion == emo, "score_2Hz"])
    target <- as.numeric(strsplit(target_scores, ", ")[[1]])
    
    # Create variable name for the target's id and emotion
    var_name <- paste(idt, emo, sep = "_")
    assign(var_name, data.frame(target = target))
    
    # Get all observer responses for the target and emotion
    df_observers <- df_o[df_o$target == idt & df_o$emotion == emo, ]
    
    # Add observer ratings to the target data
    for (i in 1:nrow(df_observers)) {
      observer_id <- df_observers[i, "id"]
      observer_scores <- as.numeric(strsplit(gsub("\\[|\\]", "", df_observers[i, "score_2Hz"]), ", ")[[1]])
      observer_df <- data.frame(observer_scores)
      colnames(observer_df) <- as.character(observer_id)
      
      assign(var_name, cbind(get(var_name), observer_df))
    }
  }
}

# Calculate the median of 12 listener ratings per time point
for (idt in id_t) {
  for (emo in emotions) {
    
    # Skip specific combinations of target and emotion
    if ((idt == "eat01" & emo == "sad") | 
        (idt == "eat14" & emo == "joy") | 
        (idt == "eat06" & emo == "anger")) {
      next
    }
    
    # Calculate median observer ratings and append to the data
    var_name <- paste(idt, emo, sep = "_")
    df_ratings <- get(var_name)
    observer_median <- apply(df_ratings[, 2:13], 1, median)
    assign(var_name, cbind(get(var_name), observer_median))
  }
}

# Function to create ggplot visualizations
plot_emotion_ratings <- function(data, color_target, color_median, ylim_vals = c(1, 9), xlim_vals = NULL) {
  ggplot(data, aes(x = time)) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 17),
      axis.text.y = element_text(size = 17)
    ) +
    geom_line(aes(y = observer_med, group = 1), color = color_median, size = 2) +
    geom_line(aes(y = target, group = 1), color = color_target, size = 2) +
    scale_y_continuous(name = "Valence", limits = ylim_vals) +
    scale_x_continuous(limits = xlim_vals)
}

# Create individual plots for each emotion
plot_emotion_ratings(eat16_anger[4:(nrow(eat16_anger) - 14),], '#a41f13', '#000000')
plot_emotion_ratings(eat01_joy[1:(nrow(eat01_joy) - 8),], '#ed553b', '#000000')
plot_emotion_ratings(eat09_sad[3:(nrow(eat09_sad) - 4),], '#3caea3', '#000000')
plot_emotion_ratings(eat17_joy, '#ed553b', '#000000', ylim_vals = c(1, 9), xlim_vals = c(0, 100))