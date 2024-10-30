################################################################################################################
# data_preparation.R: Create dataframe containing composer & listeners' ratings and MIR per music stimuli
################################################################################################################

print("Preparing a total dataset with composers, listeners' ratings, and MIR features...")

# List of music emotion types
emotions <- c("joy", "sad", "anger")

################################################################################################################
# Create variable per music stimuli that contains composers & listeners' emotional ratings
################################################################################################################

# Process each target id and emotion
for (idt in id_t) {
    for (emo in emotions) {
        # Skip specific combinations not used in the experiment
        if ((idt == "eat01" & emo == "sad") |
            (idt == "eat14" & emo == "joy") |
            (idt == "eat06" & emo == "anger")) {
            next
        } else {
            # Retrieve target's ratings
            target_scores <- gsub("\\[|\\]", "", df_t[df_t$id == idt & df_t$emotion == emo, "score_2Hz"])
            target <- as.numeric(strsplit(target_scores, ", ")[[1]])

            # Assign variable name for target's id and emotion
            var_name <- paste(idt, "_", emo, sep = "")
            assign(var_name, data.frame(target = target))

            # Get all observer responses for the current target-emotion pair
            df_observers <- df_o[df_o$target == idt & df_o$emotion == emo, ]

            # For each observer, retrieve and add ratings to the target data
            for (i in 1:nrow(df_observers)) {
                observer_id <- df_observers[i, "id"]
                observer_scores <- as.numeric(strsplit(gsub("\\[|\\]", "", df_observers[i, "score_2Hz"]), ", ")[[1]])
                observer_df <- data.frame(observer_scores)
                colnames(observer_df) <- as.character(observer_id)
                assign(var_name, cbind(get(var_name), observer_df)) # Append observer ratings to target data
            }
        }
    }
}

################################################################################################################

# Calculate the median of listener ratings per time point and add time column
for (idt in id_t) {
    for (emo in emotions) {
        # Skip specific combinations not used in the experiment
        if ((idt == "eat01" & emo == "sad") |
            (idt == "eat14" & emo == "joy") |
            (idt == "eat06" & emo == "anger")) {
            next
        } else {
            var_name <- paste(idt, "_", emo, sep = "")
            df <- get(var_name)
            observer_median <- apply(df[, 2:13], 1, median) # Calculate the median rating across observers
            time <- seq(0, (nrow(df) - 1) * 0.5, by = 0.5) # Generate a time sequence at 0.5s intervals
            assign(var_name, cbind(df, observer_med = observer_median, time = time)) # Add median and time columns
        }
    }
}

################################################################################################################

# Sample visualization for each emotion
plot_emotion <- function(data, color_target, color_median, ylim_vals = c(1, 9)) {
    ggplot(data, aes(x = time)) +
        theme(
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            axis.title.x = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 17),
            axis.text.y = element_text(size = 17)
        ) +
        geom_line(aes(y = observer_med, group = 1), color = color_median, size = 1.5) +
        geom_line(aes(y = target, group = 1), color = color_target, size = 2) +
        scale_y_continuous(name = "Valence", limits = ylim_vals)
}

# Plot each emotion with defined color scheme
plot_emotion(eat01_joy[1:(nrow(eat01_joy) - 8), ], "#ed553b", "#000000")
plot_emotion(eat09_sad[3:(nrow(eat09_sad) - 4), ], "#3caea3", "#000000")
plot_emotion(eat16_anger[4:(nrow(eat16_anger) - 14), ], "#a41f13", "#000000")

################################################################################################################
# Generate total dataset
################################################################################################################

# Define trimming indices for each stimulus based on manually checked timepoints
get_trimming_indices <- function(name, df) {
    switch(name,
        "eat01_anger" = 5:(nrow(df) - 7),
        "eat01_joy" = 2:(nrow(df) - 8),
        "eat02_anger" = 4:(nrow(df) - 6),
        "eat02_joy" = 3:(nrow(df) - 6),
        "eat02_sad" = 5:(nrow(df) - 3),
        "eat06_joy" = 2:(nrow(df) - 4),
        "eat06_sad" = 5:(nrow(df) - 2),
        "eat09_anger" = 4:nrow(df),
        "eat09_joy" = 4:(nrow(df) - 3),
        "eat09_sad" = 3:(nrow(df) - 4),
        "eat14_anger" = 3:(nrow(df) - 4),
        "eat14_sad" = 6:(nrow(df) - 5),
        "eat16_joy" = 4:(nrow(df) - 7),
        "eat16_anger" = 4:(nrow(df) - 14),
        "eat16_sad" = 6:(nrow(df) - 7),
        "eat17_joy" = 4:(nrow(df) - 3),
        "eat17_anger" = 5:(nrow(df) - 1),
        "eat17_sad" = 7:(nrow(df) - 4),
    )
}

################################################################################################################

# Create an empty dataframe to store data for all stimuli
all_stimuli <- data.frame(
    time = c(), target = c(), observer = c(), idstimuli = c(),
    rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c()
)

# Process each target-emotion combination for MIR features
for (idt in id_t) {
    for (emo in emotions) {
        if ((idt == "eat01" & emo == "sad") |
            (idt == "eat14" & emo == "joy") |
            (idt == "eat06" & emo == "anger")) {
            next
        } else {
            name <- paste(idt, "_", emo, sep = "")
            df <- get(name)
            df$idstimuli <- name # Assign unique identifier to each stimulus

            # Add MIR features from pre-loaded datasets
            for (feature in mir) {
                df_mir <- get(feature)
                feature_values <- as.numeric(strsplit(gsub("\\[|\\]", "", df_mir[grep(name, df_mir$id), 2]), ", ")[[1]])
                df[, feature] <- feature_values[1:nrow(df)]
            }

            # Apply trimming based on the defined indices
            df <- df[get_trimming_indices(name, df), ]

            # Print the number of rows to compare with the original script
            print(paste(name, "rows after trimming:", nrow(df)))

            # # Print consistency checks (ICC calculation placeholder)
            # print(name)
            # print(ICC(df[, c(2:13)])) # Adjusted to actual function if needed

            # Append to the final all_stimuli dataset
            all_stimuli <- rbind(all_stimuli, df[, c(
                "time", "target", "observer_med", "idstimuli",
                "rms", "flatness", "zerocross", "centroid", "rolloff"
            )])
        }
    }
}

# Rename columns
colnames(all_stimuli)[colnames(all_stimuli) == "observer_med"] <- "observer"

all_stimuli$idstimuli <- as.factor(all_stimuli$idstimuli)
summary(all_stimuli)

# Normalize MIR features within each target (composer level)
for (feature in mir) {
    for (idt in id_t) {
        raw_mir <- all_stimuli[grep(idt, all_stimuli$idstimuli), feature]
        all_stimuli[grep(idt, all_stimuli$idstimuli), feature] <- scale(raw_mir, center = min(raw_mir), scale = max(raw_mir) - min(raw_mir))
    }
}

################################################################################################################

# Sample MIR feature visualization for 'zerocross'
ggplot(data = all_stimuli[all_stimuli$idstimuli == "eat01_anger", ], aes(x = time)) +
    ggtitle("zerocross") +
    theme(
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
    ) +
    geom_line(aes(y = zerocross, group = 1), color = "#429ebd") +
    scale_x_continuous(limits = c(0, 95)) +
    scale_y_continuous(limits = c(0, 1))

################################################################################################################

# Confirmation message
print("Data preparation complete.")

################################################################################################################
