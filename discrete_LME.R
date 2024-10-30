################################################################################################################
# discrete_LME.R: Comparing discrete emotional ratings with LME models
################################################################################################################

# Overview:
# - Q1: Do the predictors of listeners’ dynamic emotions (audio features and composer’s emotions) vary based on the methods?

# Comparing models using Linear Mixed Effects (LME) with MIR-only models vs. discrete ratings + MIR models.

################################################################################################################
# Discrete emotional ratings dataframe preparation
################################################################################################################

# Initialize data frame for individual observer LME analysis
df_ob_dis <- data.frame(
    idstimuli = character(), arousal = numeric(), valence = numeric(), dominance = numeric(), observer = numeric(),
    rms = numeric(), flatness = numeric(), zerocross = numeric(), centroid = numeric(), rolloff = numeric(), idob = character()
)

# Reset counter for individual observer entries
ct <- 0

# Add properties of stimuli on each rows of targets' dataframe
df_t$idstimuli <- paste(df_t$id, "_", df_t$emotion, sep = "")
df_t$idstimuli <- as.factor(df_t$idstimuli)

# Populate `df_ob_dis` with individual observer data across all stimuli
for (ids in unique(as.character(all_stimuli$idstimuli))) {
    ob <- get(ids) # Assuming each `ids` refers to a pre-loaded data frame for each stimulus

    for (j in 2:13) { # Assumes observer data spans columns 2 to 13 in `ob`
        ct <- ct + 1

        # Set stimulus-specific discrete ratings and continuous valence as mean over all time points
        df_ob_dis[ct, "idstimuli"] <- ids
        df_ob_dis[ct, "arousal_d"] <- df_t[df_t$idstimuli == ids, "arousal"]
        df_ob_dis[ct, "valence_d"] <- df_t[df_t$idstimuli == ids, "valence"]
        df_ob_dis[ct, "dominance_d"] <- df_t[df_t$idstimuli == ids, "dominance"]

        df_ob_dis[ct, "valence_c"] <- mean(all_stimuli[all_stimuli$idstimuli == ids, "target"])
        df_ob_dis[ct, "observer"] <- mean(ob[, j]) # Set mean observer rating for each observer
        df_ob_dis[ct, "idob"] <- names(ob)[j] # Set observer ID

        # Compute mean values of MIR features for each stimulus
        for (feature in c("rms", "flatness", "zerocross", "centroid", "rolloff")) {
            df_ob_dis[ct, feature] <- mean(all_stimuli[all_stimuli$idstimuli == ids, feature])
        }
    }
}

# Factorize observer ID for mixed models
df_ob_dis$idob <- as.factor(df_ob_dis$idob)

################################################################################################################
# Compare zero model and full models for listeners’ discrete emotions using LME
################################################################################################################

# Zero model with random intercept for observer (baseline model)
m0_ob_dis <- lmer(observer ~ (1 | idob), data = df_ob_dis)
summary(m0_ob_dis)

# LME model with MIR predictors only
lmer_ob_dis1 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idob), data = df_ob_dis)
summary(lmer_ob_dis1) # Results indicate the significance of MIR predictors
anova(m0_ob_dis, lmer_ob_dis1) # Compare zero model with MIR-only model

# LME model with discrete AVD ratings and MIR predictors
lmer_ob_dis2 <- lmer(observer ~ arousal_d + valence_d + dominance_d + rms + flatness + zerocross + centroid + (1 | idob), data = df_ob_dis)
summary(lmer_ob_dis2) # Results show significance levels of predictors
vif(lmer_ob_dis2)
anova(m0_ob_dis, lmer_ob_dis2) # Compare zero model with AVD + MIR model

# Compare MIR-only model with AVD + MIR model
anova(lmer_ob_dis1, lmer_ob_dis2)

################################################################################################################
# Visualization
################################################################################################################

# Plot LME model with MIR predictors only
sjPlot::plot_model(lmer_ob_dis1,
    value.offset = 0.5, title = "",
    axis.labels = c(
        "Spectral Centroid", "Zero-crossing", "Flatness", "RMS"
    ),
    vline.color = "#061423", dot.size = 8, line.size = 2
) +
    theme(text = element_text(size = 25, face = "bold")) +
    scale_color_sjplot("dust")

# Plot LME model with AVD and MIR predictors
sjPlot::plot_model(lmer_ob_dis2,
    value.offset = 0.5, title = "",
    axis.labels = c(
        "Spectral Centroid", "Zero-crossing", "Flatness", "RMS",
        "Dominance", "Valence", "Arousal"
    ),
    vline.color = "#061423", dot.size = 8, line.size = 2
) +
    theme(text = element_text(size = 25, face = "bold")) +
    scale_color_sjplot("dust")

################################################################################################################

# Confirmation message
print("Discrete LME model comparison complete.")

################################################################################################################
