################################################################################################################
# dynamic_LME_total.R: Comparing dynamic emotional ratings with LME models
################################################################################################################

# Overview:
# - Q1: Do the predictors of listeners’ dynamic emotions (audio features and composer’s emotions) vary based on the methods?
# - Q2: Which audio features predict the dynamics of composers/listeners' perceived emotions, respectively?
# - Q3: Which audio features predict both composers and listeners' emotions?

# Comparing models using Linear Mixed Effects (LME) with MIR-only models vs. dynamic ratings + MIR models.
# Also, exploring how audio features predict dynamics for listeners and composers.

################################################################################################################
# Step 1: Compare zero model and full models for listeners’ dynamic emotions
################################################################################################################

# Zero model with intercept-only random effect
m0_ob_dyn <- lmer(observer ~ (1 | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)

summary(m0_ob_dyn)

# Full models to assess the effect of MIR features on listeners’ perceived emotions
# LME model with MIR predictors only
lmer_ob_dyn1 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = all_stimuli,
    REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_ob_dyn1) # Results: rms significant
anova(m0_ob_dyn, lmer_ob_dyn1) # Compare zero model with MIR-only model

# LME model with dynamic Valence ratings and MIR predictors
lmer_ob_dyn2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = all_stimuli,
    REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_ob_dyn2) # Results: target and rms significant
anova(m0_ob_dyn, lmer_ob_dyn1) # Compare zero model with Valence + MIR model

# Compare MIR-only model with Valence + MIR model
anova(lmer_ob_dyn1, lmer_ob_dyn2)

# Visualize model effects
sjPlot::plot_model(lmer_ob_dyn1,
    show.values = TRUE, show.p = TRUE,
    title = "Effect of MIR features on Listeners' Perceived Emotions"
)

################################################################################################################
# Step 2: Predict the dynamics of composers' perceived emotions based on audio features using LME
################################################################################################################

# Zero model for composers’ dynamic emotions
m0_tg_dyn <- lmer(target ~ (1 | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_tg_dyn)

# Full model to assess the effect of MIR features on composers’ expressed emotions
lmer_tg_dyn <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = all_stimuli,
    REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_tg_dyn) # Results: centroid significant
anova(m0_tg_dyn, lmer_tg_dyn) # Compare zero model with MIR feature model for target

# Visualize model effects
sjPlot::plot_model(lmer_tg_dyn,
    show.values = TRUE, show.p = TRUE,
    title = "Effect of MIR features on Composers' Expressed Emotions"
)

################################################################################################################
# Step 3: Investigate shared predictors of both listeners' and composers' emotions
# All Emotions Stimuli: joy/happiness, sadness, and anger
################################################################################################################

# Model to assess shared dynamics between listeners and composers
m1 <- lmer(observer ~ target + (1 | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
m2 <- lmer(observer ~ target + (1 + target | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)

summary(m1)
summary(m2)
anova(m1, m2) # Compare fixed vs. random slopes

# Extract and visualize the effect of "target" from model m2
x_target <- as.data.frame(effects::effect(term = "target", mod = m2))

ggplot() +
    geom_point(data = all_stimuli, aes(x = target, y = observer)) +
    geom_point(data = x_target, aes(x = target, y = fit), color = "#0091ff") +
    geom_line(data = x_target, aes(x = target, y = fit), color = "#0091ff", size = 2) +
    geom_ribbon(data = x_target, aes(x = target, ymin = lower, ymax = upper), alpha = 0.3, fill = "#0091ff") +
    labs(x = "Composers' Emotional Intentions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9)) +
    theme(
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
    )

################################################################################################################
# Step 4: Interaction effects between target and specific MIR features
################################################################################################################

# Interaction model with rms
m3_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m3_rms) # Results: target:rms significant, rms and target significant

# Interaction model with centroid
m3_centroid <- lmer(observer ~ target + centroid + target:centroid + (1 + target | idstimuli),
    data = all_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m3_centroid) # Results: target:centroid marginally significant, centroid significant

# Visualize interactions with rms
x_rms <- as.data.frame(effects::effect(term = "target*rms", mod = m3_rms, KR = TRUE))
ggplot(data = x_rms, aes(target, color = rms)) +
    geom_point(data = all_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = rms), size = 2) +
    geom_line(aes(y = lower, group = rms), linetype = 3, size = 1.3) +
    geom_line(aes(y = upper, group = rms), linetype = 3, size = 1.3) +
    labs(x = "Composers' Emotional Intentions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9)) +
    theme(
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11)
    )

# Visualize interactions with centroid
x_centroid <- as.data.frame(effects::effect(term = "target*centroid", mod = m3_centroid, KR = TRUE))
ggplot(data = x_centroid, aes(target, color = centroid)) +
    geom_point(data = all_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = centroid), size = 1.2) +
    geom_line(aes(y = lower, group = centroid), linetype = 3) +
    geom_line(aes(y = upper, group = centroid), linetype = 3) +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################

# Confirmation message
print("Comparisons of dynamic LME models with all three emotions complete.")

################################################################################################################
