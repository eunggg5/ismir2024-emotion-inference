################################################################################################################
# dynamic_LME_sadness.R: Analyzing sadness stimuli with LME models
################################################################################################################

# Subset data for sadness stimuli
sad_stimuli <- subset(all_stimuli, grepl("sad", idstimuli))

################################################################################################################
# Step 1: Model Comparison for Observers' Ratings with MIR Features
################################################################################################################

## Observer Ratings
# Zero model: Only random effect by idstimuli
m0_ob_sad <- lmer(observer ~ (1 | idstimuli),
    data = sad_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_ob_sad)

# Full model with MIR predictors
lmer_ob_sad1 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = sad_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
lmer_ob_sad2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = sad_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

# Model summaries and comparisons
summary(lmer_ob_sad1) # Result: rms ***
summary(lmer_ob_sad2) # Result: target ***, rms ***
anova(m0_ob_sad, lmer_ob_sad1) # Compare zero model with MIR-only model
anova(lmer_ob_sad1, lmer_ob_sad2) # Compare MIR-only model with Valence + MIR model

################################################################################################################
# Step 2: Model Comparison for Target Ratings with MIR Features
################################################################################################################

## Target Ratings
# Zero model
m0_tg_sad <- lmer(target ~ (1 | idstimuli),
    data = sad_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_tg_sad)

# Full model with MIR predictors
lmer_tg_sad <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = sad_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_tg_sad) # Result: rms *, flatness (.)
anova(m0_tg_sad, lmer_tg_sad) # Compare zero model with MIR model for target

################################################################################################################
# Step 3: Shared Dynamics - Modeling Observer Ratings as a Function of Target Ratings
################################################################################################################

## LME Model for Shared Dynamics
m1_sad <- lmer(observer ~ target + (1 | idstimuli),
    data = sad_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
m2_sad <- lmer(observer ~ target + (1 + target | idstimuli),
    data = sad_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)

summary(m1_sad)
summary(m2_sad)
anova(m0_ob_sad, m2_sad)
anova(m1_sad, m2_sad)

# Visualization of the model effect
x_target_sad <- as.data.frame(effects::effect(term = "target", mod = m2_sad))

ggplot() +
    geom_point(data = sad_stimuli, aes(target, observer)) +
    geom_point(data = x_target_sad, aes(x = target, y = fit), color = "blue") +
    geom_line(data = x_target_sad, aes(x = target, y = fit), color = "blue") +
    geom_ribbon(data = x_target_sad, aes(x = target, ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################
# Step 4: Interaction Effects - Investigating Interactions Between Composer Ratings and MIR Features
################################################################################################################

## Interaction models with individual MIR features
# Model with interaction between target and rms
m3_sad_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli),
    data = sad_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_sad_rms) # Result: target *
anova(m0_ob_sad, m3_sad_rms)
anova(m2_sad, m3_sad_rms)

# Visualization of interactions
x_rms_sad <- as.data.frame(effects::effect(term = "target*rms", mod = m3_sad_rms, KR = TRUE))
ggplot(data = x_rms_sad, aes(target, color = rms)) +
    geom_point(data = sad_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = rms), size = 1.2) +
    geom_line(aes(y = lower, group = rms), linetype = 3) +
    geom_line(aes(y = upper, group = rms), linetype = 3) +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################

# Completion message
print("Comparisons of dynamic LME models with sadness emotion complete.")

################################################################################################################
