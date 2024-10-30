################################################################################################################
# dynamic_LME_anger.R: Analyzing anger stimuli with LME models
################################################################################################################

# Subset data for anger stimuli
anger_stimuli <- subset(all_stimuli, grepl("anger", idstimuli))

################################################################################################################
# Step 1: Model Comparison for Observers' Ratings with MIR Features
################################################################################################################

## Observer Ratings
# Zero model: Only random effect by idstimuli
m0_ob_anger <- lmer(observer ~ (1 | idstimuli),
    data = anger_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_ob_anger)

# Full model with MIR predictors
lmer_ob_anger1 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
lmer_ob_anger2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

# Model summaries and comparisons
summary(lmer_ob_anger1) # Result: rms ***, zerocross *
summary(lmer_ob_anger2) # Result: target ***, rms ***, zerocross *
anova(m0_ob_anger, lmer_ob_anger1) # Compare zero model with MIR-only model
anova(lmer_ob_anger1, lmer_ob_anger2) # Compare MIR-only model with Valence + MIR model

################################################################################################################
# Step 2: Model Comparison for Target Ratings with MIR Features
################################################################################################################

## Target Ratings
# Zero model
m0_tg_anger <- lmer(target ~ (1 | idstimuli),
    data = anger_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_tg_anger)

# Full model with MIR predictors
lmer_tg_anger <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_tg_anger) # Result: rms *, flatness (.), centroid ***
anova(m0_tg_anger, lmer_tg_anger) # Compare zero model with MIR model for target

################################################################################################################
# Step 3: Shared Dynamics - Modeling Observer Ratings as a Function of Target Ratings
################################################################################################################

## LME Model for Shared Dynamics
m1_anger <- lmer(observer ~ target + (1 | idstimuli),
    data = anger_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
m2_anger <- lmer(observer ~ target + (1 + target | idstimuli),
    data = anger_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)

summary(m1_anger)
summary(m2_anger)
anova(m0_ob_anger, m2_anger)
anova(m1_anger, m2_anger)

# Visualization of the model effect
x_target_anger <- as.data.frame(effects::effect(term = "target", mod = m2_anger))

ggplot() +
    geom_point(data = anger_stimuli, aes(target, observer)) +
    geom_point(data = x_target_anger, aes(x = target, y = fit), color = "blue") +
    geom_line(data = x_target_anger, aes(x = target, y = fit), color = "blue") +
    geom_ribbon(data = x_target_anger, aes(x = target, ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################
# Step 4: Interaction Effects - Investigating Interactions Between Composer Ratings and MIR Features
################################################################################################################

## Interaction models with individual MIR features
# Model with interaction between target and rms
m3_anger_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_anger_rms) # Result: target:rms ***, target ***,
anova(m0_ob_anger, m3_anger_rms)
anova(m2_anger, m3_anger_rms)

# Model with interaction between target and centroid
m3_anger_centroid <- lmer(observer ~ target + centroid + target:centroid + (1 + target | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_anger_centroid) # Result: target:centroid (.), centroid (.), target **
anova(m0_ob_anger, m3_anger_centroid)
anova(m2_anger, m3_anger_centroid)

# Model with interaction between target and zerocross
m3_anger_zerocross <- lmer(observer ~ target + zerocross + target:zerocross + (1 + target | idstimuli),
    data = anger_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_anger_zerocross) # Result: target:zerocross ***, zerocross **
anova(m0_ob_anger, m3_anger_zerocross)
anova(m2_anger, m3_anger_zerocross)

# Visualization of interactions
x_rms_anger <- as.data.frame(effects::effect(term = "target*rms", mod = m3_anger_rms, KR = TRUE))
ggplot(data = x_rms_anger, aes(target, color = rms)) +
    geom_point(data = anger_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = rms), size = 1.2) +
    geom_line(aes(y = lower, group = rms), linetype = 3) +
    geom_line(aes(y = upper, group = rms), linetype = 3) +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################

# Completion message
print("Comparisons of dynamic LME models with anger emotion complete.")

################################################################################################################
