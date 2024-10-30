################################################################################################################
# dynamic_LME_joy_happiness.R: Analyzing joy stimuli with LME models
################################################################################################################

# Subset data for joy stimuli
joy_stimuli <- subset(all_stimuli, grepl("joy", idstimuli))

################################################################################################################
# Step 1: Model Comparison for Observers' Ratings with MIR Features
################################################################################################################

## Observer Ratings
# Zero model: Only random effect by idstimuli
m0_ob_joy <- lmer(observer ~ (1 | idstimuli),
    data = joy_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_ob_joy)

# Full model with MIR predictors
lmer_ob_joy1 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
lmer_ob_joy2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

# Model summaries and comparisons
summary(lmer_ob_joy1) # Result: rms ***, zerocross *
summary(lmer_ob_joy2) # Result: target ***, rms ***
anova(m0_ob_joy, lmer_ob_joy1) # Compare zero model with MIR-only model
anova(lmer_ob_joy1, lmer_ob_joy2) # Compare MIR-only model with Valence + MIR model

################################################################################################################
# Step 2: Model Comparison for Target Ratings with MIR Features
################################################################################################################

## Target Ratings
# Zero model
m0_tg_joy <- lmer(target ~ (1 | idstimuli),
    data = joy_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
summary(m0_tg_joy)

# Full model with MIR predictors
lmer_tg_joy <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(lmer_tg_joy) # Result: rms ***, zerocross ***, centroid *
anova(m0_tg_joy, lmer_tg_joy) # Compare zero model with MIR model for target

################################################################################################################
# Step 3: Shared Dynamics - Modeling Observer Ratings as a Function of Target Ratings
################################################################################################################

## LME Model for Shared Dynamics
m1_joy <- lmer(observer ~ target + (1 | idstimuli),
    data = joy_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)
m2_joy <- lmer(observer ~ target + (1 + target | idstimuli),
    data = joy_stimuli, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
)

summary(m1_joy)
summary(m2_joy) # Note: No significant effects found
anova(m0_ob_joy, m2_joy)
anova(m1_joy, m2_joy)

# Visualization of the model effect
x_target_joy <- as.data.frame(effects::effect(term = "target", mod = m2_joy))

ggplot() +
    geom_point(data = joy_stimuli, aes(target, observer)) +
    geom_point(data = x_target_joy, aes(x = target, y = fit), color = "blue") +
    geom_line(data = x_target_joy, aes(x = target, y = fit), color = "blue") +
    geom_ribbon(data = x_target_joy, aes(x = target, ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################
# Step 4: Interaction Effects - Investigating Interactions Between Composer Ratings and MIR Features
################################################################################################################

## Interaction models with individual MIR features
# Model with interaction between target and rms
m3_joy_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_joy_rms) # Result: target:rms ***, rms ***, target *
anova(m0_ob_joy, m3_joy_rms)
anova(m2_joy, m3_joy_rms)

# Model with interaction between target and zerocross
m3_joy_zerocross <- lmer(observer ~ target + zerocross + target:zerocross + (1 + target | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_joy_zerocross) # Result: target:zerocross ***, zerocross ***
anova(m0_ob_joy, m3_joy_zerocross)
anova(m2_joy, m3_joy_zerocross)

# Model with interaction between target and centroid
m3_joy_centroid <- lmer(observer ~ target + centroid + target:centroid + (1 + target | idstimuli),
    data = joy_stimuli, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(m3_joy_centroid) # Result: target:centroid ***, centroid ***
anova(m0_ob_joy, m3_joy_centroid)
anova(m2_joy, m3_joy_centroid)

# Visualization of interactions
x_rms_joy <- as.data.frame(effects::effect(term = "target*rms", mod = m3_joy_rms, KR = TRUE))
ggplot(data = x_rms_joy, aes(target, color = rms)) +
    geom_point(data = joy_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = rms), size = 1.2) +
    geom_line(aes(y = lower, group = rms), linetype = 3) +
    geom_line(aes(y = upper, group = rms), linetype = 3) +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

# Visualization for centroid interaction
x_centroid_joy <- as.data.frame(effects::effect(term = "target*centroid", mod = m3_joy_centroid, KR = TRUE))
ggplot(data = x_centroid_joy, aes(target, color = centroid)) +
    geom_point(data = joy_stimuli, aes(target, observer), color = "gray") +
    geom_line(aes(y = fit, group = centroid), size = 1.2) +
    geom_line(aes(y = lower, group = centroid), linetype = 3) +
    geom_line(aes(y = upper, group = centroid), linetype = 3) +
    labs(x = "Targets' Expressed Emotions", y = "Listeners' Perceived Emotions") +
    scale_x_continuous(limits = c(1, 9)) +
    scale_y_continuous(limits = c(1, 9))

################################################################################################################

# Completion message
print("Comparisons of dynamic LME models with joy/happiness emotion complete.")

################################################################################################################
