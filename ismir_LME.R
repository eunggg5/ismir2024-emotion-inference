library(ggplot2)
library(scico)
library(lme4)
library(lmerTest)
library(psych)
library(car)
library(caret)
library(sjPlot)
library(effects)
# install.packages("effects")

########################################################################################
########## R script for Linear mixed effect (LME) models (Tabak et al., 2023) ##########
########################################################################################


setwd("~/Library/CloudStorage/GoogleDrive-oheunji97@gmail.com/내 드라이브/Document/2KAIST/2024/conference/ISMIR")


df_t <- read.csv("target_rate_2Hz.csv")
df_o <- read.csv("observer_rate_2Hz.csv")

df_o <- df_o[-which(df_o$id == "eao28"), ]

df_o$id <- as.factor(df_o$id)

id_t <- c("eat01", "eat16", "eat17", "eat06", "eat02", "eat14", "eat09")
id_o <- unique(as.character(df_o$id))


# only 18 targets

df_t2 <- df_t[-which(df_t$id=="eat01" & df_t$emotion=="sad"),]
df_t2 <- df_t2[-which(df_t$id=="eat14" & df_t$emotion=="joy"),]
df_t2 <- df_t2[-which(df_t$id=="eat06" & df_t$emotion=="anger"),]
df_t2 <- df_t2[-which(df_t$id=="eat05"),]

summary(df_t2)
sd(df_t2$length)

summary(df_t2[df_t2$emotion=="joy",])
sd(df_t2[df_t2$emotion=="joy","valence"]); sd(df_t2[df_t2$emotion=="joy","arousal"]); sd(df_t2[df_t2$emotion=="joy","dominance"])
summary(df_t2[df_t2$emotion=="sad",])
sd(df_t2[df_t2$emotion=="sad","valence"]); sd(df_t2[df_t2$emotion=="sad","arousal"]); sd(df_t2[df_t2$emotion=="sad","dominance"])
summary(df_t2[df_t2$emotion=="anger",])
sd(df_t2[df_t2$emotion=="anger","valence"]); sd(df_t2[df_t2$emotion=="anger","arousal"]); sd(df_t2[df_t2$emotion=="anger","dominance"])


################################################################################################################
################################################################################################################

#### MIR features

# 1-d
rms <- read.csv("mir_features_240322/mir_rms.csv")
flatness <- read.csv("mir_features_240322/mir_flatness.csv")
zerocross <- read.csv("mir_features_240322/mir_zerocross.csv")
centroid <- read.csv("mir_features_240322/mir_centroid.csv")
rolloff <- read.csv("mir_features_240322/mir_rolloff.csv")

mir <- c("rms", "flatness", "zerocross", "centroid", "rolloff")

# # other features
# chroma_cens <- read.csv("mir_features_240322/mir_chroma_cens.csv")
# chroma_cqt <- read.csv("mir_features_240322/mir_chroma_cqt.csv")
# chroma_stft <- read.csv("mir_features_240322/mir_chroma_stft.csv")
# chroma_tonnetz <- read.csv("mir_features_240322/mir_chroma_tonnetz.csv")
# chroma_vqt <- read.csv("mir_features_240322/mir_chroma_vqt.csv")
# contrast <- read.csv("mir_features_240322/mir_contrast.csv")
# mfcc <- read.csv("mir_features_240322/mir_mfcc.csv")
# 
# tempo <- read.csv("mir_features_240322/mir_tempo.csv")
# dynamic_tempo <- read.csv("mir_features_240322/mir_dynamic_tempo.csv")


################################################################################################################
################################################################################################################
###### Representation data of observers' ratings (12)
################################################################################################################
################################################################################################################

emotions <- c("joy", "sad", "anger")

for (idt in id_t){
  for (emo in emotions){
    
    if ((idt == "eat01" & emo == "sad") 
        | (idt == "eat14" & emo == "joy") 
        | (idt == "eat06" & emo == "anger")){
      
      next
      
    }
    
    else {
      
      # target's ratings
      target <- as.numeric(strsplit(gsub("\\[|\\]", "", df_t[df_t$id==idt & df_t$emotion==emo, "score_2Hz"]), ", ")[[1]])
      
      # assign variable name of target's id
      name <- paste(idt, "_", emo, sep = "")
      assign(name, data.frame(target = target))
      
      # all responses data for target (12 participants)
      df <- df_o[df_o$target==idt & df_o$emotion==emo,]
      
      for (i in 1:nrow(df)){
        
        # get the observer's id
        ide <- df[i, "id"]
        
        # emotion ratings
        observer <- data.frame(ide = as.numeric(strsplit(gsub("\\[|\\]", "", df[i, "score_2Hz"]), ", ")[[1]]))
        colnames(observer)[colnames(observer) == "ide"] <- as.character(ide)
        
        assign(name, cbind(get(name), observer))
        
      }
    }
  }
}

###### 1) density estimation (KDE)

for (idt in id_t){
  for (emo in emotions){
    
    if ((idt == "eat01" & emo == "sad") 
        | (idt == "eat14" & emo == "joy") 
        | (idt == "eat06" & emo == "anger")){
      
      next
      
    }
    
    else {
      
      name <- paste(idt, "_", emo, sep = "")
      df <- get(name)
      
      density_estimates <- apply(df[,2:13], 1, function(row) {
        density_est <- density(row) 
        represent <- density_est$x[which.max(density_est$y)]  # 밀도 추정 결과 중 최댓값에 해당하는 x 값 선택
        return(represent)  # 대표값 반환
      })
      
      observer_KDE <- density_estimates
      time <- seq(0, (nrow(df)-1) * 0.5, by = 0.5)
      
      assign(name, cbind(get(name), observer_KDE, time))
      
    }
  }
}


###### 2) median / mean


for (idt in id_t){
  for (emo in emotions){
    
    if ((idt == "eat01" & emo == "sad") 
        | (idt == "eat14" & emo == "joy") 
        | (idt == "eat06" & emo == "anger")){
      
      next
      
    }
    
    else {
      
      name <- paste(idt, "_", emo, sep = "")
      df <- get(name)
      
      observer_med <- apply(df[, 2:13], 1, median)
      observer_mean <- apply(df[, 2:13], 1, mean)
      
      assign(name, cbind(get(name), observer_med, observer_mean))
      
    }
  }
}


# visualization
ggplot(data = eat16_anger[4:(nrow(eat16_anger)-14),], aes(x = time)) +
  # ggtitle("Anger") +
  theme(plot.title = element_text(size=20, hjust=0.5, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17)) +
  geom_line(aes(y = eao03, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao04, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao09, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao10, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao15, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao16, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao21, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao22, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao27, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao33, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao34, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao37, group = 1), color = 'dark grey', linetype = 1) +
  # geom_line(aes(y = observer_KDE, group = 1), color = '#3caea3', linetype = 1, size = 1.5) +
  geom_line(aes(y = observer_med, group = 1), color = '#000000', linetype = 1, size = 2) +
  # geom_line(aes(y = observer_mean, group = 1), color = '#173f5f', linetype = 1, size = 1.5) +
  geom_line(aes(y = target, group = 1), color = '#a41f13', linetype = 1, size = 2) +
  # scale_x_continuous(limits=c(0,82)) +
  scale_y_continuous(name="valence", limits=c(1,9))

ggplot(data = eat01_joy[1:(nrow(eat01_joy)-8),], aes(x = time)) +
  # ggtitle("eat01, joy") +
  theme(plot.title = element_text(size=20, hjust=0.5, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17))+
  geom_line(aes(y = eao13, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao14, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao15, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao16, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao17, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao18, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao31, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao32, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao33, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao34, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao35, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao36, group = 1), color = 'dark grey', linetype = 1) +
  # geom_line(aes(y = observer_KDE, group = 1), color = '#3caea3', linetype = 1, size = 1.5) +
  geom_line(aes(y = observer_med, group = 1), color = '#000000', linetype = 1, size = 2) +
  # geom_line(aes(y = observer_mean, group = 1), color = '#173f5f', linetype = 1, size = 1.5) +
  geom_line(aes(y = target, group = 1), color = '#ed553b', linetype = 1, size = 2) +
  # scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(name="valence", limits=c(1,9))


ggplot(data = eat09_sad[3:(nrow(eat09_sad)-4),], aes(x = time)) +
  # ggtitle("eat02, sad") +
  theme(plot.title = element_text(size=20, hjust=0.5, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17))+
  geom_line(aes(y = eao03, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao05, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao09, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao11, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao15, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao17, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao21, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao23, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao27, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao29, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao33, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao35, group = 1), color = 'dark grey', linetype = 1) +
  # geom_line(aes(y = observer_KDE, group = 1), color = '#3caea3', linetype = 1, size = 1.5) +
  geom_line(aes(y = observer_med, group = 1), color = '#000000', linetype = 1, size = 1.5) +
  # geom_line(aes(y = observer_mean, group = 1), color = '#173f5f', linetype = 1, size = 1.5) +
  geom_line(aes(y = target, group = 1), color = '#3caea3', linetype = 1, size = 1.5) +
  scale_y_continuous(name="valence", limits=c(1,9))

ggplot(data = eat17_joy, aes(x = time)) +
  # ggtitle("eat01, joy") +
  theme(plot.title = element_text(size=20, hjust=0.5, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17))+
  geom_line(aes(y = eao07, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao08, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao09, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao10, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao11, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao12, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao25, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao26, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao27, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao29, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao30, group = 1), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = eao37, group = 1), color = 'dark grey', linetype = 1) +
  # geom_line(aes(y = observer_KDE, group = 1), color = '#3caea3', linetype = 1, size = 1.5) +
  geom_line(aes(y = observer_med, group = 1), color = '#000000', linetype = 1, size = 2) +
  # geom_line(aes(y = observer_mean, group = 1), color = '#173f5f', linetype = 1, size = 1.5) +
  geom_line(aes(y = target, group = 1), color = '#ed553b', linetype = 1, size = 2) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(name="valence", limits=c(1,9))




################################################################################################################
################################################################################################################

######## LME models
# observer: observer_median

# generate total dataset

all_stimuli <- data.frame(time = c(), target = c(), observer = c(), idstimuli = c(), 
                          rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())

for (idt in id_t){
  for (emo in emotions){
    
    if ((idt == "eat01" & emo == "sad") 
        | (idt == "eat14" & emo == "joy") 
        | (idt == "eat06" & emo == "anger")){
      
      next
      
    }
    
    else {
      
      name <- paste(idt, "_", emo, sep = "")
      df <- get(name)
      
      df$idstimuli <- name
      
      # mir feature 
      
      for (feature in mir){
        
        df_mir <- get(feature)
        
        df[,feature] <- as.numeric(strsplit(gsub("\\[|\\]", "", df_mir[grep(name, df_mir$id), 2]), ", ")[[1]])[1:nrow(df)]
        
      }
      
      if (name=="eat01_anger"){
        df <- df[5:(nrow(df)-7),]
      }
      
      else if (name=="eat01_joy"){
        df <- df[2:(nrow(df)-8),]
      }
      
      else if (name=="eat02_anger"){
        df <- df[4:(nrow(df)-6),]
      }
      
      else if (name=="eat02_joy"){
        df <- df[3:(nrow(df)-6),]
      }
      
      else if (name=="eat02_sad"){
        df <- df[5:(nrow(df)-3),]
      }
      
      else if (name=="eat06_joy"){
        df <- df[2:(nrow(df)-4),]
      }
      
      else if (name=="eat06_sad"){
        df <- df[5:(nrow(df)-2),]
      }
      
      else if (name=="eat09_anger"){
        df <- df[4:(nrow(df)),]
      }
      
      else if (name=="eat09_joy"){
        df <- df[4:(nrow(df)-3),]
      }
      
      else if (name=="eat09_sad"){
        df <- df[3:(nrow(df)-4),]
      }
      
      else if (name=="eat14_anger"){
        df <- df[3:(nrow(df)-4),]
      }
      
      else if (name=="eat14_sad"){
        df <- df[6:(nrow(df)-5),]
      }
      
      else if (name=="eat16_joy"){
        df <- df[4:(nrow(df)-7),]
      }
      
      else if (name=="eat16_anger"){
        df <- df[4:(nrow(df)-14),]
      }
      
      else if (name=="eat16_sad"){
        df <- df[6:(nrow(df)-7),]
      }
      
      else if (name=="eat17_joy"){
        df <- df[4:(nrow(df)-3),]
      }
      
      else if (name=="eat17_anger"){
        df <- df[5:(nrow(df)-1),]
      }
      
      else if (name=="eat17_sad"){
        df <- df[7:(nrow(df)-4),]
      }
      
      # print(df)
      
      print(name)
      print(ICC(df[,c(2:13)]))
      print(ICC(df[,c(2:13)]))
      
      all_stimuli <- rbind(all_stimuli, df[,c("time", "target", "observer_med", "idstimuli", 
                                              "rms", "flatness", "zerocross", "centroid", "rolloff")])
      

    }
  }
}

colnames(all_stimuli)[colnames(all_stimuli) == "observer_med"] <- "observer"
all_stimuli$smr <- all_stimuli[sample(nrow(all_stimuli)), c("rms")]

all_stimuli$idstimuli <- as.factor(all_stimuli$idstimuli)
summary(all_stimuli)


# normalization

for (feature in mir){
  
  for (idt in id_t){
    
    raw_mir <- all_stimuli[grep(idt, all_stimuli$idstimuli), feature]
    all_stimuli[grep(idt, all_stimuli$idstimuli), feature] <- scale(raw_mir, center = min(raw_mir), scale = max(raw_mir) - min(raw_mir))

  }
  
}


# mir feature visualization

ggplot(data = all_stimuli[all_stimuli$idstimuli=="eat01_anger",], aes(x = time)) +
  ggtitle("zerocross") +
  theme(plot.title = element_text(size=20, hjust=0.5, face="bold"),
        axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_line(aes(y = zerocross, group = 1), color = '#429ebd', linetype = 1) +
  scale_x_continuous(limits=c(0,95)) +
  scale_y_continuous(limits=c(0,1))

all_stimuli[all_stimuli$idstimuli=="eat17_joy","flatness"]


# all_stimuli <- all_stimuli[-which(all_stimuli$idstimuli=="eat06_joy"),]
# all_stimuli <- all_stimuli[-which(all_stimuli$idstimuli=="eat16_sad"),]
# all_stimuli <- all_stimuli[-which(all_stimuli$idstimuli=="eat02_sad"),]
# all_stimuli <- all_stimuli[-which(all_stimuli$idstimuli=="eat09_sad"),]

# correlation

correlation::correlation(all_stimuli[,c("idstimuli", "rms", "flatness", "zerocross", "centroid", "rolloff")], multilevel = TRUE)




################################################################################################################
#### observer ~ MIR features
#### target ~ MIR features
################################################################################################################

## observer
# zero model
m0_ob <- lmer(observer ~ (1 | idstimuli), data = all_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))

m0_ob2 <- lmer(observer ~ (1 + target | idstimuli), data = all_stimuli, REML = F,
              control = lmerControl(optimizer ='bobyqa'))

summary(m0_ob)
summary(m0_ob2)

# icc_ob <- 3.8329 / (3.8329 + 0.7162); icc_ob

# lmer
# ob_lmer <- lmer(observer ~ rms + flatness + zerocross + centroid + rolloff + (1 | idstimuli), data = all_stimuli,
#                 REML = F, control = lmerControl(optimizer ='bobyqa'))
# 
# ob_lmer2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + rolloff + (1 | idstimuli), data = all_stimuli,
#                 REML = F, control = lmerControl(optimizer ='bobyqa'))
# 
# summary(ob_lmer)     ###### result: rms***
# summary(ob_lmer2)     ###### result: target***, rms***
# anova(m0_ob, ob_lmer)
# anova(ob_lmer, ob_lmer2)

vif(ob_lmer)

ob_lmer <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = all_stimuli,
                REML = F, control = lmerControl(optimizer ='bobyqa'))

ob_lmer2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli), data = all_stimuli,
                 REML = F, control = lmerControl(optimizer ='bobyqa'))

summary(ob_lmer)     ###### result: rms***
summary(ob_lmer2)     ###### result: target***, rms***
anova(m0_ob, ob_lmer)
anova(ob_lmer, ob_lmer2)

sjPlot::plot_model(ob_lmer,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of MIR features on Listeners Perceived Emotions")


# # linear regression model
# ob_lmer <- lm(observer ~ rms + flatness + zerocross + centroid + rolloff, data = all_stimuli)
# summary(ob_lmer)
# vif(ob_lmer) # centroid (18.693), rolloff (14.928)
# 
# ob_lmer <- lm(observer ~ rms + flatness + zerocross, data = all_stimuli)
# summary(ob_lmer)
# vif(ob_lmer)
# 
# train_control <- trainControl(method = "cv", number = 10)
# cv_results <- train(observer ~ rms + flatness + zerocross, data = all_stimuli, method = "lm", trControl = train_control)
# cv_results$results

## target
# zero model
m0_tg <- lmer(target ~ (1 | idstimuli), data = all_stimuli, REML = F,
              control = lmerControl(optimizer ='bobyqa'))
summary(m0_tg)

# icc_tg <- 4.8556 / (4.8556 + 0.9359); icc_tg

# lmer
tg_lmer <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = all_stimuli,
                REML = F, control = lmerControl(optimizer ='bobyqa'))
summary(tg_lmer)      ###### centroid ***
anova(m0_tg, tg_lmer)

vif(tg_lmer)

sjPlot::plot_model(tg_lmer,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of MIR features on Performers' Expressed Emotions")

vif(tg_lmer)

# linear regression model

# tg_lmer <- lm(target ~ rms + flatness + zerocross + centroid + rolloff, data = all_stimuli)
# summary(tg_lmer)
# vif(tg_lmer) # centroid (18.693), rolloff (14.928)
# 
# 
# tg_lmer <- lm(target ~ rms + flatness + zerocross, data = all_stimuli)
# summary(tg_lmer)
# vif(tg_lmer)
# AIC(tg_lmer) # 15612.49
# 
# 
# train_control <- trainControl(method = "cv", number = 10)
# cv_results <- train(target ~ rms + flatness + zerocross, data = all_stimuli, method = "lm", trControl = train_control)
# cv_results$results


################################################################################################################
#### observer ~ target
################################################################################################################

#### model difference


m1 <- lmer(observer ~ target + (1 | idstimuli), data = all_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))
m2 <- lmer(observer ~ target + (1 + target | idstimuli), data = all_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))

summary(m1)
summary(m2)
anova(m1, m2)

coef(summary(m2))
confint(m2, method="Wald")


sjPlot::tab_model(m2)

x_target <- as.data.frame(effects::effect(term= "target", mod= m2))


ggplot() + 
 geom_point(data=all_stimuli, aes(x=target, y=observer)) +
  geom_point(data=x_target, aes(x=target, y=fit), color="#0091ff") +
  geom_line(data=x_target, aes(x=target, y=fit), color="#0091ff", size = 2) +
  geom_ribbon(data= x_target, aes(x=target, ymin=lower, ymax=upper), alpha= 0.3, fill="#0091ff") +
  labs(x="Composers' Emotional Intentions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))


# interaction 
m3_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli), data = all_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))
summary(m3_rms)   ##### target:rms ***, rms ***, target **

sjPlot::tab_model(m3_rms)


anova(m0_ob, m3_rms)
anova(m2, m3_rms)

vif(m3_rms)

m3_centroid <- lmer(observer ~ target + centroid + target:centroid+ (1 + target | idstimuli), data = all_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))
summary(m3_centroid) ##### target:centroid (.), centroid *, target ***

anova(m0_ob, m3_centroid)
anova(m2, m3_centroid)     # p = .043*

vif(m3_centroid)

# m3_smr <- lmer(observer ~ target + smr + target:smr + (1 + target | idstimuli), data = all_stimuli, REML = F,
#               control = lmerControl(optimizer ='bobyqa'))
# summary(m3_smr)

# pchisq((AIC(m3_rms) - AIC(m3_smr)), df = 1, lower.tail = F)
# pchisq((AIC(m2) - AIC(m3_rms)), df = 1, lower.tail = F)

# interaction visualization

x_rms <- as.data.frame(effects::effect(term= "target*rms", mod = m3_rms, KR = T))

ggplot(data=x_rms, aes(target, color=rms)) + 
  geom_point(data=all_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=rms), size=2) +
  geom_line(aes(y=lower, group=rms), linetype=3, size = 1.3) +
  geom_line(aes(y=upper, group=rms), linetype=3, size = 1.3) +
  labs(x="Composers' Emotional Intentions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=11))


x_centroid <- as.data.frame(effects::effect(term= "target*centroid", mod = m3_centroid, KR = T))

ggplot(data=x_centroid, aes(target, color=centroid)) + 
  geom_point(data=all_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=centroid), size=1.2) +
  geom_line(aes(y=lower, group=centroid), linetype=3) +
  geom_line(aes(y=upper, group=centroid), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))
  



################################################################################################################
################################################################################################################
######## joy stimuli
################################################################################################################
################################################################################################################

joy_stimuli <- data.frame(time = c(), target = c(), observer = c(), idstimuli = c(), 
                          rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())


for (i in 1:nrow(all_stimuli)){
  
  if (grepl("joy", all_stimuli[i, "idstimuli"])){
    
    joy_stimuli <- rbind(joy_stimuli, all_stimuli[i,])
    
  }
}

################################################################################################################
#### observer ~ MIR features
#### target ~ MIR features
################################################################################################################

## observer
# zero model
m0_ob_joy <- lmer(observer ~ (1 | idstimuli), data = joy_stimuli, REML = F,
              control = lmerControl(optimizer ='bobyqa'))
summary(m0_ob_joy)

# icc_ob_joy <- 0.9555 / (0.9555 + 0.7698); icc_ob_joy

# lmer
ob_lmer_joy <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = joy_stimuli,
                REML = F, control = lmerControl(optimizer ='bobyqa'))
ob_lmer_joy2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli), data = joy_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))


summary(ob_lmer_joy) ##### result: rms ***, zerocross *
summary(ob_lmer_joy2) ##### result: target ***, rms ***

anova(m0_ob_joy, ob_lmer_joy)
anova(ob_lmer_joy, ob_lmer_joy2)


## target
# zero model
m0_tg_joy <- lmer(target ~ (1 | idstimuli), data = joy_stimuli, REML = F,
              control = lmerControl(optimizer ='bobyqa'))
summary(m0_tg_joy)

# icc_tg_joy <- 0.1008 / (0.1008 + 0.9448); icc_tg_joy

# lmer
tg_lmer_joy <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = joy_stimuli,
                REML = F, control = lmerControl(optimizer ='bobyqa'))
summary(tg_lmer_joy) ##### result: rms ***, zerocross ***, centroid *

anova(m0_tg_joy, tg_lmer_joy)


#### LME model

m1_joy <- lmer(observer ~ target + (1 | idstimuli), data = joy_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))
m2_joy <- lmer(observer ~ target + (1 + target | idstimuli), data = joy_stimuli, REML = F,
           control = lmerControl(optimizer ='bobyqa'))

summary(m1_joy)
summary(m2_joy) ### NO SIGNIFICANT RESULTS
anova(m0_ob_joy, m2_joy)
anova(m1_joy, m2_joy)

sjPlot::tab_model(m2_joy)

# visualization
x_target_joy <- as.data.frame(effects::effect(term= "target", mod= m2_joy))

ggplot() + 
  geom_point(data=joy_stimuli, aes(target, observer)) + 
  geom_point(data=x_target_joy, aes(x=target, y=fit), color="blue") +
  geom_line(data=x_target_joy, aes(x=target, y=fit), color="blue") +
  geom_ribbon(data=x_target_joy, aes(x=target, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))

# interaction
m3_joy_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli), data = joy_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))
summary(m3_joy_rms)   ##### target:rms *** , rms ***, target *
anova(m0_ob_joy, m3_joy_rms)
anova(m2_joy, m3_joy_rms)
 
m3_joy_zerocross <- lmer(observer ~ target + zerocross + target:zerocross + (1 + target | idstimuli), data = joy_stimuli, REML = F,
                   control = lmerControl(optimizer ='bobyqa'))
summary(m3_joy_zerocross)     ##### target:zerocross ***, zerocross ***
anova(m0_ob_joy, m3_joy_zerocross)
anova(m2_joy, m3_joy_zerocross)

m3_joy_centroid <- lmer(observer ~ target + centroid + target:centroid + (1 + target | idstimuli), data = joy_stimuli, REML = F,
                        control = lmerControl(optimizer ='bobyqa'))
summary(m3_joy_centroid)     ##### target:centroid ***, centroid *** 
anova(m0_ob_joy, m3_joy_centroid)
anova(m2_joy, m3_joy_centroid)


# interaction visualization

x_rms_joy <- as.data.frame(effects::effect(term= "target*rms", mod = m3_joy_rms, KR = T))

ggplot(data=x_rms_joy, aes(target, color=rms)) + 
  geom_point(data=joy_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=rms), size=1.2) +
  geom_line(aes(y=lower, group=rms), linetype=3) +
  geom_line(aes(y=upper, group=rms), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))

x_centroid_joy <- as.data.frame(effects::effect(term= "target*centroid", mod = m3_joy_centroid, KR = T))

ggplot(data=x_centroid_joy, aes(target, color=centroid)) + 
  geom_point(data=joy_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=centroid), size=1.2) +
  geom_line(aes(y=lower, group=centroid), linetype=3) +
  geom_line(aes(y=upper, group=centroid), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))

x_rolloff_joy <- as.data.frame(effects::effect(term= "target*rolloff", mod = m3_joy_rolloff, KR = T))

ggplot(data=x_rolloff_joy, aes(target, color=rolloff)) + 
  geom_point(data=joy_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=rolloff), size=1.2) +
  geom_line(aes(y=lower, group=rolloff), linetype=3) +
  geom_line(aes(y=upper, group=rolloff), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))


################################################################################################################
################################################################################################################
######## sad stimuli
################################################################################################################
################################################################################################################

sad_stimuli <- data.frame(time = c(), target = c(), observer = c(), idstimuli = c(), 
                          rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())


for (i in 1:nrow(all_stimuli)){
  
  if (grepl("sad", all_stimuli[i, "idstimuli"])){
    
    sad_stimuli <- rbind(sad_stimuli, all_stimuli[i,])
    
  }
}

################################################################################################################
#### observer ~ MIR features
#### target ~ MIR features
################################################################################################################

## observer
# zero model
m0_ob_sad <- lmer(observer ~ (1 | idstimuli), data = sad_stimuli, REML = F,
                  control = lmerControl(optimizer ='bobyqa'))
summary(m0_ob_sad)

# icc_ob_sad <- 0.7093 / (0.7093 + 0.6133); icc_ob_sad

# lmer
ob_lmer_sad <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = sad_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))
ob_lmer_sad2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli), data = sad_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))

summary(ob_lmer_sad) ##### result: rms ***
summary(ob_lmer_sad2) ##### result: target ***, rms ***

anova(m0_ob_sad, ob_lmer_sad)
anova(ob_lmer_sad, ob_lmer_sad2)

## target
# zero model
m0_tg_sad <- lmer(target ~ (1 | idstimuli), data = sad_stimuli, REML = F,
                  control = lmerControl(optimizer ='bobyqa'))
summary(m0_tg_sad)

# icc_tg_sad <- 0.2366 / (0.2366 + 0.6787); icc_tg_sad

# lmer
tg_lmer_sad <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = sad_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))
summary(tg_lmer_sad) ##### result: rms*, flatness (.)

anova(m0_tg_sad, tg_lmer_sad)


#### LME model

m1_sad <- lmer(observer ~ target + (1 | idstimuli), data = sad_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))
m2_sad <- lmer(observer ~ target + (1 + target | idstimuli), data = sad_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))

summary(m1_sad)
summary(m2_sad)
anova(m1_sad, m2_sad)
anova(m0_ob_sad, m2_sad)

sjPlot::tab_model(m2_sad, df.method = "satterthwaite", show.df = TRUE)

# visualization 
x_target_sad <- as.data.frame(effects::effect(term= "target", mod= m2_sad))

ggplot() + 
  geom_point(data=sad_stimuli, aes(target, observer)) + 
  geom_point(data=x_target_sad, aes(x=target, y=fit), color="blue") +
  geom_line(data=x_target_sad, aes(x=target, y=fit), color="blue") +
  geom_ribbon(data=x_target_sad, aes(x=target, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))


# interaction
m3_sad_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli), data = sad_stimuli, REML = F,
                   control = lmerControl(optimizer ='bobyqa'))
summary(m3_sad_rms) ### target *
anova(m0_ob_sad, m3_sad_rms)
anova(m2_sad, m3_sad_rms)


# interaction visualization
x_rms_sad <- as.data.frame(effects::effect(term= "target*rms", mod = m3_sad_rms, KR = T))

ggplot(data=x_rms_sad, aes(target, color=rms)) + 
  geom_point(data=sad_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=rms), size=1.2) +
  geom_line(aes(y=lower, group=rms), linetype=3) +
  geom_line(aes(y=upper, group=rms), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))


# x_zerocross_sad <- as.data.frame(effects::effect(term= "target*zerocross", mod = m3_sad_zerocross, KR = T))
# 
# ggplot(data=x_zerocross_sad, aes(target, color=zerocross)) + 
#   geom_point(data=sad_stimuli, aes(target, observer), color='gray') +
#   geom_line(aes(y=fit, group=zerocross), size=1.2) +
#   geom_line(aes(y=lower, group=zerocross), linetype=3) +
#   geom_line(aes(y=upper, group=zerocross), linetype=3) +
#   labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
#   scale_x_continuous(limits=c(1,9)) + 
#   scale_y_continuous(limits=c(1,9))



###############################################################################################################
################################################################################################################
######## anger stimuli
################################################################################################################
################################################################################################################

anger_stimuli <- data.frame(time = c(), target = c(), observer = c(), idstimuli = c(), 
                          rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())


for (i in 1:nrow(all_stimuli)){
  
  if (grepl("anger", all_stimuli[i, "idstimuli"])){
    
    anger_stimuli <- rbind(anger_stimuli, all_stimuli[i,])
    
  }
}

################################################################################################################
#### observer ~ MIR features
#### target ~ MIR features
################################################################################################################

## observer
# zero model
m0_ob_anger <- lmer(observer ~ (1 | idstimuli), data = anger_stimuli, REML = F,
                  control = lmerControl(optimizer ='bobyqa'))
summary(m0_ob_anger)

# icc_ob_anger <- 0.3640 / (0.3640 + 0.7753); icc_ob_anger

# lmer
ob_lmer_anger <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = anger_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))
ob_lmer_anger2 <- lmer(observer ~ target + rms + flatness + zerocross + centroid + (1 | idstimuli), data = anger_stimuli,
                      REML = F, control = lmerControl(optimizer ='bobyqa'))

summary(ob_lmer_anger) ##### result: rms ***, zerocross *
summary(ob_lmer_anger2) ##### result: target***, rms ***, zerocross *

anova(m0_ob_anger, ob_lmer_anger)
anova(ob_lmer_anger, ob_lmer_anger2)

## target
# zero model
m0_tg_anger <- lmer(target ~ (1 | idstimuli), data = anger_stimuli, REML = F,
                  control = lmerControl(optimizer ='bobyqa'))
summary(m0_tg_anger)

# icc_tg_anger <- 0.2196 / (0.2196 + 1.2064); icc_tg_anger

# lmer
tg_lmer_anger <- lmer(target ~ rms + flatness + zerocross + centroid + (1 | idstimuli), data = anger_stimuli,
                    REML = F, control = lmerControl(optimizer ='bobyqa'))
summary(tg_lmer_anger) ##### result: rms *, flatness (.), centroid ***

anova(m0_tg_anger, tg_lmer_anger)


#### LME model

m1_anger <- lmer(observer ~ target + (1 | idstimuli), data = anger_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))
m2_anger <- lmer(observer ~ target + (1 + target | idstimuli), data = anger_stimuli, REML = F,
               control = lmerControl(optimizer ='bobyqa'))

summary(m1_anger)
summary(m2_anger)
anova(m1_anger, m2_anger)
anova(m0_ob_anger, m2_anger)

sjPlot::tab_model(m2_anger)

# visualization
x_target_anger <- as.data.frame(effects::effect(term= "target", mod= m2_anger))

ggplot() + 
  geom_point(data=anger_stimuli, aes(target, observer)) + 
  geom_point(data=x_target_anger, aes(x=target, y=fit), color="blue") +
  geom_line(data=x_target_anger, aes(x=target, y=fit), color="blue") +
  geom_ribbon(data=x_target_anger, aes(x=target, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))

# interaction

m3_anger_rms <- lmer(observer ~ target + rms + target:rms + (1 + target | idstimuli), data = anger_stimuli, REML = F,
                   control = lmerControl(optimizer ='bobyqa'))
summary(m3_anger_rms)  ##### target:rms ***, target **
anova(m0_ob_anger, m3_anger_rms)
anova(m2_anger, m3_anger_rms)

m3_anger_centroid <- lmer(observer ~ target + centroid + target:centroid + (1 + target | idstimuli), data = anger_stimuli, REML = F,
                          control = lmerControl(optimizer ='bobyqa'))
summary(m3_anger_centroid)   ##### target:centroid (.), centroid (.), target **
anova(m0_ob_anger, m3_anger_centroid)
anova(m2_anger, m3_anger_centroid)     #####ta NO SIGNIFICANT RESULTS (p = .222)

sjPlot::tab_model(m3_anger_centroid)

m3_anger_zerocross <- lmer(observer ~ target + zerocross + target:zerocross + (1 + target | idstimuli), data = anger_stimuli, REML = F,
                          control = lmerControl(optimizer ='bobyqa'))
summary(m3_anger_zerocross)   ##### target:zerocross ***, zerocross **
anova(m0_ob_anger, m3_anger_zerocross)
anova(m2_anger, m3_anger_zerocross)   


# interaction visualization

x_rms_anger <- as.data.frame(effects::effect(term= "target*rms", mod = m3_anger_rms, KR = T))

ggplot(data=x_rms_anger, aes(target, color=rms)) + 
  geom_point(data=anger_stimuli, aes(target, observer), color='gray') +
  geom_line(aes(y=fit, group=rms), size=1.2) +
  geom_line(aes(y=lower, group=rms), linetype=3) +
  geom_line(aes(y=upper, group=rms), linetype=3) +
  labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
  scale_x_continuous(limits=c(1,9)) + 
  scale_y_continuous(limits=c(1,9))
 
# x_rolloff_anger <- as.data.frame(effects::effect(term= "target*rolloff", mod = m3_anger_rolloff, KR = T))
# 
# ggplot(data=x_rolloff_anger, aes(target, color=rolloff)) + 
#   geom_point(data=anger_stimuli, aes(target, observer), color='gray') +
#   geom_line(aes(y=fit, group=rolloff), size=1.2) +
#   geom_line(aes(y=lower, group=rolloff), linetype=3) +
#   geom_line(aes(y=upper, group=rolloff), linetype=3) +
#   labs(x="Targets' Expressed Emotions", y="Listeners' Perceived Emotions") +
#   scale_x_continuous(limits=c(1,9)) + 
#   scale_y_continuous(limits=c(1,9))

################################################################################################################
##### Targets' Discrete ratings LME models
################################################################################################################

df_dis <- data.frame(idstimuli = c(), arousal = c(), valence = c(), dominance = c(), observer = c(),
                     rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())

df_t$idstimuli <- paste(df_t$id, '_', df_t$emotion)
df_t$idstimuli <- gsub("\\s+", "", df_t$idstimuli)
df_t$idstimuli <- as.factor(df_t$idstimuli)

ct <- 0
for (ids in unique(as.character(all_stimuli$idstimuli))){
  
    ct <- ct + 1
   
    df_dis[ct,"idstimuli"] <- ids
    df_dis[ct,"arousal_d"] <- df_t[df_t$idstimuli==ids,"arousal"]
    df_dis[ct,"valence_d"] <- df_t[df_t$idstimuli==ids,"valence"]
    df_dis[ct,"dominance_d"] <- df_t[df_t$idstimuli==ids,"dominance"]
    
    df_dis[ct,"valence_c"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "target"])
   
    df_dis[ct,"observer"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "observer"])
   
    df_dis[ct,"rms"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "rms"])
    df_dis[ct,"flatness"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "flatness"])
    df_dis[ct,"zerocross"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "zerocross"])
    df_dis[ct,"centroid"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "centroid"])
    df_dis[ct,"rolloff"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "rolloff"])

}


cor(df_dis$valence_c, df_dis$valence_d)


## observers' valence
# zero model
m0_ob_dis <- lm(observer ~ 1, data = df_dis)
summary(m0_ob_dis)

# lm
lm_ob_dis <- lm(observer ~ rms + flatness + zerocross + centroid, data = df_dis)
summary(lm_ob_dis) ##### result: rms *, R^2 = 0.229, p = 0.149

vif(lm_ob_dis)

### individual observers

df_ob_dis <- data.frame(idstimuli = c(), arousal = c(), valence = c(), dominance = c(), observer = c(),
                     rms = c(), flatness = c(), zerocross = c(), centroid = c(), rolloff = c())

ct <- 0

for (ids in unique(as.character(all_stimuli$idstimuli))){

  ob <- get(ids)
  
  for (j in 2:13){
    
    ct <- ct + 1
    
    df_ob_dis[ct,"idstimuli"] <- ids
    df_ob_dis[ct,"arousal_d"] <- df_t[df_t$idstimuli==ids,"arousal"]
    df_ob_dis[ct,"valence_d"] <- df_t[df_t$idstimuli==ids,"valence"]
    df_ob_dis[ct,"dominance_d"] <- df_t[df_t$idstimuli==ids,"dominance"]
    
    df_ob_dis[ct,"valence_c"] <-  mean(all_stimuli[all_stimuli$idstimuli==ids, "target"])
    
    df_ob_dis[ct, "observer"] <- mean(ob[,j])
    df_ob_dis[ct, "idob"] <- names(ob)[j]
    
    df_ob_dis[ct,"rms"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "rms"])
    df_ob_dis[ct,"flatness"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "flatness"])
    df_ob_dis[ct,"zerocross"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "zerocross"])
    df_ob_dis[ct,"centroid"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "centroid"])
    df_ob_dis[ct,"rolloff"] <- mean(all_stimuli[all_stimuli$idstimuli==ids, "rolloff"])
    
    
  }
}

########################
## observers' valence ##
########################

df_ob_dis$idob <- as.factor(df_ob_dis$idob)

# zero model
m0_ob_dis2 <- lmer(observer ~ (1 | idob), data = df_ob_dis)
summary(m0_ob_dis2)

# lmer predictors: targets' discrete ratings (AVD) + MIR features (5)
lmer_ob_dis2 <- lmer(observer ~ arousal_d + valence_d + dominance_d + rms + flatness + zerocross + centroid + (1 | idob), data = df_ob_dis)
summary(lmer_ob_dis2) ##### arousal **, valence ***

vif(lmer_ob_dis2)

anova(m0_ob_dis2, lmer_ob_dis2)

# lmer predictors: MIR features (4)
lmer_ob_dis3 <- lmer(observer ~ rms + flatness + zerocross + centroid + (1 | idob), data = df_ob_dis)
summary(lmer_ob_dis3) ##### all significant

anova(m0_ob_dis2, lmer_ob_dis3)

anova(lmer_ob_dis2, lmer_ob_dis3)



sjPlot::plot_model(lmer_ob_dis2,
                   # show.values=T, show.p=T,
                   value.offset = 0.5, title = "",
                   axis.labels = c(
                     "Spectral Centroid", "Zero-crossing", "Flatness", "RMS",
                     "Dominance", "Valence", "Arousal"),
                   vline.color = "#061423", dot.size = 8, line.size = 2) +
  theme(text = element_text(size = 25, face = "bold"), ) +
  scale_color_sjplot("dust")

sjPlot::plot_model(lmer_ob_dis3,
                   # show.values=T, show.p=T,
                   value.offset = 0.5, title = "",
                   axis.labels = c(
                     "Spectral Centroid", "Zero-crossing", "Flatness", "RMS"),
                   vline.color = "#061423", dot.size = 8, line.size = 2) +
  theme(text = element_text(size = 25, face = "bold")) +
  scale_color_sjplot("dust")



#############cross validation############################################################
#########################################################################################
library(lme4)
library(dplyr)
library(Metrics)

# leave-one-out 교차 검증을 위한 함수 정의
leave_one_out_cv <- function(data, model_formula1, model_formula2, id_col, response_var) {
  unique_ids <- unique(data[[id_col]])
  results <- data.frame(idstimuli = character(),
                        model1_mae = numeric(),
                        model2_mae = numeric(),
                        model1_mse = numeric(),
                        model2_mse = numeric(),
                        model1_rmse = numeric(),
                        model2_rmse = numeric(),
                        model1_mape = numeric(),
                        model2_mape = numeric(),
                        stringsAsFactors = FALSE)
  
  for (leave_out_id in unique_ids) {
    # Leave-one-out 데이터셋 생성
    train_data <- data %>% filter(data[[id_col]] != leave_out_id)
    test_data <- data %>% filter(data[[id_col]] == leave_out_id)
    
    # 모델 피팅
    model1 <- lmer(model_formula1, data = train_data)
    model2 <- lmer(model_formula2, data = train_data)
    
    # 예측값 생성
    predictions_model1 <- predict(model1, newdata = test_data, allow.new.levels = TRUE)
    predictions_model2 <- predict(model2, newdata = test_data, allow.new.levels = TRUE)
    
    # 실제값
    actuals <- test_data[[response_var]]
    
    # 성능 지표 계산
    model1_mae <- mae(actuals, predictions_model1)
    model2_mae <- mae(actuals, predictions_model2)
    
    model1_mse <- mse(actuals, predictions_model1)
    model2_mse <- mse(actuals, predictions_model2)
    
    model1_rmse <- rmse(actuals, predictions_model1)
    model2_rmse <- rmse(actuals, predictions_model2)
    
    model1_mape <- mape(actuals, predictions_model1)
    model2_mape <- mape(actuals, predictions_model2)
    
    # 결과 저장
    results <- rbind(results, data.frame(idstimuli = leave_out_id,
                                         model1_mae = model1_mae,
                                         model2_mae = model2_mae,
                                         model1_mse = model1_mse,
                                         model2_mse = model2_mse,
                                         model1_rmse = model1_rmse,
                                         model2_rmse = model2_rmse,
                                         model1_mape = model1_mape,
                                         model2_mape = model2_mape,
                                         stringsAsFactors = FALSE))
  }
  return(results)
}

# 모델 공식 정의
model_formula1 <- observer ~ arousal_d + valence_d + dominance_d + rms + flatness + zerocross + centroid + (1 | idob)
model_formula2 <- observer ~ rms + flatness + zerocross + centroid + (1 | idob)

# leave-one-out 교차 검증 수행
results <- leave_one_out_cv(df_ob_dis, model_formula1, model_formula2, "idstimuli", "observer")

# 결과 확인
print(results)

# Wilcoxon signed-rank test로 모델 성능 비교
wilcox_test_results <- list(
  mae = wilcox.test(results$model1_mae, results$model2_mae, paired = TRUE),
  mse = wilcox.test(results$model1_mse, results$model2_mse, paired = TRUE),
  rmse = wilcox.test(results$model1_rmse, results$model2_rmse, paired = TRUE),
  mape = wilcox.test(results$model1_mape, results$model2_mape, paired = TRUE)
)

# 각 지표에 대한 평균 계산
mean_results <- results %>%
  summarise(
    mean_model1_mae = mean(model1_mae),
    mean_model2_mae = mean(model2_mae),
    mean_model1_mse = mean(model1_mse),
    mean_model2_mse = mean(model2_mse),
    mean_model1_rmse = mean(model1_rmse),
    mean_model2_rmse = mean(model2_rmse),
    mean_model1_mape = mean(model1_mape),
    mean_model2_mape = mean(model2_mape)
  )

# 결과를 보기 쉽게 정리
results_summary <- data.frame(
  Metric = c("MAE", "MSE", "RMSE", "MAPE"),
  Model1 = c(mean_results$mean_model1_mae, mean_results$mean_model1_mse, mean_results$mean_model1_rmse, mean_results$mean_model1_mape),
  Model2 = c(mean_results$mean_model2_mae, mean_results$mean_model2_mse, mean_results$mean_model2_rmse, mean_results$mean_model2_mape),
  PValue = c(wilcox_test_results$mae$p.value, wilcox_test_results$mse$p.value, wilcox_test_results$rmse$p.value, wilcox_test_results$mape$p.value)
)

# 결과 출력
print("Wilcoxon signed-rank test results with means:")
print(results_summary)
##########################################################################################
##########################################################################################
