# This script runs all analyses used in `animal-sounds.Rmd`
# All code was written by Logan James and reviewed by Courtney Hilton
# 17 October 2025

#################
## LIBRARIES ####
#################
library(plyr)
library(Rmisc)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(gridExtra)
library(maps)
library(cowplot)
library(lme4)
library(here)
library(tidyr)
library(readr)
library(lmerTest)
library(dispRity)
library(RColorBrewer)
library(car)
library(emmeans)
library(MCMCglmm)
library(glmmTMB)
library(ggpubr)
library(cowplot)
library(gridGraphics)
library(patchwork)
library(ggplotify)
library(readxl)

# set colors for figures
color_bird<-"#66499D"
color_frog<-"#40AC9A"
color_insect<-"#88AC42"
color_mammal<-"#AB4253"

# load data
FullTest<-read_csv(here::here("data", "animal-sounds-data.csv"))

# preference strength correlations
Strength<-summarySE(FullTest, measurevar="correct", groupvars=c("StimID","Species","Category","AnimalStrength","Trait"))

F1A<-ggplot(data=subset(Strength,!is.na(AnimalStrength)),aes(y=correct*100, x=AnimalStrength)) + 
  geom_smooth(method="lm",se=TRUE,color="black")+
  geom_point(aes(color=Category),size=2,alpha=0.5) +
  geom_smooth(method="lm",data=subset(Strength,AnimalStrength>66))+
  geom_smooth(method="lm",data=subset(Strength,AnimalStrength>=75))+
  geom_hline(yintercept=50, linetype="dashed", color = "black") +
  scale_color_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        aspect.ratio = 1,
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position="none"
  )

strength_model<-lmer(correct~AnimalStrength+(1|Species),data=Strength)
strength_anova<-anova(strength_model)

strength_model2<-lmer(correct~AnimalStrength+(1|Species),data=subset(Strength,AnimalStrength>66))
strength_anova2<-anova(strength_model2)

#############################
## Main analyses (Fig 1) ####
#############################
# main models
FullTest$user_id<-as.factor(FullTest$user_id)
FullModel_weakAnimal<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=FullTest)
FullModel_weakAnimal_summary<-summary(FullModel_weakAnimal)

FullModel_strongestAnimal<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>=75))
FullModel_strongestAnimal_summary<-summary(FullModel_strongestAnimal)

FullModel<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66))
FullModel_summary<-summary(FullModel)

FullModel_Category<-glmer(correct ~ Category + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66))
FullModel_Category_Anova<-Anova(FullModel_Category)

meanPref_weakAnimal<-mean(FullTest$correct)*100
meanPref_weakAnimalSE<-(sd(FullTest$correct)/sqrt(nrow(FullTest)))*100
meanPref_weakAnimalCI<-1.96*meanPref_weakAnimalSE

meanPref<-mean(subset(FullTest,AnimalStrength>66)$correct)*100
meanPref_SE<-(sd(subset(FullTest,AnimalStrength>66)$correct)/sqrt(nrow(subset(FullTest,AnimalStrength>66))))*100
meanPref_CI<-1.96*meanPref_SE

meanPref_strongAnimal<-mean(subset(FullTest,AnimalStrength>=75)$correct)*100
meanPref_strongAnimalSE<-(sd(subset(FullTest,AnimalStrength>=75)$correct)/sqrt(nrow(subset(FullTest,AnimalStrength>=75))))*100
meanPref_strongAnimalCI<-1.96*meanPref_strongAnimalSE

# Averaging within each stimulus pair for plotting
stimSummary<-summarySE(FullTest, measurevar="correct", groupvars=c("StimID","Species","Category","AnimalStrength"))
stimSummary$dummy<-1 # dummy variable is simply for plotting
stimSummary2<-summarySE(subset(stimSummary,AnimalStrength>66), measurevar="correct", groupvars=c("Species","Category"))
stimSummary3<-summarySE(stimSummary2, measurevar="correct", groupvars=c())
stimSummary3$dummy<-1
stimSummary<-mutate(stimSummary,
                    strengthCat=if_else(AnimalStrength>=75,"High",if_else(AnimalStrength>66,"Medium","Small")))

F1B3<-ggplot(data=subset(stimSummary,AnimalStrength>66),aes(x=dummy, y=correct)) + 
  geom_violin(adjust=0.75)+
  geom_jitter(data=subset(stimSummary,!is.na(strengthCat)),aes(color=strengthCat),shape=16,width=0.2,height=0,alpha=0.5) +
  geom_hline(yintercept=0.5,size=1, linetype="dashed", color = "black") +
  geom_errorbar(data=stimSummary3,aes(ymin=correct-ci, ymax=correct+ci),width=.1) +
  ylim(0.05,0.95)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 12),
        axis.title.x = element_blank(),
        aspect.ratio = 8,
        legend.position="none"
  )

stimSummary$Species<-factor(stimSummary$Species,levels=c("Gelada","Singing mouse","Zebra finch","Song sparrow","Swamp sparrow","Canary","Tungara frog","Green tree frog","Cope gray treefrog","Gray treefrog","Hourglass treefrog","Colorado dwarf frog","Midwife toad","Pacific field cricket","Bow-winged grasshopper","Fruitfly"))

F1B2<-ggplot(stimSummary, aes(x=Species, y=correct, fill=Category, color=Category)) + 
  geom_pointrange(aes(ymin = correct-ci, ymax = correct+ci), 
                  position=position_jitter(width=0.25,height=0),size=.3,) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  scale_fill_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  scale_color_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  ylim(0.05,0.95)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        axis.title = element_text(size = 12),
        legend.position="none"
  )

# decision time analysis
Reaction<-subset(FullTest,rt<5000&AnimalStrength>66)
Reaction$sqrt_rt<-sqrt(Reaction$rt)
Reaction<-mutate(Reaction,zsqrt_rt=(sqrt_rt - mean(sqrt_rt))/sd(sqrt_rt))

rt_model<-glmer(correct ~ zsqrt_rt+(1|StimID)+(1|Species)+(1|user_id),family=binomial,data=Reaction,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
rt_summary<-summary(rt_model)
rt_anova<-Anova(rt_model)

rtStimSum2<-summarySE(data=Reaction,measurevar="sqrt_rt",groupvars=c("correct"))
F1C<-ggplot(data=rtStimSum2,aes(x=as.factor(correct),y=sqrt_rt^2))+
  geom_point()+
  geom_errorbar(data=rtStimSum2,aes(ymin=(sqrt_rt-(se*1.96))^2, ymax=(sqrt_rt+(se*1.96))^2),width=.1,) +
  scale_fill_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  scale_color_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        aspect.ratio = 2,
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
  )

meanCorrect<-mean(subset(Reaction,correct==1)$sqrt_rt)^2
meanIncorrect<-mean(subset(Reaction,correct==0)$sqrt_rt)^2
rtDiff<-meanIncorrect-meanCorrect


# intra-rater reliability
# find examples of participants that recieved the same stimulus pair twice
duplicates <- FullTest %>%
  subset(AnimalStrength>66) %>%
  mutate(id_stim=paste(user_id,StimID)) %>%
  group_by(id_stim) %>%
  filter(n() > 1) %>%
  ungroup()

# average within the duplicates
dupe_summary<-summarySE(duplicates, measurevar="correct", groupvars=c("StimID","user_id","Species","Category"))
dupe_summary3<-subset(dupe_summary,N==2) # just include cases where the user got the stim twice for simplicity
dupe_summary3<-mutate(dupe_summary3,agreement=if_else(correct==0.5,0,1))
dupe_summary4<-summarySE(dupe_summary3, measurevar="agreement", groupvars=c("StimID","Species","Category"))
dupe_summary4$dummy<-1

F1D<-ggplot(dupe_summary4, aes(x=dummy, y=agreement)) + 
  geom_jitter(aes(color=Category),shape=16,width=0.2,height=0,size=1.5,alpha=0.5) +
  geom_boxplot(outlier.shape=NA,alpha=0.2)+
  #geom_pointrange(aes(ymin = correct2-se, ymax = correct2+se, color=Category),position=position_jitter(width=0.2,height=0),size=.3) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  scale_fill_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  scale_color_manual(values=c(color_bird,color_frog,color_insect,color_mammal),name="category") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        axis.title = element_text(size = 12),
        aspect.ratio=2,
        legend.position="none"
  )

intra_model<-lm(agreement-0.5 ~ 1 ,data=dupe_summary4) # are choices different from 50%?
intra_summary<-summary(intra_model)

intra_meanAgree<-mean(dupe_summary4$agreement)

##################################
## Secondary analyses (Fig 2) ####
##################################
#  analysis of different stimulus traits
All_Sum1<-summarySE(data=FullTest,measurevar="correct",groupvars=c("Species","Trait","StimID","Category","AnimalStrength"))
traitList<-c("Frequency","Rate","Rhythm","AmplitudeModulation","Adornment","Context","Development","Manipulated","Individuals","Complexity")
All_Sum1$Trait<-factor(All_Sum1$Trait,levels=traitList)
F2A<-ggplot(data=subset(All_Sum1,AnimalStrength>66),aes(y=correct,x=Trait))+
  geom_pointrange(aes(ymin = correct-ci, ymax = correct+ci, color=Category), 
                  position=position_jitter(width=0.20,height=0),size=.4) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "blue") +
  scale_fill_manual(values=c(color_bird,color_frog,color_insect,color_mammal)) +
  scale_color_manual(values=c(color_bird,color_frog,color_insect,color_mammal)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        legend.position="none")

traitModel<-glmer(correct ~ Trait + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66))
traitAnova<-Anova(traitModel)
## this model does not converge. LMER gives same result
Anova(lmer(correct ~ Trait + (1|StimID) + (1|Species) + (1|user_id),data=subset(FullTest,AnimalStrength>66)))

freqModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Frequency")))
rateModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Rate")))
AMModel<-summary(glmer(correct ~ 1 + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="AmplitudeModulation")))
adornModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Adornment")))
contextModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Context")))
individualsModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Individuals")))
complexModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Complexity")))
devModel<-summary(glmer(correct ~ 1 + (1|StimID) + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Development")))
manModel<-summary(glmer(correct ~ 1 + (1|user_id), family = binomial('logit'),data=subset(FullTest,AnimalStrength>66&Trait=="Manipulated")))

nonsig_trait_pList<-c(rateModel$coefficients[4],AMModel$coefficients[4],contextModel$coefficients[4],manModel$coefficients[4])

# analysis of spectral features
Spec<-summarySE(subset(FullTest,AnimalStrength>66), measurevar="correct", groupvars=c("StimID","Species","Category","GoodAudio","BadAudio","Trait"))
Features<-read_excel(here::here("data","acoustic-features.xlsx")) %>%
  mutate(
    zDF=(DF-mean(DF,na.rm=TRUE))/sd(DF,na.rm=TRUE),
    zDur=(Duration-mean(Duration,na.rm=TRUE))/sd(Duration,na.rm=TRUE),
    zPitch=(Pitch-mean(Pitch,na.rm=TRUE))/sd(Pitch,na.rm=TRUE),
    zAMvar=(AMVariance-mean(AMVariance,na.rm=TRUE))/sd(AMVariance,na.rm=TRUE),
    zCent=(SpectralCentroid-mean(SpectralCentroid,na.rm=TRUE))/sd(SpectralCentroid,na.rm=TRUE),
    zEnt=(ShannonEntropy-mean(ShannonEntropy,na.rm=TRUE))/sd(ShannonEntropy,na.rm=TRUE),
    zRough=(Roughness-mean(Roughness,na.rm=TRUE))/sd(Roughness,na.rm=TRUE),
    zAttack=(AttackSlope-mean(AttackSlope,na.rm=TRUE))/sd(AttackSlope,na.rm=TRUE),
    zRMS=(rmsEnergy-mean(rmsEnergy,na.rm=TRUE))/sd(rmsEnergy,na.rm=TRUE),
    zBright=(Brightness-mean(Brightness,na.rm=TRUE))/sd(Brightness,na.rm=TRUE),
    zSkew=(SpectralSkewness-mean(SpectralSkewness,na.rm=TRUE))/sd(SpectralSkewness,na.rm=TRUE),
    zSpread=(SpectralSpread-mean(SpectralSpread,na.rm=TRUE))/sd(SpectralSpread,na.rm=TRUE)
  )

goodFeat<-Features
colnames(goodFeat) <- paste(colnames(goodFeat), "_good",sep="")
names(goodFeat)[1] <- "GoodAudio"

badFeat<-Features
colnames(badFeat) <- paste(colnames(badFeat), "_bad",sep="")
names(badFeat)[1] <- "BadAudio"

Spec<-left_join(Spec,goodFeat,by="GoodAudio")
Spec<-left_join(Spec,badFeat,by="BadAudio")

Spec$dummy<-1

AcousticDF<-data.frame()
acousticTable<-data.frame()

##############
############## DF 
##############
Spec$zDFDiff<-Spec$zDF_good-Spec$zDF_bad
hist(Spec$zDFDiff,breaks=20) # points below -2 are outliers
DF_NAs<-nrow(subset(Spec,is.na(zDFDiff)))
DF_nRemoved<-nrow(Spec)-nrow(subset(Spec,zDFDiff>-3))

model<-lmer(correct~zDFDiff+(1|Species),data=subset(Spec,zDFDiff>-3))
acousticTable<-rbind(acousticTable,data.frame(feature="DF",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="DF",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_DF_good=if_else(correct>0.5,zDF_good,zDF_bad),
             zhuman_DF_bad=if_else(correct>0.5,zDF_bad,zDF_good),
             zDF_corr=if_else(zDFDiff<0,0.5+(0.5-correct),correct),
             zDFDiff_human=if_else(correct>0.5,zDFDiff,-zDFDiff)
)

tmp_animal<-Spec %>%
  subset(zDFDiff>-3) %>%
  dplyr::select(StimID,Species,zDF_good,zDF_bad) |>
  pivot_longer(cols=c(zDF_good,zDF_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="DF",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="DF",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zDFDiff>-3) %>%
  dplyr::select(StimID,Species,zhuman_DF_good,zhuman_DF_bad) |>
  pivot_longer(cols=c(zhuman_DF_good,zhuman_DF_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="DF",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="DF",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## DURATION
##############
Spec$zDurDiff<-Spec$zDur_good-Spec$zDur_bad
hist(Spec$zDurDiff,breaks=20)
Dur_NAs<-nrow(subset(Spec,is.na(zDurDiff)))

model<-lmer(correct~zDurDiff+(1|Species),data=subset(Spec,zDurDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Duration",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Duration",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Dur_good=if_else(correct>0.5,zDur_good,zDur_bad),
             zhuman_Dur_bad=if_else(correct>0.5,zDur_bad,zDur_good),
             zDur_corr=if_else(zDurDiff<0,0.5+(0.5-correct),correct),
             zDurDiff_human=if_else(correct>0.5,zDurDiff,-zDurDiff),
)

tmp_animal<-Spec %>%
  subset(zDurDiff!=100) %>%
  dplyr::select(StimID,Species,zDur_good,zDur_bad) |>
  pivot_longer(cols=c(zDur_good,zDur_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Duration",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Duration",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zDurDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Dur_good,zhuman_Dur_bad) |>
  pivot_longer(cols=c(zhuman_Dur_good,zhuman_Dur_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Duration",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Duration",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## PITCH
##############
Spec$zPitchDiff<-Spec$zPitch_good-Spec$zPitch_bad
hist(Spec$zPitchDiff,breaks=20)
Pitch_NAs<-nrow(subset(Spec,is.na(zPitchDiff)))

model<-lmer(correct~zPitchDiff+(1|Species),data=subset(Spec,zPitchDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Pitch",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Pitch",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Pitch_good=if_else(correct>0.5,zPitch_good,zPitch_bad),
             zhuman_Pitch_bad=if_else(correct>0.5,zPitch_bad,zPitch_good),
             zPitch_corr=if_else(zPitchDiff<0,0.5+(0.5-correct),correct),
             zPitchDiff_human=if_else(correct>0.5,zPitchDiff,-zPitchDiff),
)

tmp_animal<-Spec %>%
  subset(zPitchDiff!=100) %>%
  dplyr::select(StimID,Species,zPitch_good,zPitch_bad) |>
  pivot_longer(cols=c(zPitch_good,zPitch_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Pitch",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Pitch",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zPitchDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Pitch_good,zhuman_Pitch_bad) |>
  pivot_longer(cols=c(zhuman_Pitch_good,zhuman_Pitch_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Pitch",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Pitch",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## AMVariance  
##############
Spec$zAMvarDiff<-Spec$zAMvar_good-Spec$zAMvar_bad
hist(Spec$zAMvarDiff,breaks=20)
AMvar_NAs<-nrow(subset(Spec,is.na(zAMvarDiff)))

model<-lmer(correct~zAMvarDiff+(1|Species),data=subset(Spec,zAMvarDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="AMvar",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="AMvar",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_AMvar_good=if_else(correct>0.5,zAMvar_good,zAMvar_bad),
             zhuman_AMvar_bad=if_else(correct>0.5,zAMvar_bad,zAMvar_good),
             zAMvar_corr=if_else(zAMvarDiff<0,0.5+(0.5-correct),correct),
             zAMvarDiff_human=if_else(correct>0.5,zAMvarDiff,-zAMvarDiff),
)

tmp_animal<-Spec %>%
  subset(zAMvarDiff!=100) %>%
  dplyr::select(StimID,Species,zAMvar_good,zAMvar_bad) |>
  pivot_longer(cols=c(zAMvar_good,zAMvar_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="AMvar",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="AMvar",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zAMvarDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_AMvar_good,zhuman_AMvar_bad) |>
  pivot_longer(cols=c(zhuman_AMvar_good,zhuman_AMvar_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="AMvar",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="AMvar",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


##############
############## SPECTRALCENTROID
##############

Spec$zCentDiff<-Spec$zCent_good-Spec$zCent_bad
hist(Spec$zCentDiff,breaks=20)
Cent_NAs<-nrow(subset(Spec,is.na(zCentDiff)))
Cent_nRemoved<-nrow(subset(Spec,!is.na(zCentDiff)))-nrow(subset(Spec,zCentDiff>-0.5))

model<-lmer(correct~zCentDiff+(1|Species),data=subset(Spec,zCentDiff>-0.5))
acousticTable<-rbind(acousticTable,data.frame(feature="Cent",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Cent",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Cent_good=if_else(correct>0.5,zCent_good,zCent_bad),
             zhuman_Cent_bad=if_else(correct>0.5,zCent_bad,zCent_good),
             zCent_corr=if_else(zCentDiff<0,0.5+(0.5-correct),correct),
             zCentDiff_human=if_else(correct>0.5,zCentDiff,-zCentDiff),
)

tmp_animal<-Spec %>%
  subset(zCentDiff>-0.5) %>%
  dplyr::select(StimID,Species,zCent_good,zCent_bad) |>
  pivot_longer(cols=c(zCent_good,zCent_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Cent",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Cent",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zCentDiff>-0.5) %>%
  dplyr::select(StimID,Species,zhuman_Cent_good,zhuman_Cent_bad) |>
  pivot_longer(cols=c(zhuman_Cent_good,zhuman_Cent_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Cent",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Cent",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## SKEW 
##############
Spec$zSkewDiff<-Spec$zSkew_good-Spec$zSkew_bad
hist(Spec$zSkewDiff,breaks=20)
Skew_NAs<-nrow(subset(Spec,is.na(zSkewDiff)))

model<-lmer(correct~zSkewDiff+(1|Species),data=subset(Spec,zSkewDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Skew",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Skew",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Skew_good=if_else(correct>0.5,zSkew_good,zSkew_bad),
             zhuman_Skew_bad=if_else(correct>0.5,zSkew_bad,zSkew_good),
             zSkew_corr=if_else(zSkewDiff<0,0.5+(0.5-correct),correct),
             zSkewDiff_human=if_else(correct>0.5,zSkewDiff,-zSkewDiff),
)

tmp_animal<-Spec %>%
  subset(zSkewDiff!=100) %>%
  dplyr::select(StimID,Species,zSkew_good,zSkew_bad) |>
  pivot_longer(cols=c(zSkew_good,zSkew_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Skew",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Skew",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zSkewDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Skew_good,zhuman_Skew_bad) |>
  pivot_longer(cols=c(zhuman_Skew_good,zhuman_Skew_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Skew",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Skew",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## SPREAD
##############
Spec$zSpreadDiff<-Spec$zSpread_good-Spec$zSpread_bad
hist(Spec$zSpreadDiff,breaks=20)
Spread_NAs<-nrow(subset(Spec,is.na(zSpreadDiff)))

model<-lmer(correct~zSpreadDiff+(1|Species),data=subset(Spec,zSpreadDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Spread",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Spread",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Spread_good=if_else(correct>0.5,zSpread_good,zSpread_bad),
             zhuman_Spread_bad=if_else(correct>0.5,zSpread_bad,zSpread_good),
             zSpread_corr=if_else(zSpreadDiff<0,0.5+(0.5-correct),correct),
             zSpreadDiff_human=if_else(correct>0.5,zSpreadDiff,-zSpreadDiff),
)

tmp_animal<-Spec %>%
  subset(zSpreadDiff!=100) %>%
  dplyr::select(StimID,Species,zSpread_good,zSpread_bad) |>
  pivot_longer(cols=c(zSpread_good,zSpread_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Spread",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Spread",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zSpreadDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Spread_good,zhuman_Spread_bad) |>
  pivot_longer(cols=c(zhuman_Spread_good,zhuman_Spread_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Spread",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Spread",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## SHANNON ENTROPY
##############
Spec$zEntDiff<-Spec$zEnt_good-Spec$zEnt_bad
hist(Spec$zEntDiff,breaks=20)
Ent_NAs<-nrow(subset(Spec,is.na(zEntDiff)))

model<-lmer(correct~zEntDiff+(1|Species),data=subset(Spec,zEntDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Ent",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Ent",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Ent_good=if_else(correct>0.5,zEnt_good,zEnt_bad),
             zhuman_Ent_bad=if_else(correct>0.5,zEnt_bad,zEnt_good),
             zEnt_corr=if_else(zEntDiff<0,0.5+(0.5-correct),correct),
             zEntDiff_human=if_else(correct>0.5,zEntDiff,-zEntDiff),
)

tmp_animal<-Spec %>%
  subset(zEntDiff!=100) %>%
  dplyr::select(StimID,Species,zEnt_good,zEnt_bad) |>
  pivot_longer(cols=c(zEnt_good,zEnt_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Ent",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Ent",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zEntDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Ent_good,zhuman_Ent_bad) |>
  pivot_longer(cols=c(zhuman_Ent_good,zhuman_Ent_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Ent",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Ent",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


##############
############## ROUGHNESS
##############
Spec$zRoughDiff<-Spec$zRough_good-Spec$zRough_bad
hist(Spec$zRoughDiff,breaks=80) # one suuper low value and one high value
Rough_NAs<-nrow(subset(Spec,is.na(zRoughDiff)))
Rough_nRemoved<-nrow(subset(Spec,!is.na(zRoughDiff)))-nrow(subset(Spec,abs(zRoughDiff)<1.5))

model<-lmer(correct~zRoughDiff+(1|Species),data=subset(Spec,abs(zRoughDiff)<1.5))
acousticTable<-rbind(acousticTable,data.frame(feature="Rough",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Rough",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Rough_good=if_else(correct>0.5,zRough_good,zRough_bad),
             zhuman_Rough_bad=if_else(correct>0.5,zRough_bad,zRough_good),
             zRough_corr=if_else(zRoughDiff<0,0.5+(0.5-correct),correct),
             zRoughDiff_human=if_else(correct>0.5,zRoughDiff,-zRoughDiff),
)

tmp_animal<-Spec %>%
  subset(abs(zRoughDiff)<1.5) %>%
  dplyr::select(StimID,Species,zRough_good,zRough_bad) |>
  pivot_longer(cols=c(zRough_good,zRough_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Rough",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Rough",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(abs(zRoughDiff)<1.5) %>%
  dplyr::select(StimID,Species,zhuman_Rough_good,zhuman_Rough_bad) |>
  pivot_longer(cols=c(zhuman_Rough_good,zhuman_Rough_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Rough",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Rough",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## ATTACK SLOPE
##############
Spec$zAttackDiff<-Spec$zAttack_good-Spec$zAttack_bad
hist(Spec$zAttackDiff,breaks=20)
Attack_NAs<-nrow(subset(Spec,is.na(zAttackDiff)))

model<-lmer(correct~zAttackDiff+(1|Species),data=subset(Spec,zAttackDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Attack",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Attack",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Attack_good=if_else(correct>0.5,zAttack_good,zAttack_bad),
             zhuman_Attack_bad=if_else(correct>0.5,zAttack_bad,zAttack_good),
             zAttack_corr=if_else(zAttackDiff<0,0.5+(0.5-correct),correct),
             zAttackDiff_human=if_else(correct>0.5,zAttackDiff,-zAttackDiff),
)

tmp_animal<-Spec %>%
  subset(zAttackDiff!=100) %>%
  dplyr::select(StimID,Species,zAttack_good,zAttack_bad) |>
  pivot_longer(cols=c(zAttack_good,zAttack_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Attack",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Attack",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zAttackDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Attack_good,zhuman_Attack_bad) |>
  pivot_longer(cols=c(zhuman_Attack_good,zhuman_Attack_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Attack",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Attack",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## RMS
##############

Spec$zRMSDiff<-Spec$zRMS_good-Spec$zRMS_bad
hist(Spec$zRMSDiff,breaks=20)
RMS_NAs<-nrow(subset(Spec,is.na(zRMSDiff)))

model<-lmer(correct~zRMSDiff+(1|Species),data=subset(Spec,zRMSDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="RMS",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="RMS",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_RMS_good=if_else(correct>0.5,zRMS_good,zRMS_bad),
             zhuman_RMS_bad=if_else(correct>0.5,zRMS_bad,zRMS_good),
             zRMS_corr=if_else(zRMSDiff<0,0.5+(0.5-correct),correct),
             zRMSDiff_human=if_else(correct>0.5,zRMSDiff,-zRMSDiff),
)

tmp_animal<-Spec %>%
  subset(zRMSDiff!=100) %>%
  dplyr::select(StimID,Species,zRMS_good,zRMS_bad) |>
  pivot_longer(cols=c(zRMS_good,zRMS_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="RMS",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="RMS",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zRMSDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_RMS_good,zhuman_RMS_bad) |>
  pivot_longer(cols=c(zhuman_RMS_good,zhuman_RMS_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="RMS",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="RMS",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## BRIGHTNESS
##############
Spec$zBrightDiff<-Spec$zBright_good-Spec$zBright_bad
hist(Spec$zBrightDiff,breaks=20) ## a lot of values near zero because both stim max out the feature
Bright_NAs<-nrow(subset(Spec,is.na(zBrightDiff)))

model<-lmer(correct~zBrightDiff+(1|Species),data=subset(Spec,zBrightDiff!=100))
acousticTable<-rbind(acousticTable,data.frame(feature="Bright",comparison="Diff",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Bright",comparison="Diff",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

Spec<-mutate(Spec,
             zhuman_Bright_good=if_else(correct>0.5,zBright_good,zBright_bad),
             zhuman_Bright_bad=if_else(correct>0.5,zBright_bad,zBright_good),
             zBright_corr=if_else(zBrightDiff<0,0.5+(0.5-correct),correct),
             zBrightDiff_human=if_else(correct>0.5,zBrightDiff,-zBrightDiff),
)

tmp_animal<-Spec %>%
  subset(zBrightDiff!=100) %>%
  dplyr::select(StimID,Species,zBright_good,zBright_bad) |>
  pivot_longer(cols=c(zBright_good,zBright_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_animal)
acousticTable<-rbind(acousticTable,data.frame(feature="Bright",comparison="Animals",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Bright",comparison="Animals",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

tmp_human<-Spec %>%
  subset(zBrightDiff!=100) %>%
  dplyr::select(StimID,Species,zhuman_Bright_good,zhuman_Bright_bad) |>
  pivot_longer(cols=c(zhuman_Bright_good,zhuman_Bright_bad), names_to = "quality", values_to = "feature")

model<-lmer(feature~quality + (1|StimID) + (1|Species),data=tmp_human)
acousticTable<-rbind(acousticTable,data.frame(feature="Bright",comparison="Humans",anova=anova(model)))
AcousticDF<-rbind(AcousticDF,data.frame(feature="Bright",comparison="Humans",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


F2B<-ggplot(data=subset(AcousticDF,comparison %in% c("Humans","Animals")),aes(y=feature,x=estimate,color=comparison))+
  geom_errorbar(aes(xmin=estimate-(1.69*std), xmax=estimate+(1.96*std)),width=.2,position=position_dodge(width=0.7))+
  geom_point(position=position_dodge(width=0.7),size=0.8)+
  geom_vline(xintercept=0.0, linetype="dashed", color = "blue") +
  scale_y_discrete(limits = rev)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        aspect.ratio = 2,
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.position="none"
  )

write.csv(acousticTable,here::here("temp", "acousticTable.csv"))

##############
############## ALL FEATURES TOGETHER
##############
# SpecAnimals<-Spec %>%
#   select(zDFDiff,zDurDiff,zPitchDiff,zAMvarDiff,zSkewDiff,zSpreadDiff,zEntDiff,zAttackDiff,zRMSDiff,zCentDiff,zRoughDiff,zBrightDiff) %>%
#   pivot_longer(cols=c("zDFDiff","zDurDiff","zPitchDiff","zAMvarDiff","zSkewDiff","zSpreadDiff","zEntDiff","zAttackDiff","zRMSDiff","zCentDiff","zRoughDiff","zBrightDiff"),names_to = "feature", values_to = "diff") %>%
#   mutate(group="animals")
# 
# SpecHumans<-Spec %>%
#   select(zDFDiff_human,zDurDiff_human,zPitchDiff_human,zAMvarDiff_human,zSkewDiff_human,zSpreadDiff_human,zEntDiff_human,zAttackDiff_human,zRMSDiff_human,zCentDiff_human,zRoughDiff_human,zBrightDiff_human) %>%
#   pivot_longer(cols=c("zDFDiff_human","zDurDiff_human","zPitchDiff_human","zAMvarDiff_human","zSkewDiff_human","zSpreadDiff_human","zEntDiff_human","zAttackDiff_human","zRMSDiff_human","zCentDiff_human","zRoughDiff_human","zBrightDiff_human"),names_to = "feature", values_to = "diff") %>%
#   mutate(group="humans")
# SpecHumans$feature<-str_remove(SpecHumans$feature,"_human")
# 
# SpecAll<-rbind(SpecAnimals,SpecHumans)
# SpecAllModel<-lm(diff~group*feature,data=SpecAll)
# SpecAllAnova<-anova(SpecAllModel)

# demographic analysis

## First need to reshape most of the variables
FullTest$expert<-as.factor(FullTest$expert)
FullTest_withAllCovariates<-FullTest %>%
  mutate(incomeNum=if_else(income=="I'd prefer not to say"|is.na(income),NA,
                           if_else(income=="Under $10,000",1,
                                   if_else(income=="$10,000 to $19,999",2,
                                           if_else(income=="$20,000 to $29,999",3,
                                                   if_else(income=="$30,000 to $39,999",4,
                                                           if_else(income=="$40,000 to $49,999",5,
                                                                   if_else(income=="$50,000 to $74,999",6,
                                                                           if_else(income=="$75,000 to $99,999",7,
                                                                                   if_else(income=="$100,000 to $150,000",8,9))))))))),
         education=if_else(education=="<NA>"|education=="Other (specify on next page)"|is.na(education),NA,education),
         musicEnjoy=if_else(is.na(musicEnjoy)|musicEnjoy==500,NA,if_else(musicEnjoy==1000,"Yes","No")),
         listenExpert=if_else(is.na(musicListen)|musicListen==500,NA,if_else(musicListen>500,"Yes","No")),
         MusicExpert=if_else(musicSkill=="I have a lot of skill"|musicSkill=="I'm an expert","Yes","No"))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,makeNum=if_else(musicMakeTime=="No time at all",1,if_else(
  musicMakeTime=="1-5 minutes",2,if_else(
    musicMakeTime=="6-10 minutes",3,if_else(
      musicMakeTime=="11-15 minutes",4,if_else(
        musicMakeTime=="16-30 minutes",5,if_else(
          musicMakeTime=="31-60 minutes",6,if_else(
            musicMakeTime=="1-2 hours",7,if_else(
              musicMakeTime=="2-4 hours",8,9
            )
          )
        )
      )
    )
  )
)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,education_num=if_else(education=="Some elementary/middle school<br>(primary school)",0,
                                                                                    if_else(education=="Completed elementary/middle school<br>(primary school)",1,
                                                                                            if_else(education=="Some high school",2,
                                                                                                    if_else(education=="Completed high school<br>(secondary school)",3,
                                                                                                            if_else(education=="Some undergrad<br>(higher education)",4,
                                                                                                                    if_else(education=="Completed undergrad degree<br>(~3-5 years higher education)",5,
                                                                                                                            if_else(education=="Some graduate school",6,7))))))))



FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,ListenNum=if_else(musicListenTime=="No time at all",1,if_else(
  musicListenTime=="1-5 minutes",2,if_else(
    musicListenTime=="6-10 minutes",3,if_else(
      musicListenTime=="11-15 minutes",4,if_else(
        musicListenTime=="16-30 minutes",5,if_else(
          musicListenTime=="31-60 minutes",6,if_else(
            musicListenTime=="1-2 hours",7,if_else(
              musicListenTime=="2-4 hours",8,9
            )
          )
        )
      )
    )
  )
)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,lessonsNum=if_else(lessonsPeers=="They were a lot better than me",1,if_else(
  lessonsPeers=="They were a little better than me",2,if_else(
    lessonsPeers=="We had about the same level of musical skills",3,if_else(
      lessonsPeers=="I was a little better than them",4,5
    )
  )
)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,
                                   zListen=(ListenNum - mean(ListenNum,na.rm=TRUE))/sd(ListenNum,na.rm=TRUE),
                                   zLessons=(lessonsNum - mean(lessonsNum,na.rm=TRUE))/sd(lessonsNum,na.rm=TRUE),
                                   zEducation=(education_num - mean(education_num,na.rm=TRUE))/sd(education_num,na.rm=TRUE),
                                   zIncome=(incomeNum - mean(incomeNum,na.rm=TRUE))/sd(incomeNum,na.rm=TRUE),
                                   zAge=(age - mean(age,na.rm=TRUE))/sd(age,na.rm=TRUE)
)

FullModel_withDemographics<-glmer(correct ~ (1|StimID) + (1|Species) + (1|user_id) + gender + zAge + zEducation + zIncome, family = binomial('logit'),data=subset(FullTest_withAllCovariates,AnimalStrength>66))
FullModel_withDemographics_summary<-summary(FullModel_withDemographics)

FullModel_withListenTime<-glmer(correct ~ (1|StimID) + (1|Species) + (1|user_id) + zListen, family = binomial('logit'),data=subset(FullTest_withAllCovariates,AnimalStrength>66))
FullModel_withListenTime_summary<-summary(FullModel_withListenTime)

# analysis of each characteristic: 

demographic<-data.frame(feature=NULL,estimate=NULL,std=NULL)

##############
############## ANIMAL EXPERTS
##############
model<-glmer(correct ~ expert + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(expert)))

demographic<-rbind(demographic,data.frame(feature="animal sounds expert",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))
expertAnova<-Anova(model)
demographicAnova<-data.frame(feature="animal sounds expert",anova=expertAnova)

nAnimalExperts<-length(unique(subset(FullTest_withAllCovariates,expert==0)$user_id))
nAnimalNonExperts<-length(unique(subset(FullTest_withAllCovariates,expert==1)$user_id))

##############
############## GENDER
##############
model<-glmer(correct ~ gender + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(gender)))
pairs(emmeans(model,~gender))
genderAnova<-Anova(model)
demographicAnova<-rbind(demographicAnova,data.frame(feature="gender",anova=genderAnova))

model<-glmer(correct ~ gender + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&gender!="Other"))
demographic<-rbind(demographic,data.frame(feature="gender",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## INCOME
##############
FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,incomeBinary=if_else(incomeNum>5,"High","Low"))

model<-glmer(correct ~ incomeBinary + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(incomeBinary)))
demographic<-rbind(demographic,data.frame(feature="income",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

model<-glmer(correct ~ incomeNum + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(incomeNum)))
incomeAnova<-Anova(model)
demographicAnova<-rbind(demographicAnova,data.frame(feature="income",anova=incomeAnova))

##############
############## AGE
##############
model<-glmer(correct ~ zAge + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&age<45))
ageAnova<-Anova(model)
demographicAnova<-rbind(demographicAnova,data.frame(feature="age",anova=ageAnova))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,ageBinary=if_else(age<29,"Low","High"))
model<-glmer(correct ~ ageBinary + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66))
demographic<-rbind(demographic,data.frame(feature="age",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## MUSIC LESSONS
##############
model<-glmer(correct ~ musicLessons + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(musicLessons)))
demographic<-rbind(demographic,data.frame(feature="music lessons",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))
demographicAnova<-rbind(demographicAnova,data.frame(feature="music lessons",anova=Anova(model)))

##############
############## EDUCATION
##############
model<-glmer(correct ~ education_num + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(education_num)))
demographicAnova<-rbind(demographicAnova,data.frame(feature="education",anova=Anova(model)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,educationBinary=if_else(education_num>4,"high","low"))

model<-glmer(correct ~ educationBinary + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(educationBinary)))
demographic<-rbind(demographic,data.frame(feature="education",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## MUSIC EXPERT
##############
model<-glmer(correct ~ MusicExpert + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(MusicExpert)))
demographicAnova<-rbind(demographicAnova,data.frame(feature="music expert",anova=Anova(model)))
expertMusicAnova<-Anova(model)
demographic<-rbind(demographic,data.frame(feature="music expert",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

nMusicExperts<-length(unique(subset(FullTest_withAllCovariates,MusicExpert=="Yes")$user_id))
nMusicNonExperts<-length(unique(subset(FullTest_withAllCovariates,MusicExpert=="No")$user_id))


##############
############## MUSIC ENJOYMENT
##############
model<-glmer(correct ~ musicEnjoy + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(musicEnjoy)))

n_enjoy_total<-length(unique(subset(FullTest,AnimalStrength>66&!is.na(musicEnjoy))$user_id))
n_enjoy_500<-length(unique(subset(FullTest,AnimalStrength>66&musicEnjoy==500)$user_id))

enjoyAnova<-Anova(model)
demographicAnova<-rbind(demographicAnova,data.frame(feature="music enjoyment",anova=Anova(model)))
demographic<-rbind(demographic,data.frame(feature="music enjoyment",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


##############
############## TIME MAKING MUSIC
##############
model<-glmer(correct ~ makeNum + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(makeNum)))
demographicAnova<-rbind(demographicAnova,data.frame(feature="time making music",anova=Anova(model)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,makeBinary=if_else(makeNum>3,"high","low"))
model<-glmer(correct ~ makeBinary + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(makeBinary)))
demographic<-rbind(demographic,data.frame(feature="time making music",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

##############
############## TIME LISTENING TO MUSIC
##############
model<-glmer(correct ~ ListenNum + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(ListenNum)))
listenAnova<-Anova(model)
demographicAnova<-rbind(demographicAnova,data.frame(feature="time listening to music",anova=listenAnova))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,listenBinary=if_else(ListenNum>6,"high","low"))
model<-glmer(correct ~ listenBinary + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(listenBinary)))
demographic<-rbind(demographic,data.frame(feature="time listening to music",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


##############
############## ABILITY WRT PEERS
##############
model<-glmer(correct ~ lessonsNum + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(lessonsNum)))
demographicAnova<-rbind(demographicAnova,data.frame(feature="ability wrt peers",anova=Anova(model)))

FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,lessonsBinary=if_else(lessonsNum>3,"high","low"))
model<-glmer(correct ~ lessonsBinary + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(lessonsBinary)))
demographic<-rbind(demographic,data.frame(feature="ability wrt peers",estimate=-summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))


##############
############## LISTEN SKILL
##############
n_listenSkill_total<-length(unique(subset(FullTest,AnimalStrength>66&!is.na(musicListen))$user_id))
n_listenSkill_500<-length(unique(subset(FullTest,AnimalStrength>66&musicListen==500)$user_id))

FullTest_withAllCovariates$musicListen<-as.numeric(FullTest_withAllCovariates$musicListen)
FullTest_withAllCovariates<-mutate(FullTest_withAllCovariates,listenExpert=if_else(musicListen==500,NA,if_else(musicListen>500,"Yes","No")))
model<-glmer(correct ~ listenExpert + (1|StimID) + (1|Species) + (1|user_id), family=binomial, data=subset(FullTest_withAllCovariates,AnimalStrength>66&!is.na(listenExpert)))
demographicAnova<-rbind(demographicAnova,data.frame(feature="listening skill",anova=Anova(model)))
listenSkillAnova<-Anova(model)
demographic<-rbind(demographic,data.frame(feature="listening skill",estimate=summary(model)$coeff[2,1],std=summary(model)$coeff[2,2]))

demographic$feature<-factor(demographic$feature,levels=c("animal sounds expert","music expert","music lessons","ability wrt peers","time making music","time listening to music","listening skill","music enjoyment","education","age","income","gender"))

demographicAnova$feature<-factor(demographicAnova$feature,levels=c("animal sounds expert","music expert","music lessons","ability wrt peers","time making music","time listening to music","listening skill","music enjoyment","education","age","income","gender"))

F2C<-ggplot(data=demographic,aes(y=feature,x=estimate))+
  geom_errorbar(aes(xmin=estimate-(1.96*std), xmax=estimate+(1.96*std)),width=.2)+
  geom_point()+
  geom_vline(xintercept=0.0, linetype="dashed", color = "blue") +
  scale_y_discrete(limits = rev)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        aspect.ratio = 1.5,
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        #legend.position="none"
  )

write.csv(demographicAnova,here::here("temp", "demographicTable.csv"))

## figures
F1B1<- ggplot() + theme_void()

F1L <- (F1A/(F1C|F1D))+
  plot_layout(heights=c(4,3))

F1RB <- (F1B2|F1B3) +
  plot_layout(widths=c(7,1))

F1R <- (F1B1 / F1RB)+
  plot_layout()

F1 <- (F1L|F1R)+
  plot_layout(widths=c(3,7))

pdf(file = here::here("temp", "Figure1.pdf"),
    width = 10, 
    height = 10) 
F1
dev.off()
# NOTE - this exports the source images for the figure that actually appears 
# in the article, but the source images were subsequently manually edited, 
# e.g., to include the silhouettes of each animal, significance stars, etc


F2BC<-(F2B|F2C)+
  plot_layout(widths=c(3,4))

F2 <- F2A/F2BC

pdf(file = here::here("temp", "Figure2.pdf"),
    width = 6, 
    height = 6) 
F2
dev.off()

# NOTE - this exports the source images for the figure that actually appears 
# in the article, but the source images were subsequently manually edited, 
# e.g., to include the silhouettes of each animal, significance stars, etc

# gender analyses for SI
genderModel_fullDataset<-glmer(correct ~ gender + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=FullTest_withAllCovariates)
genderAnova_fullDataset<-Anova(genderModel_fullDataset)
genderPosthoc<-as.data.frame(pairs(emmeans(genderModel_fullDataset,~gender)))

genderModel_Male<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,gender=="Male"&AnimalStrength>66))
genderSum_Male<-summary(genderModel_Male)
genderModel_Other<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,gender=="Other"&AnimalStrength>66))
genderSum_Other<-summary(genderModel_Other)
genderModel_Female<-glmer(correct ~ 1 + (1|StimID) + (1|Species) + (1|user_id), family='binomial',data=subset(FullTest_withAllCovariates,gender=="Female"&AnimalStrength>66))
genderSum_Female<-summary(genderModel_Female)

meanPref_Male<-mean(subset(FullTest,gender=="Male"&AnimalStrength>66)$correct)
meanPref_Female<-mean(subset(FullTest,gender=="Female"&AnimalStrength>66)$correct)
meanPref_Other<-mean(subset(FullTest,gender=="Other"&AnimalStrength>66)$correct)

SEPref_Male<-sd(subset(FullTest,gender=="Male"&AnimalStrength>66)$correct)/sqrt(nrow(subset(FullTest_withAllCovariates,gender=="Male"&AnimalStrength>66)))
SEPref_Female<-sd(subset(FullTest,gender=="Female"&AnimalStrength>66)$correct)/sqrt(nrow(subset(FullTest_withAllCovariates,gender=="Female"&AnimalStrength>66)))
SEPref_Other<-sd(subset(FullTest,gender=="Other"&AnimalStrength>66)$correct)/sqrt(nrow(subset(FullTest_withAllCovariates,gender=="Other"&AnimalStrength>66)))

strength_model_female<-glmer(correct~AnimalStrength+(1|Species)+(1|StimID)+(1|user_id),family="binomial",data=subset(FullTest_withAllCovariates,!is.na(AnimalStrength)&gender=="Female"))
strength_anova_female<-Anova(strength_model_female)
strength_model_male<-glmer(correct~AnimalStrength+(1|Species)+(1|StimID)+(1|user_id),family="binomial",data=subset(FullTest_withAllCovariates,!is.na(AnimalStrength)&gender=="Male"))
strength_anova_male<-Anova(strength_model_male)
strength_model_other<-glmer(correct~AnimalStrength+(1|Species)+(1|StimID)+(1|user_id),family="binomial",data=subset(FullTest_withAllCovariates,!is.na(AnimalStrength)&gender=="Other"))
strength_anova_other<-Anova(strength_model_other)

gender_corr<- FullTest_withAllCovariates %>%
  subset(gender=="Male"|gender=="Female") %>%
  group_by(StimID,gender,Species,Category,Trait) %>%
  dplyr::summarize(meanAgreement=mean(correct)) %>%
  pivot_wider(names_from=gender,values_from = meanAgreement)
gender_corr_model<-lm(Female~Male,data=gender_corr)
gender_corr_summary<-summary(gender_corr_model)
gender_corr_anova<-anova(gender_corr_model)

# save workspace for use in Rmd
save.image(file = here::here("temp", "results.RData"))

