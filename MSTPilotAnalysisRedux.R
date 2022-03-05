rm(list=ls())

#install necessary packages
packages <- c("tidyverse", "readr", "lavaan", "Hmisc", "cowplot", "ggridges",
              "here", "afex","RColorBrewer","patchwork")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}


#### MST Pilot Analysis ####
library(tidyverse)
library(readr)
library(Hmisc)
library(ggridges)
library(here)
library(afex)
library(cowplot) 
library(RColorBrewer) 
library(patchwork)



####load in data

MSTOlder <- read_csv(here("MSTOlderPilot.csv"))
MSTOlder$Group<-"Old"
#MSTOlder<-MSTOlder[,c(1:9,13:27)]
###bug fixed so young has no prolific ID, but older do.
MSTYounger <- read_csv(here("MSTYoungerPilot.csv"))

MSTYounger$Group<-"Young"

#unique(MSTYoungerSecond$ip)


#MST<-rbind(MSTOlder,MSTYoungerFirst)

YoungReVecF<-c(1:95,122,258,266,271,297,311)
YoungReVecS<-c(96:121,123:257,259:265:267:270,272:296,298:310,312:452)


MSTYoungerFirst <-filter(MSTYounger, run_id %in% YoungReVecF)
#replace a mixed up code
MSTYoungerFirst$ip<-MSTYoungerFirst$STUDY_ID

MSTYoungerSecond<-filter(MSTYounger, run_id %in% YoungReVecS)

OldReVecF<-c(1:205,207:303,305:352)
OldReVecS<-c(206,304)

# for SAM
MSTYoungerSeconds<-MSTYoungerSecond
MSTYoungerSeconds$responses<-MSTYoungerSeconds$key_press

MSTOlderFirst <-filter(MSTOlder, run_id %in% OldReVecF)

MSTOlderSecond <-filter(MSTOlder, run_id %in% OldReVecS)

Names<-as.data.frame(colnames(MSTYoungerSecond))
MSTYoungerSecond$key_press<-MSTYoungerSecond$STUDY_ID
MSTYoungerSecond$correct<-MSTYoungerSecond$SESSION_ID

MSTYounger<-rbind(MSTYoungerFirst,MSTYoungerSecond)

#for SAM analysis
MSTYoungers<-rbind(MSTYoungerFirst,MSTYoungerSeconds)

#MSTOlder<-MSTOlder[,c(1:23,27)]

Names<-cbind(colnames(MSTYounger),colnames(MSTOlder))

MSTOlder<-MSTOlderFirst[,c(1:26,28)]
MSTYounger<-MSTYounger[,c(1:26,28)]

MSTYoungers<-MSTYoungers[,c(1:26,28)]

MST <- rbind(MSTOlder,MSTYounger)

MSTs <- rbind(MSTOlder, MSTYoungers)

ForSAM <-MSTs %>%
  group_by(Group,run_id) %>%
  summarise(mean=mean(as.numeric(rt)))



#Trial Index 5-142 is for the Indoor/Outdoor Judgement Task
JudgeVec<-c(5:142)
MSTJudge <- filter(MST, trial_index %in% JudgeVec)

JAttVec<- c("Check1.jpg","Check2.jpg","Check3.jpg","Check4.jpg","Check5.jpg") 
  
MSTAttentionJudge <- filter(MSTJudge, stimulus %in% JAttVec)

MSTAttentionJudge$correct<-ifelse(MSTAttentionJudge$key_press==77,1,0)

MSTAttentionJudgey <- MSTAttentionJudge %>%
select(stimulus,Group,run_id,ip,correct,subject) %>%
group_by(run_id,ip,Group,subject) %>%
summarise(Count = n(),score=mean(correct))

MSTAttentionJudgey$Exclude<-ifelse(MSTAttentionJudgey$score<0.6,"Exclude","Keep")

MSTAttentionJudgeyExclude <- filter(MSTAttentionJudgey, Exclude %in% "Exclude")
MSTAttentionJudgeyInclude <- filter(MSTAttentionJudgey, Exclude %in% "Keep")


MSTMisunderstoodInstructYoung<-c("5f31961c57eb9507db148757","5f399beaf4527611ac425a28")
MSTMisunderstoodInstructOld<-c("57bf14513c449a000189c89a","5c3393d9f53f7a000119160f")

MSTMisunderstoodInstruct<-c(MSTMisunderstoodInstructYoung,MSTMisunderstoodInstructOld)

#MSTAttentionJudgeyInclude<-subset(MSTAttentionJudgeyInclude, subject  != MSTMisunderstoodInstruct)


#Trial Index 146-550 is for the Old/Similar/New Judgement Task
TestVec<-c(146:550)
TestVecAns<-c("0","1","2")

MSTTest <- filter(MST, trial_index %in% TestVec)


TestVec3<-MSTTest$stimulus
TestVec3<-append(TestVec3,"placeholder",after= 0)
TestVec3<-TestVec3[-length(TestVec3)]

MSTTest$Stim<-TestVec3

MSTTest <- filter(MSTTest, button_pressed %in% TestVecAns)

StimVec<-c("AttCheckNew1.jpg","AttCheckOld1.jpg","AttCheckSimilar1.jpg")

MSTTest <- filter(MSTTest, Stim %in% StimVec)

MSTTestOld <- filter(MSTTest, Stim %in% "AttCheckOld1.jpg")
MSTTestSimilar <- filter(MSTTest, Stim %in% "AttCheckSimilar1.jpg")
MSTTestNew <- filter(MSTTest, Stim %in% "AttCheckNew1.jpg")

MSTTestOld$correct <-ifelse(MSTTestOld$button_pressed=="0",1,0)
MSTTestSimilar$correct <-ifelse(MSTTestSimilar$button_pressed=="1",1,0)
MSTTestNew$correct <-ifelse(MSTTestNew$button_pressed=="2",1,0)

MSTTest<-rbind(MSTTestOld,MSTTestSimilar,MSTTestNew)

MSTAttentionTesty <- MSTTest %>%
  select(Stim,Group,run_id,ip,correct) %>%
  group_by(run_id,ip) %>%
  summarise(Count = n(),score=mean(correct))

MSTAttentionTesty$Exclude<-ifelse(MSTAttentionTesty$score<0.7,"Exclude","Keep")

MSTAttentionTestyExclude <- filter(MSTAttentionTesty, Exclude %in% "Exclude")
MSTAttentionTestyInclude <- filter(MSTAttentionTesty, Exclude %in% "Keep")







#### Actual Behavioral Analysis ####
#MSTTest <- filter(MST, trial_index %in% TestVec)

MSTTest<-MST

TestVec3<-MSTTest$stimulus
TestVec3<-append(TestVec3,"placeholder",after= 0)
TestVec3<-TestVec3[-length(TestVec3)]

MSTTest$Stim<-TestVec3

MSTTest <- filter(MSTTest, rt != "null")
MSTTest <- filter(MSTTest, ip %in% MSTAttentionJudgeyInclude$ip)
MSTTest <- filter(MSTTest, ip %in% MSTAttentionTestyInclude$ip)
MSTTest$rt <- as.integer(MSTTest$rt)

MSTTestOld <- filter(MSTTest, Group == "Old")
MSTTestYoung <- filter(MSTTest, Group == "Young")

t.test(MSTTestOld$rt,MSTTestYoung$rt)

my2cols <- c("darkturquoise", "darkgoldenrod1")
mycol<- c("black","black")

ggplot(MSTTest, aes(x = REC, y = Group,fill = Group)) +
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines=TRUE, vline_size= 1, vline_color="red",
    alpha = 0.7, scale = 0.8, 
    point_size = 1.5, point_alpha= 1, 
    position =position_raincloud(adjust_vlines = TRUE)
  ) +
  scale_fill_manual(values = my2cols)

glm.probs


MSTTestAnsOld<-filter(MSTTest, correct == "Old")
MSTTestAnsSimilar<-filter(MSTTest, correct == "Similar")
MSTTestAnsNew<-filter(MSTTest, correct == "New")

#Them actually being correct
MSTTestAnsOld$Ans<-ifelse(MSTTestAnsOld$button_pressed=="0",1,0)
MSTTestAnsSimilar$Ans<-ifelse(MSTTestAnsSimilar$button_pressed=="1",1,0)
MSTTestAnsNew$Ans<-ifelse(MSTTestAnsNew$button_pressed=="2",1,0)

MSTTestAns<-rbind(MSTTestAnsOld,MSTTestAnsSimilar,MSTTestAnsNew)
write.csv(MSTTestAns,"ForLogistic.csv")

#pEta^2 = t^2/(t^2 + df)



Scores <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(ip,correct,Group) %>%
  summarise(Count = n(),score=mean(Ans),rtmean=mean(rt))

#Scores<-filter(Scores, score > .15)


#Scores <- filter(Scores, score>0)#



ScoresOverall <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(ip,Group) %>%
  summarise(Count = n(),score=mean(Ans),rtmean=mean(rt),run_id=mean(run_id))

ScoresOverallOld<-filter(ScoresOverall, Group=="Old")
mean(ScoresOverallOld$score)
sd(ScoresOverallOld$score)


ScoresOverallYoung<-filter(ScoresOverall, Group=="Young")
mean(ScoresOverallYoung$score)
sd(ScoresOverallYoung$score)

t.test(ScoresOverallOld$score,mu = .33)
t.test(ScoresOverallYoung$score,mu = .33)

t.test(ScoresOverall$score,mu = .33)


#e + geom_violin(aes(fill = Group), trim = FALSE) + 
#  geom_boxplot(width = 0.2)+
#  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
#  theme(legend.position = "none")


#write.csv(as_tibble(Scores[,c(2,3,5)]),"RaincloudDemo.csv")






#### make a raincloud plot ####

Offset<-ifelse(ScoresOverall$Group=="Old",1,2)

#Accuracy
library(PupillometryR)
library(wesanderson)

pal <- wes_palette("Darjeeling1", 2, type = "discrete")

p6 <- ggplot(ScoresOverall,aes(x=Group,y=score, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Accuracy')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=pal)+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Accuracy Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5,
  color = "black", size = 24, face = "bold"),
  axis.title =element_text(color = "black", size = 20, face = "bold"), 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))



anova<-aov(data=ScoresOverall,score~Group)
summary.aov(anova)


p8 <- ggplot(ScoresOverall,aes(x=Group,y=rtmean, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Response Time (in msec)')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=pal)+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Response Times Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))




p6 + p8

library(plyr)

#offset<-revalue(Scores$correct,c("New"=1,"Old"=2,"Similar"=3))

Offset<-ifelse(Scores$Group=="Old",1,2)

p10 <- ggplot(Scores,aes(x=Group,y=score, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .15, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.15,y=score, fill=Group),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Accuracy')+xlab('Age Group')+ ylim(0,1) +
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Accuracy Across Stimuli Types")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p10a<-p10 + facet_wrap(~correct)

p12 <- ggplot(Scores,aes(x=Group,y=rtmean, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Response Time (in msec)')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Response Times Across Stimuli Type")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p12a<-p12 + facet_wrap(~correct)

pAccRT<-p10a / p12a

pAccRT

# save the ggplot to png --------------------------------------------------





Mixed.aov.Firstfc<-aov_car(value ~ Group*Horizon + Error(SN/Horizon), data=FirstTrialfc)

knitr::kable(nice(Mixed.aov.Firstfc)) ##call for formatted ANOVA table using knitr




anova<-aov(data=ScoresOverall,score~Group)
summary.aov(anova)




                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  scale_fill_brewer(palette = "Dark2")+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")


#LDI

# False Alarms (Old stimuli called new)
MSTTestAnsOld$TrueOld<-ifelse(MSTTestAnsOld$button_pressed=="0",1,0)

MSTTestAnsNew$FalseOlds<-ifelse(MSTTestAnsNew$button_pressed=="0",1,0)

detach(package:plyr)
library(dplyr)

Rec1<- MSTTestAnsOld %>%
  select(Group,run_id,ip,correct,Ans,TrueOld) %>%
  group_by(Group,run_id) %>%
  summarise(Count = n(),TrueOldScore=mean(TrueOld))


Rec2<- MSTTestAnsNew %>%
  select(Group,run_id,ip,correct,Ans,FalseOlds) %>%
  group_by(run_id,Group) %>%
  summarise(Count = n(),FalseOldScore=mean(FalseOlds))

Rec1$FalseOldScore<-Rec2$FalseOldScore

Rec1$REC<-Rec1$TrueOldScore - Rec1$FalseOldScore

#Rec1Old<-


ggplot(Rec1, aes(x = REC, y = Group,fill = Group)) +
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines=TRUE, vline_size= 1, vline_color="red",
    alpha = 0.7, scale = 0.8, 
    point_size = 1.5, point_alpha= 1, 
    position =position_raincloud(adjust_vlines = TRUE)
  ) +
  scale_fill_manual(values = pal)



Rec1Old<-filter(Rec1,Group=="Old")
Rec1Young<-filter(Rec1,Group=="Young")

t.test(Rec1Old$REC,Rec1Young$REC)










# Call it similar when it is actually new
MSTTestAnsNew$FalseSimilars<-ifelse(MSTTestAnsNew$button_pressed=="1",1,0)

FalseSimilars<- MSTTestAnsNew %>%
  select(Group,run_id,ip,correct,Ans,FalseSimilars) %>%
  group_by(run_id,Group) %>%
  summarise(Count = n(),FalseSimilarsScore=mean(FalseSimilars))

#FalseSimilars<-filter(FalseSimilars, ip %in% SimilarScores$ip )

SimilarScores<-filter(Scores, correct=="Similar")
SimilarScores$FalseSimilars<-FalseSimilars$FalseSimilarsScore
SimilarScores$run_id<-FalseSimilars$run_id

SimilarScores$LDI<-SimilarScores$score-SimilarScores$FalseSimilars

SimilarScoresOld<-filter(SimilarScores,Group=="Old")
SimilarScoresYoung<-filter(SimilarScores,Group=="Young")


t.test(SimilarScoresOld$LDI,SimilarScoresYoung$LDI)
wilcox.test(SimilarScoresOld$LDI,SimilarScoresYoung$LDI)

MSTTestAns$rt<-as.numeric(MSTTestAns$rt)
Scores <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(run_id,correct,Group) %>%
  summarise(Count = n(),score=mean(Ans),run_id=mean(run_id), rtmean=mean(rt))

ScoresOld<-filter(Scores, Group=="Old")
ScoresYoung<-filter(Scores, Group=="Young")

mean(SimilarScoresOld$LDI)

ScoresOld$id<-sort(rep(c(1:76),3))
ScoresYoung$id<-sort(rep(c(77:161),3))

Scores<-rbind(ScoresOld, ScoresYoung)

Mixed.aov.Acc<-aov_car(score ~ Group*correct + Error(id/correct), data=Scores)

knitr::kable(nice(Mixed.aov.Acc))

Mixed.aov.RT<-aov_car(rtmean ~ Group*correct + Error(id/correct), data=Scores)

knitr::kable(nice(Mixed.aov.RT))




ScoresFiltered <- filter(ScoresOverall, score > .55)

SimilarScoresFiltered <- filter(SimilarScores, ip %in% ScoresFiltered$ip )


p7 <- ggplot(SimilarScores,aes(x=Group,y=LDI, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = ifelse(SimilarScores$Group=="Young",2,1)+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Lure Discimination Index')+xlab('Age Group')+
  ylim(-.5,1)+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Lure Discrimination Index")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p7


p9 <- ggplot(Rec1,aes(x=Group,y=REC, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE) +
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22) +
  geom_boxplot(aes(x = ifelse(Rec1$Group=="Young",2,1)+0.25, colour=),outlier.shape = NA, alpha = 0.5, width = .1) + 
  ylab('Recognition Index')+xlab('Age Group')+
  ylim(0,1.5)+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Recognition Index")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))


#### analysis with only those who did it the worst ####


# Supplementary Analyses --------------------------------------------------




#ScoresFiltered <- filter(ScoresOverall, score > .55)

#SimilarScoresFiltered <- filter(SimilarScores, ip %in% ScoresFiltered$ip )

Scores <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(ip,correct,Group) %>%
  summarise(Count = n(),score=mean(Ans),rtmean=mean(rt))

ScoresForFilterOld<-filter(Scores, correct == "Old")

ScoresForFilterOld<-filter(ScoresForFilterOld, score > .33)


Scores <- filter(Scores, ip %in% ScoresForFilterOld$ip )


#Scores <- filter(Scores, score>0)#



ScoresOverall <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(ip,Group) %>%
  summarise(Count = n(),score=mean(Ans),rtmean=mean(rt),run_id=mean(run_id))

ScoresOverall <- filter(ScoresOverall, ip %in% ScoresForFilterOld$ip )


MSTTestAns <- filter(MSTTestAns, ip %in% ScoresForFilterOld$ip)

MSTTestAnsNew <- filter(MSTTestAns, correct == "New")
MSTTestAnsOld <- filter(MSTTestAns, correct == "Old")
MSTTestAnsSimilar <- filter(MSTTestAns, correct == "Similar")


#e + geom_violin(aes(fill = Group), trim = FALSE) + 
#  geom_boxplot(width = 0.2)+
#  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
#  theme(legend.position = "none")


#write.csv(as_tibble(Scores[,c(2,3,5)]),"RaincloudDemo.csv")






#### make a raincloud plot ####

detach(package:plyr)

packages <- c("ggplot2", "lavaan", "dplyr", "cowplot", "rmarkdown",
              "readr", "caTools", "bitops")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

Offset<-ifelse(ScoresOverall$Group=="Old",1,2)

#Accuracy
library(PupillometryR)
library(wesanderson)

pal <- wes_palette("Darjeeling1", 2, type = "discrete")

p6 <- ggplot(ScoresOverall,aes(x=Group,y=score, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Accuracy')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=pal)+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Accuracy Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))



anova<-aov(data=ScoresOverall,score~Group)
summary.aov(anova)





p8 <- ggplot(ScoresOverall,aes(x=Group,y=rtmean, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Response Time (in msec)')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=pal)+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Response Times Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))



library(patchwork)

p6 + p8

#library(plyr)

#offset<-revalue(Scores$correct,c("New"=1,"Old"=2,"Similar"=3))

Offset<-ifelse(Scores$Group=="Old",1,2)

p10 <- ggplot(Scores,aes(x=Group,y=score, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .15, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.15,y=score, fill=Group),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Accuracy')+xlab('Age Group')+ ylim(0,1) +
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Accuracy Across Stimuli Types")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p10a<-p10 + facet_wrap(~correct)

p12 <- ggplot(Scores,aes(x=Group,y=rtmean, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Response Time (in msec)')+xlab('Age Group')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Response Times Across Stimuli Type")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p12a<-p12 + facet_wrap(~correct)

pAccRT<-p10a / p12a

pAccRT

# save the ggplot to png --------------------------------------------------




Mixed.aov.Acc<-aov_car(score ~ Group*correct + Error(ip/correct), data=Scores)


#Mixed.aov.Firstfc<-aov_car(value ~ Group*Horizon + Error(SN/Horizon), data=FirstTrialfc)

knitr::kable(nice(Mixed.aov.Acc)) ##call for formatted ANOVA table using knitr




anova<-aov(data=ScoresOverall,score~Group)
summary.aov(anova)




scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")


#LDI

# False Alarms (Old stimuli called new)
MSTTestAnsOld$TrueOld<-ifelse(MSTTestAnsOld$button_pressed=="0",1,0)

MSTTestAnsNew$FalseOlds<-ifelse(MSTTestAnsNew$button_pressed=="0",1,0)

detach(package:plyr)
library(dplyr)

Rec1<- MSTTestAnsOld %>%
  select(Group,run_id,ip,correct,Ans,TrueOld) %>%
  group_by(Group,run_id) %>%
  summarise(Count = n(),TrueOldScore=mean(TrueOld))


Rec2<- MSTTestAnsNew %>%
  select(Group,run_id,ip,correct,Ans,FalseOlds) %>%
  group_by(run_id,Group) %>%
  summarise(Count = n(),FalseOldScore=mean(FalseOlds))

Rec1$FalseOldScore<-Rec2$FalseOldScore

Rec1$REC<-Rec1$TrueOldScore - Rec1$FalseOldScore

#Rec1Old<-


ggplot(Rec1, aes(x = REC, y = Group,fill = Group)) +
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines=TRUE, vline_size= 1, vline_color="red",
    alpha = 0.7, scale = 0.8, 
    point_size = 1.5, point_alpha= 1, 
    position =position_raincloud(adjust_vlines = TRUE)
  ) +
  scale_fill_manual(values = pal)



Rec1Old<-filter(Rec1,Group=="Old")
Rec1Young<-filter(Rec1,Group=="Young")

t.test(Rec1Old$REC,Rec1Young$REC)










# Call it similar when it is actually new
MSTTestAnsNew$FalseSimilars<-ifelse(MSTTestAnsNew$button_pressed=="1",1,0)

FalseSimilars<- MSTTestAnsNew %>%
  select(Group,run_id,ip,correct,Ans,FalseSimilars) %>%
  group_by(run_id,Group,ip) %>%
  summarise(Count = n(),FalseSimilarsScore=mean(FalseSimilars))


SimilarScores<-filter(Scores, correct=="Similar")

#FalseSimilars<-filter(FalseSimilars, ip %in% SimilarScores$id)

SimilarScores$FalseSimilars<-FalseSimilars$FalseSimilarsScore
SimilarScores$run_id<-FalseSimilars$run_id

SimilarScores$LDI<-SimilarScores$score-SimilarScores$FalseSimilars

SimilarScoresOld<-filter(SimilarScores,Group=="Old")
SimilarScoresYoung<-filter(SimilarScores,Group=="Young")


t.test(SimilarScoresOld$LDI,SimilarScoresYoung$LDI)
wilcox.test(SimilarScoresOld$LDI,SimilarScoresYoung$LDI)

MSTTestAns$rt<-as.numeric(MSTTestAns$rt)
Scores <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(run_id,correct,Group) %>%
  summarise(Count = n(),score=mean(Ans),run_id=mean(run_id), rtmean=mean(rt))

ScoresOld<-filter(Scores, Group=="Old")
ScoresYoung<-filter(Scores, Group=="Young")

mean(SimilarScoresOld$LDI)

ScoresOld$id<-sort(rep(c(1:70),3))
ScoresYoung$id<-sort(rep(c(77:155),3))

Scores<-rbind(ScoresOld, ScoresYoung)

Mixed.aov.Acc<-aov_car(score ~ Group*correct + Error(id/correct), data=Scores)

knitr::kable(nice(Mixed.aov.Acc))

Mixed.aov.RT<-aov_car(rtmean ~ Group*correct + Error(id/correct), data=Scores)

knitr::kable(nice(Mixed.aov.RT))




#ScoresFiltered <- filter(ScoresOverall, score > .55)

#SimilarScoresFiltered <- filter(SimilarScores, ip %in% ScoresFiltered$ip )


p7 <- ggplot(SimilarScores,aes(x=Group,y=LDI, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = ifelse(SimilarScores$Group=="Young",2,1)+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('Lure Discimination Index')+xlab('Age Group')+
  ylim(-.5,1)+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Lure Discrimination Index")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p7


p9 <- ggplot(Rec1,aes(x=Group,y=REC, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE) +
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22) +
  geom_boxplot(aes(x = ifelse(Rec1$Group=="Young",2,1)+0.25, colour=),outlier.shape = NA, alpha = 0.5, width = .1) + 
  ylab('Recognition Index')+xlab('Age Group')+
  ylim(0,1.5)+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Recognition Index")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))


TogAcc <- p7 + p9

TogAcc














#####break it down by trial half

MSTTestAns$trialhalf<-ifelse(MSTTestAns$trial_index<349,1,2)

MSTTestAnsOld<-filter(MSTTestAns, MSTTestAns$correct == "Old")
MSTTestAnsSimilar<-filter(MSTTestAns, MSTTestAns$correct == "Similar")
MSTTestAnsNew<-filter(MSTTestAns, MSTTestAns$correct == "New")



# Call it similar when it is actually new
MSTTestAnsNew$FalseSimilars<-ifelse(MSTTestAnsNew$button_pressed=="1",1,0)

FalseSimilars<- MSTTestAnsNew %>%
  select(Group,run_id,ip,correct,Ans,FalseSimilars,trialhalf) %>%
  group_by(run_id,Group,trialhalf) %>%
  summarise(Count = n(),FalseSimilarsScore=mean(FalseSimilars))

FalseSimilars<-filter(FalseSimilars, ip %in% SimilarScores$ip )

SimilarScores<-filter(Scores, correct=="Similar")
SimilarScores$FalseSimilars<-FalseSimilars$FalseSimilarsScore
SimilarScores$run_id<-FalseSimilars$run_id

SimilarScores$LDI<-SimilarScores$score-SimilarScores$FalseSimilars

SimilarScoresOld<-filter(SimilarScores,Group=="Old")
SimilarScoresYoung<-filter(SimilarScores,Group=="Young")

p9 + p7



p9

Rec1$Group


SimilarOld<-filter(SimilarScores, Group=="Old")
SimilarOld<-inner_join(SimilarOld,DemsPilotOld,by="run_id")
SimilarOld<-SimilarOld[c(1:23,25:77),]

SimilarYoung<-filter(SimilarScores, Group=="Young")
SimilarYoung<-inner_join(SimilarYoung,DemsPilotYoung,by="run_id")
SimilarYoung<-SimilarYoung[c(1:45,47:86),]


SimilarDems<-rbind(SimilarOld,SimilarYoung)

AgeStats <- SimilarDems %>%
group_by(Group.x) %>%
summarise(AgeM = mean(Age),AgeSD = sd(Age), AgeMin = min(Age), AgeMax = max(Age))
  

mappingEd<- c("No High School" = 10,         
              "High School" = 12,
              "Some College" = 13,           
              "AA" = 14,
              "BA or BS" = 16,        
              "Master's" = 18,
              "Professional Degree" = 21,
              "PhD" = 21)

SimilarDems$Ed <- mappingEd[SimilarDems$Education]

EdStats <- SimilarDems %>%
  group_by(Group.x) %>%
  summarise(EdM = mean(Ed),EdSD = sd(Ed), EdMin = min(Ed), EdMax = max(Ed))

SimilarDemsOld<-filter(SimilarDems, Group.x=="Old")
SimilarDemsYoung<-filter(SimilarDems, Group.x=="Young")

t.test(SimilarDemsOld$Ed,SimilarDemsYoung$Ed)

GenderStats <- SimilarDems %>%
  group_by(Group.x,Gender) %>%
  summarise(Count=n())

GenderStats<-as.table(GenderStats)


RaceStats <- SimilarDems %>%
  group_by(Group.x,Race) %>%
  summarise(Count=n())

EthnicityStats <- SimilarDems %>%
  group_by(Group.x,Ethnicity) %>%
  summarise(Count=n())

library(afex)

sort(rep(c(1:161),3))


Scores$rtmean

t.test(SimilarScoresOld$rtmean,SimilarScoresYoung$rtmean)

unique(SimilarDems$Education)