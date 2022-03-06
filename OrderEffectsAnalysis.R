#Logistic Multilevel Regression model in R, 
#the goal is to see if the previous trial is impacting
#the behavior on the current trial and whether this differs by age group
# the majority of this code is cleaning the data and ensuring that the data is 
# indexed correctly


#install packages if necessary
list.of.packages <- c("here", "tidyverse","PupillometryR","cowplot","lme4","scico")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#load libraries
library(tidyverse)
library(PupillometryR)
library(here)
library(cowplot)
library(scico)
library(lme4)

#get data in
MSTTestAns <- read_csv(here("ForLogistic.csv"))

#get down to just trials where it is coded if they were correct, and also what their exact response was. 
Correctvec<-MSTTestAns$correct
Correctvec<-Correctvec[-(1)]
Correctvec<-append(Correctvec,NA,after=length(Correctvec))

Responsevec<-MSTTestAns$button_pressed
Responsevec<-Responsevec[-(1)]
Responsevec<-append(Responsevec,NA,after=length(Responsevec))


MSTTestAns$PreviousStim<-Correctvec
MSTTestAns$PreviousResponse<-Responsevec

#get binary on trial type and previous response being the same
MSTTestAns$PreviousStimSame<-ifelse(MSTTestAns$PreviousStim == MSTTestAns$correct,1,0)
MSTTestAns$PreviousRespSame<-ifelse(MSTTestAns$PreviousResponse == MSTTestAns$button_pressed,1,0)




# mapping of the familiarity rating between an item and its similar image
mappingStim<- c("065b.jpg" = 5,
                "066b.jpg" = 3,
                "067b.jpg" = 3,
                "068b.jpg" = 2,
                "069b.jpg" = 2,
                "070b.jpg" = 4,
                "071b.jpg" = 4,
                "072b.jpg" = 3,
                "073b.jpg" = 2,
                "074b.jpg" = 4,
                "075b.jpg" = 5,
                "076b.jpg" = 1,
                "077b.jpg" = 5,
                "078b.jpg" = 3,
                "079b.jpg" = 1,
                "080b.jpg" = 5,
                "081b.jpg" = 2,
                "082b.jpg" = 4,
                "083b.jpg" = 3,
                "084b.jpg" = 2,
                "085b.jpg" = 1,
                "086b.jpg" = 5,
                "087b.jpg" = 3,
                "088b.jpg" = 1,
                "089b.jpg" = 3,
                "090b.jpg" = 5,
                "091b.jpg" = 5,
                "092b.jpg" = 3,
                "093b.jpg" = 4,
                "094b.jpg" = 3,
                "095b.jpg" = 4,
                "096b.jpg" = 4,
                "097b.jpg" = 3,
                "098b.jpg" = 4,
                "099a.jpg" = 1,
                "100b.jpg" = 1,
                "101b.jpg" = 2,
                "102b.jpg" = 3,
                "103b.jpg" = 5,
                "104b.jpg" = 1,
                "105b.jpg" = 4,
                "106b.jpg" = 4,
                "107b.jpg" = 2,
                "108b.jpg" = 2,
                "109b.jpg" = 4,
                "110b.jpg" = 1,
                "111b.jpg" = 2,
                "112b.jpg" = 5,
                "113b.jpg" = 3,
                "114b.jpg" = 3,
                "115b.jpg" = 1,
                "116b.jpg" = 4,
                "117b.jpg" = 4,
                "118b.jpg" = 4,
                "119b.jpg" = 1,
                "120b.jpg" = 3,
                "121b.jpg" = 4,
                "122b.jpg" = 2,
                "123b.jpg" = 4,
                "124b.jpg" = 2,
                "125b.jpg" = 4,
                "126b.jpg" = 5,
                "127b.jpg" = 2,
                "128b.jpg" = 2 )



MSTTestAns$LureBin <- mappingStim[MSTTestAns$Stim]


Scores <- MSTTestAns %>%
  select(Stim,Group,run_id,ip,correct,Ans,rt) %>%
  group_by(run_id,correct,Group,ip) %>%
  summarise(score=mean(Ans),rtmean=mean(rt))

ScoresOld <- filter(Scores, correct == "Old")

ScoresAcc <- filter(ScoresOld, score > .33 )


MSTTestAns <-filter(MSTTestAns, ip %in% ScoresAcc$ip)


MSTTestAnsSimilar<-filter(MSTTestAns, correct=="Similar")

MSTTestAnsSimilar$ID<-as.numeric(factor(paste0(MSTTestAnsSimilar$Group, MSTTestAnsSimilar$run_id)))

data <-filter(MST)

MSTTestAnsSimilar %>%
  mutate_if()
mutate(ID = group_indices(.dots=c("Group", "run_id"))) 

#data frame
ForLogModel<- MSTTestAnsSimilar %>%
  select(Ans, correct, PreviousStimSame, PreviousRespSame, Group, ID) %>% 
  mutate_if(is.character, factor)

ForCheck<-ForLogModel %>% 
  group_by(ID,Group, PreviousRespSame) %>%
  summarise( Acc= mean(Ans))

ggplot(ForCheck, aes(x=as.factor(PreviousRespSame), y=Acc, fill=Group)) + 
  geom_boxplot()



ForCheckSame<-filter(ForCheck,PreviousRespSame==1)
ForCheckDifferent<-filter(ForCheck,PreviousRespSame==0)

t.test(ForCheckSame$Acc,ForCheckDifferent$Acc)

ForCheck<-ForLogModel %>% 
  group_by(ID,Group, PreviousRespSame) %>%
  summarise( Acc= mean(Ans))

ggplot(ForCheck, aes(x=as.factor(PreviousRespSame), y=Acc, fill=Group)) + 
  geom_boxplot()

aov<-anov

ForCheckSame<-filter(ForCheck,PreviousRespSame==1)
ForCheckDifferent<-filter(ForCheck,PreviousRespSame==0)

t.test(ForCheckSame$Acc,ForCheckDifferent$Acc)


#actual analysis

#actual analysis


MSTTestAnsSimilar<-MSTTestAnsSimilar %>% 
  select(Ans,LureBin,PreviousStimSame, PreviousRespSame,ID, Group)


MSTTestGroupSimilar<-MSTTestAnsSimilar %>% 
  select(ID,Group) %>% 
  group_by(ID,Group) %>%
  summarise(ID = mean(ID))



Test<-na.omit(MSTTestAnsSimilar)
length(unique(Test$ID))

Test$Ans<-ifelse(Test$Ans==1,1,-1)

Testlogit <- lmList(Ans ~ LureBin + PreviousStimSame + PreviousRespSame | ID, data=Test)

summary(Testlogit)

coefficients <- coef(Testlogit)

IndLogData<-cbind(coefficients,MSTTestGroupSimilar)


longdata2 = IndLogData %>%
  gather("(Intercept)", "LureBin", "PreviousStimSame","PreviousRespSame", key = Betas, value = coefficients)
longdata2

Offset<-ifelse(longdata2$Group=="Old",1,2)

p16 <- ggplot(longdata2,aes(x=Group,y=coefficients, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(aes(color=Group,fill=Group),position = position_jitter(width = .05), size = 5.00, alpha=.5, shape=22)+
  geom_boxplot(aes(x = Offset+0.25,colour=),outlier.shape = NA, alpha = 0.5, width = .1)+ 
  ylab('regression weights')+xlab('predictors')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=c("#00A08A","#FF0000"))+
  scale_colour_manual(values=c('#141414','#141414'))+
  ggtitle("Regression model of Similar Trial Behavior Across Groups")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p16a<-p16 + facet_wrap(~Betas, nrow=1)

p16a



pal <- scico(6,palette="vik")

p17 <- ggplot(longdata2,aes(x=Betas,y=-coefficients, fill = Betas))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .05), size = 2.50, alpha=.15, shape=22)+
  geom_boxplot(outlier.shape = NA, alpha = 0.9, width = .1)+ 
  ylab('regression weights')+xlab('predictors')+
  theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values=pal) +
  scale_colour_manual(values=c('#141414','#141414','#141414','#141414'))+
  ggtitle("Regression model of Similar Trial Behavior")+
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black", size = 24, face = "bold"),
        axis.title =element_text(color = "black", size = 20, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=16))

p17a<- p17  + geom_hline(yintercept = 0,linetype="dotted", size=1.5) + coord_flip() 

p17a 

















cor.test(coefficients$PreviousStimSame,coefficients$PreviousRespSame)


