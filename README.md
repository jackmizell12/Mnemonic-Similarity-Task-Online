# Replication of Age Effects in the Mnemonic Similarity Task in an Online Sample

This project was completed for my PhD in Psychology at the University of Arizona. I handled experimental design, data collection, analysis, and drafting of the manuscript. This project was conducted under the mentorship of Dr. Robert C. Wilson in the Neuroscience of Reinforcement Learning and Decision Making Lab at the University of Arizona.

The task itself was designed in Javascript using JSPsych and hosted on Cognition.Run. The host requires a particular structure to their code so this file will not run on a desktop, but will if the files are uploaded to Cognition.run

## Project Summary

Performance on The Mnemonic Similarity Task (MST) has been shown to be sensitive to age-related changes in hippocampal function. The traditional MST has participants see a set of images during an encoding phase and then test their memory for these images in a test phase where participants must judge whether the images they are seeing are repetitions of previous images, similar images to those they saw, or completely new images. Age-related changes have been shown to relate to the ability to discriminate similar images (Lures) from previously seen images. This sensitivity has led to widespread use of the task, and several of its variants to probe age-related changes within the temporal lobes. However, whether these effects replicate in an online setting are currently unknown. In this study we used an online version of the original MST Objects version (MST-O) on an online sample of Older and Younger adults. Using this paradigm, we replicate previously reported age effects in the online setting. Specifically, and in line with lab-based studies, we see age-dependent effect on lure discrimination, while finding no such age effect on recognition abilities.. Additionally, we were able to replicate a similarity-dependent effect on Lure Discrimination, where the more similar a Lure is to its original image, the less likely it is to be correctly identified,regardless of age group. Our online version of the MST-O captures similar behavior, and age-related differences to the lab-based experiment which opens up the potential for using the MST on larger and more diverse samples than are typically possible in lab-based

## Introduction

The Mnemonic Similarity task (MST) is a widely used task in the study of cognitive aging and neuroscience. In this task participants are asked to differentiate between previously seen images from novel images with a subset of these novel images sharing a high degree of similarity with previously seen images. Those images that share a high level of similarity with previously seen images create a high level of memory interference and this discrimination process is thought to tax pattern separation processes within the Dentate Gyrus, and more broadly across the Temporal lobe.

Healthy cognitive aging has a selective impact on task performance in the MST. Age specifically reduces the Lure Discrimination Index (LDI), a measure of the ability to discriminate these Similar novel images (termed Lures) from those images they had previously seen. The LDI is taken as an indirect index of pattern separation. This is in line with neural work showing age lessens perforant path input to the Dentate gyrus as well as to hyperactivity in the CA3 in more cognitively impaired older adults. LDI has been shown to relate to both structural and functional integrity within this network() Age, however has little or no impact on the Recognition index (REC) in the task, a measure of being able to differentiate new, dissimilar objects from the images they had seen before(Stark and Stark 2017; Webb et al. 2020).  The LDI has also been shown to be sensitive in discriminating healthy cognitive aging from amnestic Mild Cognitive Impairment (aMCI), a condition with high risk for Alzheimer's Disease. These findings have been found to hold over a variety of task variations and conditions showing that this effect is robust.

Given the importance of these findings, it should come as no surprise the MST has become widely used across labs. However, we have found no publications of the MST being used in an online setting. Online behavioral testing provides several benefits, including access to larger samples, access to atypical samples for psychology, less of a burden on both the experimenters and participants in terms of scheduling and coordinating experimental sessions, and finally safety when it comes to a situation like the COVID-19 pandemic().

However, online behavioral research comes with its own set of drawbacks including less control over the experimental context (greater variance in unwanted distractions), poor data quality and less vetting of the participant information. However, many of these concerns can be alleviated somewhat through careful study design, use of verified subject pools and attention checks throughout the experiment().

The goal of the current pilot study was to design an online variant of the MST that could replicate the age-related deficit in LDI while showing no such deficit in general recollection abilities. The MST provides an excellent foundation for testing the efficacy of online samples of older adults for cognitive testing given that its effect has been replicated in many lab-based studies before and it also shows a selective age-related effect within the task, allowing one to reduce worries of overall attention or motivation between older and younger participants causing the differences seen in overall task performance.

## Methods

### Participants

We recruited 100 YA between the ages of 18-30 and 100 OA participants between the age of 60-80. Of these, 84 younger adults(Age: M=23.7, SD=3.77) and 77 older adults  (Age: M=65.7,SD=4.96)passed attention-check criteria and were included for further analysis. The YA group consisted of 46 women, 36 men, 1 non-binary person, and one person not identifying. The OA  77 OA consisted of 42 women, 32 men, and 3 people not identifying.

In both groups, the majority of participants identified as White (OA=68, YA=50), while qualitatively it appears the younger adult sample was more racially and ethnically diverse than the older adult sample. In the OA group, 4 people identified as Black, 1 as Indigenous, 2 as Middle Eastern, and 1 did not identify. 2 of the OA participants also identified as Hispanic and/or Latinx. In the YA group, 18 people identified as Asian, 9 as Black or, and 8 as Middle Eastern. 14 of the YA participants also identified as Hispanic and/or Latinx.

Participants were also asked their Education level which was then transformed into years of education. YA (M=14.8, SD=2.38) and OA (M=15.4, SD=2.34) did not significantly differ in their education levels ( t(1,156)=1.71, p=.08).

### Procedure

Participants for this study were recruited using Prolific, an online participant recruitment platform that emphasizes fair payment for its participants and in doing so offers higher data quality for researchers. The task took approximately 20-25 minutes so we paid participants \$4.50 for acceptable performance (the equivalent of \$10.80-\$13.50 U.S. dollars per hour) in the task (passing the majority of the attention checks in each phase of the experiment.) Participants first answered a short set of demographics before moving on to the task. The study was advertised to people currently residing within the US and UK. For the younger adult group (YA) , participants had to be between the ages of 18 to 30. For the older adult group (OA) they had to be between the ages of 60 to 80. Participants were first shown an Informed Consent and had to agree to it before being allowed to begin the experiment. The experiment was approved by the University of Arizona IRB.

### The Online Mnemonic Similarity Task (MST-O)

In this variant of the Mnemonic Similarity Task, we attempted to closely mimic the the original Mnemonic Similarity Task for Objects (MST-O) which includes an incidental Encoding Phase and a Test Phase. In the Encoding Phase, participants saw a set of 128 images for 3 seconds each and were asked to judge if the object was more likely to belong indoors or outdoors by pressing either the 'i' or 'o' keys on their keyboard respectively. Even if participants responded quicker than the 3 seconds, the image remained on the screen to ensure more equivalent encoding. Their answer was unimportant, but what was important is that they were attending to the images and encoding their features. To ensure participants were paying attention during the Encoding Phase, participants also had 10 attention checks throughout in which two images not in the main trials were each displayed 5 times with text imposed on them asking them to "press the 'm' key on this trial". Participants had to pass 60% or more of these attention-check trials in order to be included in the analysis. In total, participants had 138 trials in the Encoding Phase.

After the Encoding Phase, participants had both text instructions and video instructions for the Test Phase drawn and slightly altered from the MST repository on the Starke Lab's website. Participants had to watch the entire 2-minute video in order to move on to the Test Phase.

The Test Phase consisted of 192 test trials with 12 attention-checks trials, totaling 204 trials. In the test trials, participants were shown an image for 2 seconds and afterwards they had 5 seconds to judge whether the image they were seeing was "Old", "Similar", or "New" by using a mouse to click on virtual buttons present on the screen. "Old" objects were those that they had seen previously in the Encoding Phase (repetitions). "Similar" objects were those that were different, but shared a high degree of similarity with those they had seen in the Encoding Phase (lures). These Similar images have been normed for their similarity allowing images to be able to be binned based on shared similarity, see Figure 2 for an example(). "New" objects were those that they had not previously seen in the Encoding Phase. Participants saw 64 images of each type. Attention checks consisted of a set of 12 images with text instructing participants to respond with a particular answer in the response ("Old","Similar", or "New"), 4 images for each response type. See Figure 1 for an example of each stimulus pair for the Encoding and Test phases. In order to be included in the analysis, participants had to get 8 out of the 12 attention checks correct.

Images were taken from the Mnemonic Similarity Task Repository available at (). We used Set 1 from the freely available set of normed images with the images used for the Attention Check images taken from Sets 2 & 3 from the repository. The Task itself was written in jsPsych and hosted on Cognition.run. All analyses for the study were conducted in R.

![](MST/MSTPilot/MSTExample.jpg)

![](MST/MSTPilot/MSTExample2.JPG)

![Figure 1a. The Mnemonic Similarity Task : Example stimuli during the Incidental Encoding Phase and a corresponding image in the Test Phase representing the three possible Test conditions. Note that these are toy examples and not actually used in the experiment.Figure 1b: Examples of images for each of the lure bins, ranging from the most similar (lure bin 1) to least similar (lure bin 5). Note that these are toy examples and have not actually been normed with the exception of the image of the rubber ducky.](MSTExample2.JPG)

### Task Measures

#### The Lure Discrimination Index (LDI):

The Lure Discrimination index (LDI) was calculated as the difference between the rate of "Similar" responses given to Lures (correctly identifying similar stimuli) minus the "Similar" responses given to "New" items, or Foils. This is done to correct for any response bias a participant may have where they may just develop a strategy of calling all "New" items "similar". This measure is sensitive to aging, with older adults tending to show a lower LDI than younger adults so in the current online study we looked to replicate this age effect on LDI that has been found across many lab and clinic-based studies.

#### The Recognition Index (REC):

The Recognition Index (REC) for repeat items was calculated as the difference between the rate of "Old" responses given to repetitions (correct identification of an exact item seen during the Encoding Phase minus "Old" responses given to "New" items, or foils. This corrects for response bias in participants. This measure has been shown to not be affected in healthy aging samples, so in the current study we were hoping to show no effect of age group on REC.

Taken together the goal of the current study is to replicate lab-based age-related effects of MST behavior using a a novel online variant of the task on a sample of older and younger adults.

## Sample Results

### Accuracy and Response Times Across Age Groups

![](MST/MSTPilot/figs/Fig1.png)

To first gain a baseline understanding of the task, we looked at Overall accuracy and response times. Performance was above chance in both age groups, with both younger and older adults correctly identifying new and old stimuli XXX% of the time, and similar XX% of the time. Importantly, we did have a subset of older and younger adults who performed below chance (33%) and have included an analysis showing that exclusion of these participants does not affect the outcomes of the analyses (see Supplementary Analyses)

To understand if stimulus type ("New","Similar","Old") and Age Group impacted Accuracy in the Online MST-O, we conducted a mixed effect ANOVA with age group as a between-subjects factor and Stimuli Type as a within-subjects factor. We found a main effect of Stimuli Type F(2,308)=290.64,p\<.001, with no effect of Age Group (p=.256) and no Interaction effect between Age Group and Stimuli Type (p=.080).

To understand if stimulus type ("New","Similar","Old") and Age Group impacted response times in the Online MST-O, we conducted a mixed effect ANOVA with Age Group as a between-subjects factor and Stimuli Type as a within-subjects factor. We found a main effect of Stimuli Type F(2,300)=25.57,p\<.001, a main effect effect of Age Group F(1,159)=59.42,p\<.001, and no Interaction effect between Age Group and Stimuli Type (p=.816).

### LDI and REC

### ![](MST/MSTPilot/figs/Fig2.png)

To examine the impact of Age Group on the Recognition Index score, we conducted a Two-Samples T-Test and found that the two age groups did not differ in their Recognition Index (p=.77).

To examine the impact of Age Group on the Lure Discrimination Index, we conducted a Two-Samples T-Test and found that the Older Adult Group did show a significantly lower (M=.31) LDI than the younger adults(M=.37), t(1,159)=-2.27, p\<.05.

Importantly, all of these effects survive even when we exclude participant whose overall task performance is at or below chance (33% accuracy on all Test trials).

### Lure Discrimination Across Levels of Similarity

![](MST/MSTPilot/figs/AccuracyByBins.png)

To determine if we could replicate difficulty-based accuracy on "Similar" trials, we broke down Similar stimuli trials by their Lure Bin (with Bin 1 being the most similar and Bin 5 being the least Similar) The level of similarity being between Encoding Images and their Similar Lures in the Test Phase, with the greater the similarity being the most difficult. We conducted a mixed-effects ANOVA with Group as a between-subjects factor and lure bin as a within-subjects factor. We found a main effect of Age Group with OA being less accurate overall (obvious given the LDI result). We also found a main effect of Lure Bin with less similar bins having a an overall higher level of accuracy, F(4,551)=193.60, p\<.001. Consistent with previous findings, we found no interaction between Age Group and Lure Bin (p=.697)

## Discussion

In this study, we have shown we can replicate age-group and similarity-dependent effects in an online sample using a variant of the MST-O. This opens up the potential for studying the MST in larger and potentially more diverse samples than have typically been done in any in-person studies. These larger sample sizes will also allow for the examination of subtle behavioral effects in the task, such as sequential effects.

However, the question still remains as to how performance in this online setting directly compares to performance in lab-based variants. Overall accuracy with both "Old" and "New" stimuli types from our study followed closely in line with lab-based studies. However, this is not the case with the Similar trials where in this online variant we have found that both age groups are performing better than what has been shown in lab-based studies, specifically noticeable for older adults.

Qualitatively, we found a smaller effect size for age-based LDI differences than has been found within in-person studies. There could be several reasons for this. One is that this could be related to the difference in sample sizes across these studies, with the in-person studies typically having half or less than the current study's sample in each age group. This could lead to inflated effect sizes. There could also be differences in this older adult's groups comfort with technology compared to in-person samples, as well as differences in motivation that differentially impact their motivation for the more difficult similar trials. There is also the question of whether we can capture meaningful individual differences with the MST-O, which will require testing the same individuals over multiple time points. These are all important considerations and experiments addressing these issues will be important future directions for this line of research.
