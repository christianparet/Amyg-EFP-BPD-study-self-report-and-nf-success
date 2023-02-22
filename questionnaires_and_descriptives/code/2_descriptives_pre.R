#################################################################
# Analysis of questionnaire pre-assessment and demographics of EFPTest study
# Zopfs, Paret, ZI Mannheim, 2022
#################################################################
# Set Working Directory-----------------------------------------
#################################################################

setwd("Y:/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data/") # Set to path where EFP_data.Rda is expected. Alternatively comment out this line and load EFP_data.RdA to workspace.

#################################################################
# Loading packages (installs if necessary)
#################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               arsenal,
               xfun,
               rstatix) 

###################################################################################################################################################################
#
# Preprocessing 
#
###################################################################################################################################################################

#################################################################
# Loading packages (installs if necessary)
#################################################################

load("EFP_data.Rda")

#Construct separate dataframes for each questionnaire

ALScomplete<-  EFP_data%>%
  select(contains('ALS'))

STAIcomplete<-  EFP_data%>%
  select(contains('STAI'))

BDIcomplete<-  EFP_data%>%
  select(contains('BDI'))

TAScomplete<-  EFP_data%>%
  select(contains('TAS'))

IMIcomplete<-  EFP_data%>%
  select(contains('IMI'))
  IMIcomplete$`Bulimia Nervosa F50.2`<- NULL


#################################################################################
#BDI
#################################################################################

#BDI Recoding
BDIcomplete$pre_BDI_16recoded <- car::recode(BDIcomplete$pre_BDI_16,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4") 
BDIcomplete$pre_BDI_18recoded <- car::recode(BDIcomplete$pre_BDI_18,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4")
BDIcomplete$post_BDI_16recoded <- car::recode(BDIcomplete$post_BDI_16,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4") 
BDIcomplete$post_BDI_18recoded <- car::recode(BDIcomplete$post_BDI_18,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4")

BDIrecoded <- BDIcomplete %>%mutate_at(vars(-pre_BDI_16recoded,
                                            -pre_BDI_18recoded,
                                            -post_BDI_16recoded,
                                            -post_BDI_18recoded),~ifelse(. == 1, 0, ifelse(. == 2, 1, ifelse(. == 3, 2, ifelse(. == 4, 3, .))))) 

#remove old BDI_16 and BDI_18
BDIrecoded$pre_BDI_16 <- NULL
BDIrecoded$pre_BDI_18 <- NULL
BDIrecoded$post_BDI_16 <- NULL
BDIrecoded$post_BDI_18 <- NULL

#Sumscore BDI

BDIrecoded$pre_BDI_Total <- BDIrecoded %>%
  select(contains('pre_BDI')) %>%
  rowSums()

BDIrecoded$post_BDI_Total<- BDIrecoded %>%
  select(contains('post_BDI')) %>%
  rowSums()

#################################################################################
#ALS
#################################################################################
#ALS Recoding & Subscales
ALSrecoded <- ALScomplete %>%
  mutate_all(~ifelse(. == 1, 3, ifelse(. == 2, 2, ifelse(. == 3, 1, ifelse(. == 4, 0, .)))))

##Forms subscale ALSDeprs from dataset ALSrecoded by summing relevant items
ALSrecoded$pre_ALS_subscale_Deprs <- rowSums (subset(ALSrecoded, select = c(pre_ALS_01, pre_ALS_08, pre_ALS_09, pre_ALS_13, pre_ALS_19, pre_ALS_25, pre_ALS_31, pre_ALS_40, pre_ALS_42, pre_ALS_46, pre_ALS_54)))
ALSrecoded$post_ALS_subscale_Deprs <- rowSums (subset(ALSrecoded, select = c(post_ALS_01, post_ALS_08, post_ALS_09, post_ALS_13, post_ALS_19, post_ALS_25, post_ALS_31, post_ALS_40, post_ALS_42, post_ALS_46, post_ALS_54)))

##subscale Hypomania
ALSrecoded$pre_ALS_subscale_Hypom <- rowSums (subset(ALSrecoded, select = c(pre_ALS_02, pre_ALS_06, pre_ALS_07, pre_ALS_18, pre_ALS_27, pre_ALS_32, pre_ALS_35, pre_ALS_36, pre_ALS_39, pre_ALS_45, pre_ALS_52, pre_ALS_53)))
ALSrecoded$post_ALS_subscale_Hypom <- rowSums (subset(ALSrecoded, select = c(post_ALS_02, post_ALS_06, post_ALS_07, post_ALS_18, post_ALS_27, post_ALS_32, post_ALS_35, post_ALS_36, post_ALS_39, post_ALS_45, post_ALS_52, post_ALS_53)))

##subscale Biphs
ALSrecoded$pre_ALS_subscale_Biphs <- rowSums (subset(ALSrecoded, select = c(pre_ALS_11, pre_ALS_24, pre_ALS_29, pre_ALS_30, pre_ALS_34, pre_ALS_47, pre_ALS_48, pre_ALS_51)))
ALSrecoded$post_ALS_subscale_Biphs <- rowSums (subset(ALSrecoded, select = c(post_ALS_11, post_ALS_24, post_ALS_29, post_ALS_30, post_ALS_34, post_ALS_47, post_ALS_48, post_ALS_51)))

##subscale Anxiety
ALSrecoded$pre_ALS_subscale_Anxty <- rowSums (subset(ALSrecoded, select = c(pre_ALS_04, pre_ALS_05, pre_ALS_12, pre_ALS_20, pre_ALS_26, pre_ALS_28, pre_ALS_44)))
ALSrecoded$post_ALS_subscale_Anxty <- rowSums (subset(ALSrecoded, select = c(post_ALS_04, post_ALS_05, post_ALS_12, post_ALS_20, post_ALS_26, post_ALS_28, post_ALS_44)))

##subscale Anger
ALSrecoded$pre_ALS_subscale_Anger <- rowSums (subset(ALSrecoded, select = c(pre_ALS_14, pre_ALS_15, pre_ALS_21, pre_ALS_23, pre_ALS_33, pre_ALS_41, pre_ALS_50)))
ALSrecoded$post_ALS_subscale_Anger <- rowSums (subset(ALSrecoded, select = c(post_ALS_14, post_ALS_15, post_ALS_21, post_ALS_23, post_ALS_33, post_ALS_41, post_ALS_50)))

##subcale Anxdp
ALSrecoded$pre_ALS_subscale_Anxdp <- rowSums (subset(ALSrecoded, select = c(pre_ALS_03, pre_ALS_10, pre_ALS_16, pre_ALS_17, pre_ALS_22, pre_ALS_37, pre_ALS_38, pre_ALS_49)))
ALSrecoded$post_ALS_subscale_Anxdp <- rowSums (subset(ALSrecoded, select = c(post_ALS_03, post_ALS_10, post_ALS_16, post_ALS_17, post_ALS_22, post_ALS_37, post_ALS_38, post_ALS_49)))

##ALS total pre sumscore
ALSrecoded$pre_ALS_Total <- ALSrecoded %>%
  select(contains('pre_ALS_subscale')) %>%
  rowSums()

##ALS total post sumscore
ALSrecoded$post_ALS_Total <- ALSrecoded %>%
  select(contains('post_ALS_subscale')) %>%
  rowSums()


#################################################################################
#STAI
#################################################################################


#STAI Recoding & Subscales

STAIrecoded <- STAIcomplete %>%
  mutate_at(vars(pre_STAI_01, pre_STAI_06, pre_STAI_07, pre_STAI_10, pre_STAI_13, pre_STAI_16, pre_STAI_19), ~ifelse(. == 1, 4, ifelse(. == 2, 3, ifelse(. == 3, 2, ifelse(. == 4, 1, .)))))


#Calculate sum score for STAI

STAIrecoded$pre_STAI_Total <- rowSums(STAIrecoded)


#################################################################################
#TAS
#################################################################################

##TAS Recoding & Subscales

TAS_withoutdaydream <- subset(TAScomplete, # remove daydreaming subscale because of negative correlation with other subscales in German population, https://doi.org/10.13109/zptm.2000.46.4.368
                              select=c (pre_TAS_03, 
                                        pre_TAS_04, 
                                        pre_TAS_08, 
                                        pre_TAS_09, 
                                        pre_TAS_10, 
                                        pre_TAS_11, 
                                        pre_TAS_12, 
                                        pre_TAS_13, 
                                        pre_TAS_14, 
                                        pre_TAS_15, 
                                        pre_TAS_17, 
                                        pre_TAS_20, 
                                        pre_TAS_21, 
                                        pre_TAS_22, 
                                        pre_TAS_23, 
                                        pre_TAS_24, 
                                        pre_TAS_25, 
                                        pre_TAS_26,
                                        post_TAS_03, 
                                        post_TAS_04, 
                                        post_TAS_08, 
                                        post_TAS_09, 
                                        post_TAS_10, 
                                        post_TAS_11, 
                                        post_TAS_12, 
                                        post_TAS_13, 
                                        post_TAS_14, 
                                        post_TAS_15, 
                                        post_TAS_17, 
                                        post_TAS_20, 
                                        post_TAS_21, 
                                        post_TAS_22, 
                                        post_TAS_23, 
                                        post_TAS_24, 
                                        post_TAS_25, 
                                        post_TAS_26))



TASrecoded <- TAS_withoutdaydream %>%
  mutate_at(vars(pre_TAS_09, 
                 pre_TAS_11, 
                 pre_TAS_13, 
                 pre_TAS_15, 
                 pre_TAS_21, 
                 pre_TAS_24, 
                 post_TAS_09, 
                 post_TAS_11, 
                 post_TAS_13, 
                 post_TAS_15, 
                 post_TAS_21, 
                 post_TAS_24), 
            ~ifelse(. == 1, 5, ifelse(. == 2, 4, ifelse(. == 3, 3, ifelse(. == 4, 2, ifelse(. == 5, 1,.))))))

#Subscale identification of emotion
TASrecoded$pre_TAS_subscale_ident <- rowSums(subset(TASrecoded, select = c(pre_TAS_04, pre_TAS_10, pre_TAS_14, pre_TAS_17, pre_TAS_20, pre_TAS_25))) 
TASrecoded$post_TAS_subscale_ident <- rowSums(subset(TASrecoded, select = c(post_TAS_04, post_TAS_10, post_TAS_14, post_TAS_17, post_TAS_20, post_TAS_25))) 

#Subscale description of emotion
TASrecoded$pre_TAS_subscale_descr <- rowSums(subset(TASrecoded, select = c(pre_TAS_03, pre_TAS_08, pre_TAS_12, pre_TAS_22, pre_TAS_23)))
TASrecoded$post_TAS_subscale_descr <- rowSums(subset(TASrecoded, select = c(post_TAS_03, post_TAS_08, post_TAS_12, post_TAS_22, post_TAS_23)))

#Subscale thinking style
TASrecoded$pre_TAS_subscale_thnkng <- rowSums(subset(TASrecoded, select = c(pre_TAS_09, pre_TAS_11, pre_TAS_13, pre_TAS_15, pre_TAS_21, pre_TAS_24))) 
TASrecoded$post_TAS_subscale_thnkng <- rowSums(subset(TASrecoded, select = c(post_TAS_09, post_TAS_11, post_TAS_13, post_TAS_15, post_TAS_21, post_TAS_24))) 

#TAS total sum score 

TASrecoded$pre_TAS_Total <- TASrecoded %>%
  select(contains('pre_TAS_subscale')) %>%
  rowSums()

#TAS Total post 

TASrecoded$post_TAS_Total <- TASrecoded %>%
  select(contains('post_TAS_subscale')) %>%
  rowSums()



#################################################################
# IMI
#################################################################
#subset with imi items

#### DELETE commented code below after code review ####
# IMIrecoded <- IMIcomplete %>%
#   mutate_at(vars(pre_IMI_14, 
#                  pre_IMI_16, 
#                  pre_IMI_21, 
#                  pre_IMI_22,
#                  pre_IMI_29, 
#                  post_IMI_14, 
#                  post_IMI_16, 
#                  post_IMI_21, 
#                  post_IMI_22,
#                  post_IMI_29  ), ~ifelse(. == 1, 7, ifelse(. == 2, 6, ifelse(. == 3, 5, ifelse(. == 4, 4, ifelse(. == 5, 3, ifelse(. == 6, 2, ifelse(. == 7, 1, .))))))))
# 

#recode items

IMIrecoded <- IMIcomplete %>%
  mutate_at(vars(pre_IMI_14, 
                 pre_IMI_16, 
                 pre_IMI_21, 
                 pre_IMI_22,
                 pre_IMI_29, 
                 post_IMI_14, 
                 post_IMI_16, 
                 post_IMI_21, 
                 post_IMI_22,
                 post_IMI_29  ), ~ifelse(. == 1, 7, ifelse(. == 2, 6, ifelse(. == 3, 5, ifelse(. == 4, 4, ifelse(. == 5, 3, ifelse(. == 6, 2, ifelse(. == 7, 1, .))))))))


IMIrecoded$Probanden_ID <- EFP_data$Probanden_ID
IMIrecoded$Condition <- EFP_data$Condition

#build subscales pre
IMIrecoded$pre_IMI_subscale_interest <- rowMeans(cbind(IMIrecoded$pre_IMI_02, # comment: usage of cbind unfavorable, better use subset (as cbind does not return error message if variable = NULL)
                                          IMIrecoded$pre_IMI_06,
                                          IMIrecoded$pre_IMI_09,
                                          IMIrecoded$pre_IMI_11,
                                          IMIrecoded$pre_IMI_22,
                                          IMIrecoded$pre_IMI_25,
                                          IMIrecoded$pre_IMI_29))
IMIrecoded$pre_IMI_subscale_competence <- rowMeans(cbind(IMIrecoded$pre_IMI_10,
                                            IMIrecoded$pre_IMI_13,
                                            IMIrecoded$pre_IMI_15,
                                            IMIrecoded$pre_IMI_17,
                                            IMIrecoded$pre_IMI_21,
                                            IMIrecoded$pre_IMI_23))
IMIrecoded$pre_IMI_subscale_effort <- rowMeans(cbind(IMIrecoded$pre_IMI_03,
                                        IMIrecoded$pre_IMI_07,
                                        IMIrecoded$pre_IMI_14,
                                        IMIrecoded$pre_IMI_18,
                                        IMIrecoded$pre_IMI_27))
IMIrecoded$pre_IMI_subscale_tension <- rowMeans(cbind(IMIrecoded$pre_IMI_05,
                                         IMIrecoded$pre_IMI_08,
                                         IMIrecoded$pre_IMI_16,
                                         IMIrecoded$pre_IMI_19))
IMIrecoded$pre_IMI_subscale_value <- rowMeans(cbind(IMIrecoded$pre_IMI_01,
                                       IMIrecoded$pre_IMI_04,
                                       IMIrecoded$pre_IMI_12,
                                       IMIrecoded$pre_IMI_20,
                                       IMIrecoded$pre_IMI_24,
                                       IMIrecoded$pre_IMI_26,
                                       IMIrecoded$pre_IMI_28))

#build subscales post
IMIrecoded$post_IMI_subscale_interest <- rowMeans(cbind(IMIrecoded$post_IMI_02,
                                               IMIrecoded$post_IMI_06,
                                               IMIrecoded$post_IMI_09,
                                               IMIrecoded$post_IMI_11,
                                               IMIrecoded$post_IMI_22r,
                                               IMIrecoded$post_IMI_25,
                                               IMIrecoded$post_IMI_29r))
IMIrecoded$post_IMI_subscale_competence <- rowMeans(cbind(IMIrecoded$post_IMI_10,
                                                 IMIrecoded$post_IMI_13,
                                                 IMIrecoded$post_IMI_15,
                                                 IMIrecoded$post_IMI_17,
                                                 IMIrecoded$post_IMI_21r,
                                                 IMIrecoded$post_IMI_23))
IMIrecoded$post_IMI_subscale_effort <- rowMeans(cbind(IMIrecoded$post_IMI_03,
                                             IMIrecoded$post_IMI_07r,
                                             IMIrecoded$post_IMI_14r,
                                             IMIrecoded$post_IMI_18,
                                             IMIrecoded$post_IMI_27))
IMIrecoded$post_IMI_subscale_tension <- rowMeans(cbind(IMIrecoded$post_IMI_05,
                                              IMIrecoded$post_IMI_08,
                                              IMIrecoded$post_IMI_16r,
                                              IMIrecoded$post_IMI_19))
IMIrecoded$post_IMI_subscale_value <- rowMeans(cbind(IMIrecoded$post_IMI_01,
                                            IMIrecoded$post_IMI_04,
                                            IMIrecoded$post_IMI_12,
                                            IMIrecoded$post_IMI_20,
                                            IMIrecoded$post_IMI_24,
                                            IMIrecoded$post_IMI_26,
                                            IMIrecoded$post_IMI_28))



################################################################################
#Construct dataframes for following analyses 
################################################################################

# Dataframe containing only outcome variables of questionnaires

Data2<- as.data.frame(cbind(ALSrecoded$pre_ALS_subscale_Deprs,
                            ALSrecoded$pre_ALS_subscale_Anger,
                            ALSrecoded$pre_ALS_subscale_Anxdp,
                            ALSrecoded$pre_ALS_subscale_Anxty,
                            ALSrecoded$pre_ALS_subscale_Hypom,
                            ALSrecoded$pre_ALS_subscale_Biphs,
                            ALSrecoded$pre_ALS_Total,
                            TASrecoded$pre_TAS_subscale_ident,
                            TASrecoded$pre_TAS_subscale_descr,
                            TASrecoded$pre_TAS_subscale_thnkng,
                            TASrecoded$pre_TAS_Total,
                            BDIrecoded$pre_BDI_Total,
                            STAIrecoded$pre_STAI_Total,
                            IMIrecoded$pre_IMI_subscale_competence,
                            IMIrecoded$pre_IMI_subscale_effort,
                            IMIrecoded$pre_IMI_subscale_interest,
                            IMIrecoded$pre_IMI_subscale_tension,
                            IMIrecoded$pre_IMI_subscale_value,
                            EFP_data$Condition))



colnames(Data2) <- c("ALSDeprs", "ALSAnger", "ALSAnxdp", "ALSAnxty", "ALSHypom", 
                     "ALSBiphs", "ALSTotal", "TASident", "TASdescr", 
                     "TASthnkng", "TASTotal", "BDITotal", "STAITotal", 'IMIcompetence', 'IMIeffort', 'IMIinterest', 'IMItension', 'IMIvalue', 'Condition')  

Data2<- Data2 %>% mutate_at(c(1:18), as.numeric)
Data2<- na.omit(Data2)
###################################################################################################################################################################
#
# Sample descriptives for questionnaires 
#
###################################################################################################################################################################

mydata.long <- Data2 %>%
  pivot_longer(-Condition, names_to = "variables", values_to = "value")


mydata.long$value <-as.numeric(mydata.long$value)
mydata.long$Condition <-as.factor(mydata.long$Condition)
mydata.long$variables <-as.factor(mydata.long$variables)


#Gives mean and SD for each outcome variable and each group at time-point pre
df.sum<- Data2 %>%
  group_by(Condition)%>%
  select(ALSDeprs, ALSAnger, ALSAnxdp, ALSAnxty, ALSHypom, ALSBiphs, ALSTotal, TASident, TASdescr, TASthnkng, TASTotal, BDITotal, STAITotal) %>% # select variables to summarise
   summarise_each(funs(
    mean = mean, 
    sd = sd))
df.sum

#compares means between groups for each outcome variable at time-point pre
stat.test <- mydata.long %>%
  group_by(variables) %>%
  t_test(value ~ Condition, var.equal = TRUE) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() 
stat.test

###################################################################################################################################################################
#
# sample descriptives for BPD criteria, age, comorbidities, medication 
#
###################################################################################################################################################################


#################################################################
# Data Preparation (counts how many people in each group fall into each category)
#################################################################
descriptives<- subset(EFP_data, select=c(1, 285:372)) # CP: needed to change selections of colomns during code review. Check whether information was lost in descriptives table.
#exclude non-completers # CP: irrelevant, remove comment after code review

#desc_table <- descriptives[,c(2,5,14,19,20,28,44,50,53,58,63,65,70,81,88:90)] # CP: I assume this is the list of completers/treatment group. In this case this code is useless

descriptives$group<-as.factor(descriptives$group)

#mean+SD age, BPD criteria
table1 <- tableby(group~Age_inclusion, data=descriptives) # CP: removed BPD_DSM_criteria because too many missings
summary(table1, title = "descriptive Data", text = TRUE)

t.test(descriptives$Age_inclusion ~ descriptives$group)
# t.test(descriptives$BPD_DSM_criteria ~ descriptives$group) # remove in clean version

#####comorbidities frequencies
descriptivesKG <- subset(descriptives, group == 0)
descriptivesEG <- subset(descriptives, group == 1)

##Affective
#affective disorder all completers (Werte gr??er 1 werden aufsummiert)
affective <- table(descriptives$AffectiveDisorder)
affective
Affective <- prop.table(affective)
Affective <- round(Affective, digits=2)
Affective

#affective disorder KG completers (Werte gr??er 1 werden aufsummiert)
affectivekg <- table(descriptivesKG$AffectiveDisorder)
affectivekg
Affectivekg <- prop.table(affectivekg)
Affectivekg <- round(Affectivekg, digits=2)
Affectivekg

#affective disorder EG completers (Werte gr??er 1 werden aufsummiert)
affectiveeg <- table(descriptivesEG$AffectiveDisorder)
affectiveeg
Affectiveeg <- prop.table(affectiveeg)
Affectiveeg <- round(Affectiveeg, digits=2)
Affectiveeg


##Anxiety
#anxiety disorder all completers (Werte gr??er 1 werden aufsummiert)
anxiety <- table(descriptives$AnxietyDisorder)
anxiety
Anxiety <- prop.table(anxiety)
Anxiety <- round(Anxiety, digits=2)
Anxiety

#anxiety disorder KG completers (Werte gr??er 1 werden aufsummiert)
anxietykg <- table(descriptivesKG$AnxietyDisorder)
anxietykg
Anxietykg <- prop.table(anxietykg)
Anxietykg <- round(Anxietykg, digits=2)
Anxietykg

#anxiety disorder EG completers (Werte gr??er 1 werden aufsummiert)
anxietyeg <- table(descriptivesEG$AnxietyDisorder)
anxietyeg
Anxietyeg <- prop.table(anxietyeg)
Anxietyeg <- round(Anxietyeg, digits=2)
Anxietyeg


#Eating
#eating disorder all completers (Werte gr??er 1 werden aufsummiert)
eating <- table(descriptives$EatingDisorder)
eating
Eating <- prop.table(eating)
Eating <- round(Eating, digits=2)
Eating

#eating disorder KG completers (Werte gr??er 1 werden aufsummiert)
eatingkg <- table(descriptivesKG$EatingDisorder)
eatingkg
Eatingkg <- prop.table(eatingkg)
Eatingkg <- round(Eatingkg, digits=2)
Eatingkg

#eating disorder EG completers (Werte gr??er 1 werden aufsummiert)
eatingeg <- table(descriptivesEG$EatingDisorder)
eatingeg
Eatingeg <- prop.table(eatingeg)
Eatingeg <- round(Eatingeg, digits=2)
Eatingeg


#PTSD
#PTSD disorder all completers (Werte gr??er 1 werden aufsummiert)
ptbs <- table(descriptives$PTBS)
ptbs
Ptbs <- prop.table(ptbs)
Ptbs <- round(Ptbs, digits=2)
Ptbs

#ptbs disorder KG completers (Werte gr??er 1 werden aufsummiert)
ptbskg <- table(descriptivesKG$PTBS)
ptbskg
Ptbskg <- prop.table(ptbskg)
Ptbskg <- round(Ptbskg, digits=2)
Ptbskg

#ptbs disorder EG completers (Werte gr??er 1 werden aufsummiert)
ptbseg <- table(descriptivesEG$PTBS)
ptbseg
Ptbseg <- prop.table(ptbseg)
Ptbseg <- round(Ptbseg, digits=2)
Ptbseg


#other comorbidites
#other all completers (Werte gr??er 1 werden aufsummiert)
other <- table(descriptives$otherComorbidities_all)
other
Other <- prop.table(other)
Other <- round(Other, digits=2)
Other

#other KG completers (Werte gr??er 1 werden aufsummiert)
otherkg <- table(descriptivesKG$otherComorbidities_all)
otherkg
Otherkg <- prop.table(otherkg)
Otherkg <- round(Otherkg, digits=2)
Otherkg

#other EG completers (Werte gr??er 1 werden aufsummiert)
othereg <- table(descriptivesEG$otherComorbidities_all)
othereg
Othereg <- prop.table(othereg)
Othereg <- round(Othereg, digits=2)
Othereg


####medication
#SSRI
#ssri all completers (Werte gr??er 1 werden aufsummiert)
ssri <- table(descriptives$SSRI_total)
ssri
Ssri <- prop.table(ssri)
Ssri <- round(Ssri, digits=2)
Ssri

#ssri KG completers (Werte gr??er 1 werden aufsummiert)
ssrikg <- table(descriptivesKG$SSRI_total)
ssrikg
Ssrikg <- prop.table(ssrikg)
Ssrikg <- round(Ssrikg, digits=2)
Ssrikg

#ssri EG completers (Werte gr??er 1 werden aufsummiert)
ssrieg <- table(descriptivesEG$SSRI_total)
ssrieg
Ssrieg <- prop.table(ssrieg)
Ssrieg <- round(Ssrieg, digits=2)
Ssrieg


#Serotonin Antagonist
#sant all completers (Werte gr??er 1 werden aufsummiert)
sant <- table(descriptives$SerotoninAntagonist_total)
sant
Sant <- prop.table(sant)
Sant <- round(Sant, digits=2)
Sant

#sant KG completers (Werte gr??er 1 werden aufsummiert)
santkg <- table(descriptivesKG$SerotoninAntagonist_total)
santkg
Santkg <- prop.table(santkg)
Santkg <- round(Santkg, digits=2)
Santkg

#sant EG completers (Werte gr??er 1 werden aufsummiert)
santeg <- table(descriptivesEG$SerotoninAntagonist_total)
santeg
Santeg <- prop.table(santeg)
Santeg <- round(Santeg, digits=2)
Santeg


#SSNRI
#ssnri all completers (Werte gr??er 1 werden aufsummiert)
ssnri <- table(descriptives$SSNRI_total)
ssnri
Ssnri <- prop.table(ssnri)
Ssnri <- round(Ssnri, digits=2)
Ssnri

#ssnri KG completers (Werte gr??er 1 werden aufsummiert)
ssnrikg <- table(descriptivesKG$SSNRI_total)
ssnrikg
Ssnrikg <- prop.table(ssnrikg)
Ssnrikg <- round(Ssnrikg, digits=2)
Ssnrikg

#ssnri EG completers (Werte gr??er 1 werden aufsummiert)
ssnrieg <- table(descriptivesEG$SSNRI_total)
ssnrieg
Ssnrieg <- prop.table(ssnrieg)
Ssnrieg <- round(Ssnrieg, digits=2)
Ssnrieg


#tetrazyklische Antidepressiva
#tetraAD all completers (Werte gr??er 1 werden aufsummiert)
tetra <- table(descriptives$tetraAD_total)
tetra
Tetra <- prop.table(tetra)
Tetra <- round(Tetra, digits=2)
Tetra

#tetra KG completers (Werte gr??er 1 werden aufsummiert)
tetrakg <- table(descriptivesKG$tetraAD_total)
tetrakg
Tetrakg <- prop.table(tetrakg)
Tetrakg <- round(Tetrakg, digits=2)
Tetrakg

#tetra EG completers (Werte gr??er 1 werden aufsummiert)
tetraeg <- table(descriptivesEG$tetraAD_total)
tetraeg
Tetraeg <- prop.table(tetraeg)
Tetraeg <- round(Tetraeg, digits=2)
Tetraeg


#trizyklische Antidepressiva
#triAD all completers (Werte gr??er 1 werden aufsummiert)
tri <- table(descriptives$triAD_total)
tri
Tri <- prop.table(tri)
Tri <- round(Tri, digits=2)
Tri

#tri KG completers (Werte gr??er 1 werden aufsummiert)
trikg <- table(descriptivesKG$triAD_total)
trikg
Trikg <- prop.table(trikg)
Trikg <- round(Trikg, digits=2)
Trikg

#tri EG completers (Werte gr??er 1 werden aufsummiert)
trieg <- table(descriptivesEG$triAD_total)
trieg
Trieg <- prop.table(trieg)
Trieg <- round(Trieg, digits=2)
Trieg


#Conventional Antipsychotica
#cap all completers (Werte gr??er 1 werden aufsummiert)
cap <- table(descriptives$conventAntiPsy_total)
cap
Cap <- prop.table(cap)
Cap <- round(Cap, digits=2)
Cap

#cap KG completers (Werte gr??er 1 werden aufsummiert)
capkg <- table(descriptivesKG$conventAntiPsy_total)
capkg
Capkg <- prop.table(capkg)
Capkg <- round(Capkg, digits=2)
Capkg

#cap EG completers (Werte gr??er 1 werden aufsummiert)
capeg <- table(descriptivesEG$conventAntiPsy_total)
capeg
Capeg <- prop.table(capeg)
Capeg <- round(Capeg, digits=2)
Capeg


#atypical Antipsychotica
#aap all completers (Werte gr??er 1 werden aufsummiert)
aap <- table(descriptives$atypAntiPsy_total)
aap
Aap <- prop.table(aap)
Aap <- round(Aap, digits=2)
Aap

#aap KG completers (Werte gr??er 1 werden aufsummiert)
aapkg <- table(descriptivesKG$atypAntiPsy_total)
aapkg
Aapkg <- prop.table(aapkg)
Aapkg <- round(Aapkg, digits=2)
Aapkg

#aap EG completers (Werte gr??er 1 werden aufsummiert)
aapeg <- table(descriptivesEG$atypAntiPsy_total)
aapeg
Aapeg <- prop.table(aapeg)
Aapeg <- round(Aapeg, digits=2)
Aapeg


#other Medication
#otherm all completers (Werte gr??er 1 werden aufsummiert)
otherm <- table(descriptives$otherMed_total)
otherm
Otherm <- prop.table(otherm)
Otherm <- round(Otherm, digits=2)
Otherm

#otherm KG completers (Werte gr??er 1 werden aufsummiert)
othermkg <- table(descriptivesKG$otherMed_total)
othermkg
Othermkg <- prop.table(othermkg)
Othermkg <- round(Othermkg, digits=2)
Othermkg

#otherm EG completers (Werte gr??er 1 werden aufsummiert)
othermeg <- table(descriptivesEG$otherMed_total)
othermeg
Othermeg <- prop.table(othermeg)
Othermeg <- round(Othermeg, digits=2)
Othermeg

#############################################################
#Chi-square tests (values from previous analysis)
#############################################################

Affective_D <- c(12, 9)
Affective_D_test <- chisq.test(Affective_D, p = c(1/2, 1/2))
Affective_D_test

#Anxiety_D <- c(1, 1) # frequencies too small for test
#Anxiety_D_test <- chisq.test(Anxiety_D, p = c(1/2, 1/2))
#Anxiety_D_test

Eating_D <- c(7, 7)
Eating_D_test <- chisq.test(Eating_D, p = c(1/2, 1/2))
Eating_D_test

PTSD <- c(10, 10)
PTSD_test <- chisq.test(PTSD, p = c(1/2, 1/2))
PTSD_test

Other_Comorbidities<-c(10, 10)
Other_Comorbidities_test <- chisq.test(Other_Comorbidities, p = c(1/2, 1/2))
Other_Comorbidities_test

SSRI<-c(6, 4)
SSRI_test <- chisq.test(SSRI, p = c(1/2, 1/2))
SSRI_test

#Serotonin_Ant<-c(0, 3)  # frequencies too small for test
#Serotonin_Ant_test <- chisq.test(Serotonin_Ant, p = c(1/2, 1/2))
#Serotonin_Ant_test

#SSSNRI<-c(4, 3) # frequencies too small for test
#SSSNRI_test <- chisq.test(SSSNRI, p = c(1/2, 1/2))
#SSSNRI_test

#Tetracyclic<-c(3, 2) # frequencies too small for test
#Tetracyclic_test <- chisq.test(Tetracyclic, p = c(1/2, 1/2))
#Tetracyclic_test

#conventional_antipsychotica<-c(3, 2)  # frequencies too small for test
#conventional_antipsychotica_test <- chisq.test(conventional_antipsychotica, p = c(1/2, 1/2))
#conventional_antipsychotica_test

atypical_antipsychotica<-c(5, 5)
atypical_antipsychotica_test <- chisq.test(atypical_antipsychotica, p = c(1/2, 1/2))
atypical_antipsychotica_test

other_medication<-c(6, 3)
other_medication_test <- chisq.test(other_medication, p = c(1/2, 1/2))
other_medication_test


################################################################################
# > # sessionInfo()
#   > ################################################################################
# > # R version 4.0.5 (2021-03-31)
#   > # Platform: x86_64-w64-mingw32/x64 (64-bit)
#   > # Running under: Windows Server x64 (build 17763)
#   > # 
#   > # Matrix products: default
#   > # 
#   > # locale:
#   > #   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
#   > # [5] LC_TIME=German_Germany.1252    
#   > # 
#   > # attached base packages:
#   > #   [1] stats     graphics  grDevices utils     datasets  methods   base     
#   > # 
#   > # other attached packages:
#   > #   [1] rstatix_0.7.0   xfun_0.22       arsenal_3.6.3   forcats_0.5.1   stringr_1.4.0   dplyr_1.0.5     purrr_0.3.4     readr_1.4.0    
#   > # [9] tidyr_1.1.3     tibble_3.1.1    ggplot2_3.3.3   tidyverse_1.3.1 pacman_0.5.1   
#   > # 
#   > # loaded via a namespace (and not attached):
#   > #   [1] Rcpp_1.0.6        lubridate_1.7.10  assertthat_0.2.1  utf8_1.2.1        R6_2.5.0          cellranger_1.1.0  backports_1.2.1  
#   > # [8] reprex_2.0.0      highr_0.9         httr_1.4.2        pillar_1.6.0      rlang_0.4.11      curl_4.3.1        readxl_1.3.1     
#   > # [15] rstudioapi_0.13   data.table_1.14.0 car_3.0-10        foreign_0.8-81    munsell_0.5.0     broom_0.7.6       compiler_4.0.5   
#   > # [22] modelr_0.1.8      pkgconfig_2.0.3   tidyselect_1.1.1  rio_0.5.26        fansi_0.4.2       crayon_1.4.1      dbplyr_2.1.1     
#   > # [29] withr_2.4.2       grid_4.0.5        jsonlite_1.7.2    gtable_0.3.0      lifecycle_1.0.0   DBI_1.1.1         magrittr_2.0.1   
#   > # [36] scales_1.1.1      zip_2.1.1         cli_2.5.0         stringi_1.5.3     carData_3.0-4     fs_1.5.0          xml2_1.3.2       
#   > # [43] ellipsis_0.3.2    generics_0.1.0    vctrs_0.3.8       openxlsx_4.2.3    tools_4.0.5       glue_1.4.2        hms_1.0.0 