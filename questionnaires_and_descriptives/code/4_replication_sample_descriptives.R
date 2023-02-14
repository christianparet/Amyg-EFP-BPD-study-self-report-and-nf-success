#replication sample descriptives
#################################################################
# Set Working Directory-----------------------------------------
#################################################################

#setwd("Y:/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/questionnaires_and_descriptives/data/") # Set to path where EFP_data.Rda is expected. Alternatively comment out this line and load EFP_data.RdA to workspace.
read.delim("//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/questionnaires_and_descriptives/data/EFP_data.Rda")

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
