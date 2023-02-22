#################################################################
# Analysis of correlations of NF success/learning with improvement in clinical measures EFPTest study
# Paret, ZI Mannheim, 2023
#################################################################
# Set Working Directory-----------------------------------------
#################################################################

setwd("Y:/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/questionnaires_and_descriptives/data")

#################################################################
# Loading packages (installs if necessary)
#################################################################

if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse,
#              reshape2,
#             afex,
#            ggpubr,
#           rstatix)

# Read and edit data frames -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Neurofeedback training data
subj_data <- read.delim("Y:/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/trainingsession_analysis/aggregated data/Pooleddata_difference-score_task-efpnftraining.txt")

# Choose completers of experimental group (see CSV "Datenpflege-Log and QC")
# Subset of completers
subj_data_completer <- subset(subj_data, SubjectID2=="EFP02" |
                   SubjectID2=="EFP04"| # completer of training, questionnaire data of post-assessment lost due to technical error
                   SubjectID2=="EFP05"|
                   SubjectID2=="EFP07"|
                   SubjectID2=="EFP10"|
                   SubjectID2=="EFP11"|
                   SubjectID2=="EFP12"|
                   SubjectID2=="EFP14"|
                   SubjectID2=="EFP15"|
                   SubjectID2=="EFP17"|
                   SubjectID2=="EFP18"|
                   SubjectID2=="EFP19"|
                   SubjectID2=="EFP23"| # completer of training, no post-scan due to dissociation and technical problem during scanning
                   SubjectID2=="EFP33"|
                   SubjectID2=="EFP34")

# Self-report/clinical data
load("EFP_data.Rda")


###################################################################################################################################################################
# Preprocessing 
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

STAIrecoded$STAI_Total <- rowSums(STAIrecoded)


#################################################################################
#TAS
#################################################################################

##TAS Recoding & Subscales

TAS_withoutdaydream <- subset(TAScomplete, 
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


################################################################################
#Construct dataframes for following analyses 
################################################################################

###Dataframe with all recoded Variables

Data1<- cbind(ALSrecoded,TASrecoded,
              BDIrecoded, STAIrecoded, EFP_data$Condition, EFP_data$group, EFP_data$Probanden_ID)

Data1 <- dplyr::rename(Data1, Condition = 'EFP_data$Condition')
Data1$Condition <- as.factor(Data1$Condition)
Data1 <- dplyr::rename(Data1, Probanden_ID = 'EFP_data$Probanden_ID')
Data1 <- dplyr::rename(Data1, Group = 'EFP_data$group')


###################################################################################################################################################################
#
# Spaghetti plots pre-post for each group 
#
###################################################################################################################################################################

#################################################################
# Spaghetti plot for ALS 
#################################################################
ALS<- melt(data = Data1, id.vars = c("Condition", "Group", "Probanden_ID"), measure.vars = c("pre_ALS_Total", "post_ALS_Total"))


ALS<- ALS%>%dplyr::rename(time = 'variable',
                   ALSscore='value',
                   group='Condition',
                   group_id='Group')

ALScontrol <- subset(ALS, group=='control')
ALStreatment <- subset(ALS, group=='treatment')

ALS$group<-as.factor(ALS$group)

#Spaghetti plot ALSTotal 

ggplot(ALS, aes(time, ALSscore)) + geom_line(aes(colour = group, group =
                                                   Probanden_ID)) + stat_smooth(data= ALScontrol, aes(group = 1), color= "red") + stat_smooth(data= ALStreatment, aes(group = 1), color= "blue") + stat_summary(
                                                     aes(colour = group, group = Probanden_ID),
                                                     geom = "point",
                                                     fun = mean,
                                                     shape = 24,
                                                     size = 2)


#################################################################
# Spaghetti plot for BDI
#################################################################
BDI<- melt(data = Data1, id.vars = c("Condition", "Probanden_ID"), measure.vars = c("pre_BDI_Total", "post_BDI_Total"))


BDI<- BDI%>%dplyr::rename(time = 'variable',
                   BDIscore='value',
                   group='Condition')

BDIcontrol <- subset(BDI, group=='control')
BDItreatment <- subset(BDI, group=='treatment')

BDI$group<-as.factor(BDI$group)

#Spaghetti plot BDITotal 

ggplot(BDI, aes(time, BDIscore)) + geom_line(aes(colour = group, group =
                                                   Probanden_ID)) + stat_smooth(data= BDIcontrol, aes(group = 1), color= "red") + stat_smooth(data= BDItreatment, aes(group = 1), color= "blue") + stat_summary(
                                                     aes(colour = group, group = Probanden_ID),
                                                     geom = "point",
                                                     fun = mean,
                                                     shape = 24,
                                                     size = 2)



#################################################################
# Spaghetti plot for TAS 
#################################################################
TAS<- melt(data = Data1, id.vars = c("Condition", "Probanden_ID"), measure.vars = c("pre_TAS_Total", "post_TAS_Total"))


TAS<- TAS%>%dplyr::rename(time = 'variable',
                   TASscore='value',
                   group='Condition')

TAScontrol <- subset(TAS, group=='control')
TAStreatment <- subset(TAS, group=='treatment')

TAS$group<-as.factor(TAS$group)

#Spaghetti plot TASTotal 

ggplot(TAS, aes(time, TASscore)) + geom_line(aes(colour = group, group =
                                                   Probanden_ID)) + stat_smooth(data= TAScontrol, aes(group = 1), color= "red") + stat_smooth(data= TAStreatment, aes(group = 1), color= "blue") + stat_summary(
                                                     aes(colour = group, group = Probanden_ID),
                                                     geom = "point",
                                                     fun = mean,
                                                     shape = 24,
                                                     size = 2)

###################################################################################################################################################################
#
# Mixed ANOVAS 
#
###################################################################################################################################################################


################################################################################
# Mixed ANOVA for ALS 
################################################################################

#Melt(convert) dataframe into different structure to be able to conduct further analysis # CP: remove commented code in clean version
#ALS<- melt(data = Data1, id.vars = c("Condition", "Probanden_ID"), measure.vars = c("pre_ALS_Total", "post_ALS_Total"))

#rename variables in new dataframe for simplicity
#ALS <- ALS%>%dplyr::rename(time = 'variable',
#                    ALSscore='value',
 #                   group='Condition')

#coerce "group" into a factor 
#ALS$group<-as.factor(ALS$group)

#interaction plot (per condition, both time points) ### Not functioning, Paret 11.07.2022
#with(ALS, interaction.plot(x.factor = time , trace.factor = group, 
#                           response = ALSscore))
##boxplot
bxp <- ggboxplot(
  ALS, x = "time", y = "ALSscore",
  color = "group", palette = "jco")
bxp

##Outliers
ALS %>%
  group_by(time, group) %>%
  identify_outliers(ALSscore)

##Normality
ALS %>%
  group_by(time, group) %>%
  shapiro_test(ALSscore)

ggqqplot(ALS, "ALSscore", ggtheme = theme_bw()) +
  facet_grid(time ~ group)

##Homoscedasticity
ALS %>%
#  mutate(time=factor(time))%>% # CP: remove in clean version
  group_by(time) %>%
  levene_test(ALSscore ~ group)

##Homogeneity of Covariances
box_m(
  
  #box_m hat agrumente "data" und "group"; ALS[,"ALSscore"] ist kein data.frame --> as.data.frame
  as.data.frame(
    
    #na.omit, weil box_m kann keine missing values verwenden
    na.omit(
      
      #data = ALS [,"ALSscore"]
      ALS
    )[,"ALSscore"]
    
    #hier auch na.omit weil sonst ALS$ALSscore und ALS$group nicht gleich lang sind
  ), na.omit(ALS)[,"group"]
)
# statistic p.value parameter method                                             
# <dbl>   <dbl>     <dbl> <chr>                                              
#   1     0.677   0.411         1 Box's M-test for Homogeneity of Covariance Matrices

##Splitplot Anova 
ALSAnova<-aov_ez(id="Probanden_ID",
                  dv="ALSscore",
                  data=ALS,
                  between="group",
                  within= "time")
get_anova_table(ALSAnova)

# Effect    df     MSE       F  ges p.value
# 1      group 1, 24 1658.01    0.19 .007    .665
# 2       time 1, 24  165.42 8.46 ** .031    .008
# 3 group:time 1, 24  165.42    0.40 .002    .533
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

################################################################################
# Mixed ANOVA for TAS
################################################################################


# TAS<- melt(data = Data1, id.vars = c("Condition", "Probanden_ID"), measure.vars = c("pre_TAS_Total", "post_TAS_Total")) # CP: remove in clean version
# 
# TAS <- TAS%>%dplyr::rename(time = 'variable',
#                     TASscore='value',
#                     group='Condition')
# 
# TAS$group<-as.factor(TAS$group)

#interaction plot
# with(TAS, interaction.plot(x.factor = time , trace.factor = group, 
#                            response = TASscore))
##boxplot
bxp2 <- ggboxplot(
  TAS, x = "time", y = "TASscore",
  color = "group", palette = "jco")
bxp2

##Outliers
TAS %>%
  group_by(time, group) %>%
  identify_outliers(TASscore)

# group     time           Probanden_ID TASscore is.outlier is.extreme
# <fct>     <fct>          <chr>           <dbl> <lgl>      <lgl>     
#   1 treatment pre_TAS_Total  EFP34              39 TRUE       FALSE     
# 2 treatment post_TAS_Total EFP34              29 TRUE       FALSE  

##Normality
TAS %>%
  group_by(time, group) %>%
  shapiro_test(TASscore)

ggqqplot(TAS, "TASscore", ggtheme = theme_bw()) +
  facet_grid(time ~ group)

##Homoscedasticity
TAS %>%
  group_by(time) %>%
  levene_test(TASscore ~ group)


##Homogeneity of Covariances
box_m(
  as.data.frame(
    na.omit(
      TAS
    )[,"TASscore"]
  ), na.omit(TAS)[,"group"]
)
# statistic p.value parameter method                                             
# <dbl>   <dbl>     <dbl> <chr>                                              
#   1     0.157   0.692         1 Box's M-test for Homogeneity of Covariance Matrices

##Splitplot Anova
TASAnova<-aov_ez(id="Probanden_ID",
                  dv="TASscore",
                  data=TAS,
                  between="group",
                  within= "time")
get_anova_table(TASAnova)
 
# Effect    df   MSE      F   ges p.value
# 1      group 1, 24 81.67 4.73 *  .131    .040
# 2       time 1, 24 25.45 4.41 *  .042    .046
# 3 group:time 1, 24 25.45   0.05 <.001    .831

##Repeat analysis without outlier
# Subset w/o subject EFP34                            # CP: keep list for documentary reasons
# TAS_outlier_removed <- subset(TAS, 
#                    Probanden_ID=="EFP02"|
#                    Probanden_ID=="EFP04"| # no post
#                    Probanden_ID=="EFP05"|
#                    Probanden_ID=="EFP07"|
#                    Probanden_ID=="EFP10"|
#                    Probanden_ID=="EFP11"|
#                    Probanden_ID=="EFP12"|
#                    Probanden_ID=="EFP14"|
#                    Probanden_ID=="EFP15"|
#                    Probanden_ID=="EFP17"|
#                    Probanden_ID=="EFP18"|
#                    Probanden_ID=="EFP19"|
#                    Probanden_ID=="EFP23"|
#                    Probanden_ID=="EFP28"|
#                    Probanden_ID=="EFP29"|
#                    Probanden_ID=="EFP30"| # no post
#                    Probanden_ID=="EFP33"|
#                    Probanden_ID=="EFP37"|
#                    Probanden_ID=="EFP38"|
#                    Probanden_ID=="EFP39"|
#                    Probanden_ID=="EFP42"|
#                    Probanden_ID=="EFP43"|
#                    Probanden_ID=="EFP44"|
#                    Probanden_ID=="EFP45"|
#                    Probanden_ID=="EFP46"| # no post
#                    Probanden_ID=="EFP47"|
#                    Probanden_ID=="EFP48"|
#                    Probanden_ID=="EFP49"|
#                    Probanden_ID=="EFP51")
# table(TAS_outlier_removed$Probanden_ID)
TAS_outlier_removed<-subset(TAS,Probanden_ID!="EFP34")
##boxplot
bxp2 <- ggboxplot(
  TAS_outlier_removed, x = "time", y = "TASscore",
  color = "group", palette = "jco")
bxp2

##Splitplot Anova, outlier removed
TASAnova<-aov_ez(id="Probanden_ID",
                 dv="TASscore",
                 data=TAS_outlier_removed,
                 between="group",
                 within= "time")
get_anova_table(TASAnova)

# Effect    df   MSE      F  ges p.value
# 1      group 1, 23 58.72 3.71 + .101    .067
# 2       time 1, 23 25.29 3.50 + .044    .074
# 3 group:time 1, 23 25.29   0.17 .002    .684
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

#################################################################
# Mixed ANOVA for BDI
#################################################################

# BDI<- melt(data = Data1, id.vars = c("Condition", "Probanden_ID"), measure.vars = c("pre_BDI_Total", "post_BDI_Total"))  # CP: remove in clean version
# 
# BDI<- BDI%>%dplyr::rename(time = 'variable',
#                    BDIscore='value',
#                    group='Condition')
# 
# BDI$group<-as.factor(BDI$group)

#interaction plot                                                                   # CP: remove in clean version
# with(BDI, interaction.plot(x.factor = time , trace.factor = group, 
#                            response = BDIscore))

##boxplot
bxp3 <- ggboxplot(
  BDI, x = "time", y = "BDIscore",
  color = "group", palette = "jco")
bxp3

##Outliers
BDI %>%
  group_by(time, group) %>%
  identify_outliers(BDIscore)

# Extreme outlier found:
# group     time           Probanden_ID BDIscore is.outlier is.extreme
# <fct>     <fct>          <chr>           <dbl> <lgl>      <lgl>     
# 1 control   pre_BDI_Total  EFP37               4 TRUE       TRUE      
# 2 control   pre_BDI_Total  EFP48              18 TRUE       FALSE     
# 3 treatment pre_BDI_Total  EFP34              17 TRUE       FALSE     
# 4 control   post_BDI_Total EFP37               5 TRUE       FALSE 

##Normality
BDI %>%
  group_by(time, group) %>%
  shapiro_test(BDIscore)

# Normality violated:
# group     time           variable statistic       p
# <fct>     <fct>          <chr>        <dbl>   <dbl>
#   1 control   pre_BDI_Total  BDIscore     0.814 0.00745
# 2 treatment pre_BDI_Total  BDIscore     0.934 0.317  
# 3 control   post_BDI_Total BDIscore     0.944 0.518  
# 4 treatment post_BDI_Total BDIscore     0.925 0.259  

ggqqplot(BDI, "BDIscore", ggtheme = theme_bw()) +
  facet_grid(time ~ group)

##Homoscedasticity
BDI %>%
  group_by(time) %>%
  levene_test(BDIscore ~ group)

##Homogeneity of Covariances
box_m(
  as.data.frame(
    na.omit(
      BDI
    )[,"BDIscore"]
  ), na.omit(BDI)[,"group"]
)
# statistic p.value parameter method                                             
# <dbl>   <dbl>     <dbl> <chr>                                              
#   1     0.108   0.742         1 Box's M-test for Homogeneity of Covariance Matrices

##Splitplot Anova
BDIAnova<-aov_ez(id="Probanden_ID",
                  dv="BDIscore",
                  data=BDI,
                  between="group",
                  within= "time") 
get_anova_table(BDIAnova)

# Effect    df    MSE       F  ges p.value
# 1      group 1, 24 283.37    0.22 .008    .645
# 2       time 1, 24  46.98 9.15 ** .051    .006
# 3 group:time 1, 24  46.98  3.40 + .020    .077
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

##Repeat analysis without outlier
# Subset w/o subject EFP37                                # CP: keep list below for documentation
# BDI_outlier_removed <- subset(BDI, 
#                               Probanden_ID=="EFP02"|
#                                 Probanden_ID=="EFP04"| # # completer of training, questionnaire data of post-assessment lost due to technical error
#                                 Probanden_ID=="EFP05"|
#                                 Probanden_ID=="EFP07"|
#                                 Probanden_ID=="EFP10"|
#                                 Probanden_ID=="EFP11"|
#                                 Probanden_ID=="EFP12"|
#                                 Probanden_ID=="EFP14"|
#                                 Probanden_ID=="EFP15"|
#                                 Probanden_ID=="EFP17"|
#                                 Probanden_ID=="EFP18"|
#                                 Probanden_ID=="EFP19"|
#                                 Probanden_ID=="EFP23"|
#                                 Probanden_ID=="EFP28"|
#                                 Probanden_ID=="EFP29"|
#                                 Probanden_ID=="EFP30"| # no post
#                                 Probanden_ID=="EFP33"|
#                                 Probanden_ID=="EFP34"|
#                                 Probanden_ID=="EFP38"|
#                                 Probanden_ID=="EFP39"|
#                                 Probanden_ID=="EFP42"|
#                                 Probanden_ID=="EFP43"|
#                                 Probanden_ID=="EFP44"|
#                                 Probanden_ID=="EFP45"|
#                                 Probanden_ID=="EFP46"| # no post
#                                 Probanden_ID=="EFP47"|
#                                 Probanden_ID=="EFP48"|
#                                 Probanden_ID=="EFP49"|
#                                 Probanden_ID=="EFP51")
# table(BDI_outlier_removed$Probanden_ID)
BDI_outlier_removed <- subset(BDI,Probanden_ID!="EFP37")
##boxplot
bxp3 <- ggboxplot(
  BDI_outlier_removed, x = "time", y = "BDIscore",
  color = "group", palette = "jco")
bxp3

##Splitplot Anova
BDIAnova<-aov_ez(id="Probanden_ID",
                 dv="BDIscore",
                 data=BDI_outlier_removed,
                 between="group",
                 within= "time",
                 return="nice") 
get_anova_table(BDIAnova)

# Effect           df MSE       F        ges     p.value
# 1      group 1, 23 198.30    1.61     .053    .217
# 2       time 1, 23  48.77    8.84 **  .071    .007
# 3 group:time 1, 23  48.77    2.87     .024    .104
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

################################################################################
#sessionInfo()
################################################################################
