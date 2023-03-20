#replication sample descriptives
#################################################################
# Set Working Directory-----------------------------------------
#################################################################

#setwd("Y:/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/questionnaires_and_descriptives/data/") # Set to path where EFP_data.Rda is expected. Alternatively comment out this line and load EFP_data.RdA to workspace.
setwd("//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data")

#################################################################
# Loading packages (installs if necessary)
#################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               arsenal,
               xfun,
               readxl,
               rstatix) 

EFP_post <- read_excel("POST.xlsx")

###################################################################################################################################################################
#
# Preprocessing 
#
###################################################################################################################################################################

EFP_post <- rename(EFP_post, Probanden_ID ='Probanden-ID')

#Construct separate dataframes for each questionnaire

ALScomplete<-  EFP_post%>%
  select(contains('ALS'))

STAIcomplete<-  EFP_post%>%
  select(contains('STAI'))

BDIcomplete<-  EFP_post%>%
  select(contains('BDI'))

TAScomplete<-  EFP_post%>%
  select(contains('TAS'))

IMIcomplete<-  EFP_post%>%
  select(contains('IMI'))
IMIcomplete$`Bulimia Nervosa F50.2`<- NULL


#################################################################################
#BDI
#################################################################################

#BDI Recoding
BDIcomplete$post_BDI_16recoded <- car::recode(BDIcomplete$post_BDI_16,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4") 
BDIcomplete$post_BDI_18recoded <- car::recode(BDIcomplete$post_BDI_18,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4")

BDIrecoded <- BDIcomplete %>%mutate_at(vars(-post_BDI_16recoded,
                                            -post_BDI_18recoded),~ifelse(. == 1, 0, ifelse(. == 2, 1, ifelse(. == 3, 2, ifelse(. == 4, 3, .))))) 

#remove old BDI_16 and BDI_18
BDIrecoded$post_BDI_16 <- NULL
BDIrecoded$post_BDI_18 <- NULL

#Sumscore BDI

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
ALSrecoded$post_ALS_subscale_Deprs <- rowSums (subset(ALSrecoded, select = c(post_ALS_01, post_ALS_08, post_ALS_09, post_ALS_13, post_ALS_19, post_ALS_25, post_ALS_31, post_ALS_40, post_ALS_42, post_ALS_46, post_ALS_54)))

##subscale Hypomania
ALSrecoded$post_ALS_subscale_Hypom <- rowSums (subset(ALSrecoded, select = c(post_ALS_02, post_ALS_06, post_ALS_07, post_ALS_18, post_ALS_27, post_ALS_32, post_ALS_35, post_ALS_36, post_ALS_39, post_ALS_45, post_ALS_52, post_ALS_53)))

##subscale Biphs
ALSrecoded$post_ALS_subscale_Biphs <- rowSums (subset(ALSrecoded, select = c(post_ALS_11, post_ALS_24, post_ALS_29, post_ALS_30, post_ALS_34, post_ALS_43, post_ALS_47, post_ALS_48, post_ALS_51)))

##subscale Anxiety
ALSrecoded$post_ALS_subscale_Anxty <- rowSums (subset(ALSrecoded, select = c(post_ALS_04, post_ALS_05, post_ALS_12, post_ALS_20, post_ALS_26, post_ALS_28, post_ALS_44)))

##subscale Anger
ALSrecoded$post_ALS_subscale_Anger <- rowSums (subset(ALSrecoded, select = c(post_ALS_14, post_ALS_15, post_ALS_21, post_ALS_23, post_ALS_33, post_ALS_41, post_ALS_50)))

##subcale Anxdp
ALSrecoded$post_ALS_subscale_Anxdp <- rowSums (subset(ALSrecoded, select = c(post_ALS_03, post_ALS_10, post_ALS_16, post_ALS_17, post_ALS_22, post_ALS_37, post_ALS_38, post_ALS_49)))

##ALS total post sumscore
ALSrecoded$post_ALS_Total <- ALSrecoded %>%
  select(contains('post_ALS_subscale')) %>%
  rowSums()

#################################################################################
#TAS
#################################################################################

##TAS Recoding & Subscales

TAS_withoutdaydream <- subset(TAScomplete, # remove daydreaming subscale because of negative correlation with other subscales in German population, https://doi.org/10.13109/zptm.2000.46.4.368
                              select=c (post_TAS_03, 
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
  mutate_at(vars(post_TAS_09, 
                 post_TAS_11, 
                 post_TAS_12,
                 post_TAS_13, 
                 post_TAS_15, 
                 post_TAS_21, 
                 post_TAS_24), 
            ~ifelse(. == 1, 5, ifelse(. == 2, 4, ifelse(. == 3, 3, ifelse(. == 4, 2, ifelse(. == 5, 1,.))))))

#Subscale identification of emotion
TASrecoded$post_TAS_subscale_ident <- rowSums(subset(TASrecoded, select = c(post_TAS_04, post_TAS_10, post_TAS_14, post_TAS_17, post_TAS_20, post_TAS_25, post_TAS_26))) 

#Subscale description of emotion
TASrecoded$post_TAS_subscale_descr <- rowSums(subset(TASrecoded, select = c(post_TAS_03, post_TAS_08, post_TAS_12, post_TAS_22, post_TAS_23)))

#Subscale thinking style
TASrecoded$post_TAS_subscale_thnkng <- rowSums(subset(TASrecoded, select = c(post_TAS_09, post_TAS_11, post_TAS_13, post_TAS_15, post_TAS_21, post_TAS_24))) 

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

#IMIrecoded <- IMIcomplete %>%
#  mutate_at(vars(post_IMI_14, 
#                 post_IMI_16, 
#                 post_IMI_21, 
#                 post_IMI_22,
#                 post_IMI_29  ), ~ifelse(. == 1, 7, ifelse(. == 2, 6, ifelse(. == 3, 5, ifelse(. == 4, 4, ifelse(. == 5, 3, ifelse(. == 6, 2, ifelse(. == 7, 1, .))))))))


#IMIrecoded$Probanden_ID <- EFP_post$Probanden_ID
#IMIrecoded$Condition <- EFP_post$Condition

#build subscales post
#IMIrecoded$post_IMI_subscale_interest <- rowMeans(cbind(IMIrecoded$post_IMI_02,
#                                                        IMIrecoded$post_IMI_06,
#                                                        IMIrecoded$post_IMI_09,
#                                                        IMIrecoded$post_IMI_11,
#                                                        IMIrecoded$post_IMI_22r,
#                                                        IMIrecoded$post_IMI_25,
#                                                        IMIrecoded$post_IMI_29r))
#IMIrecoded$post_IMI_subscale_competence <- rowMeans(cbind(IMIrecoded$post_IMI_10,
#                                                          IMIrecoded$post_IMI_13,
#                                                          IMIrecoded$post_IMI_15,
#                                                          IMIrecoded$post_IMI_17,
#                                                          IMIrecoded$post_IMI_21r,
#                                                          IMIrecoded$post_IMI_23))
#IMIrecoded$post_IMI_subscale_effort <- rowMeans(cbind(IMIrecoded$post_IMI_03,
#                                                      IMIrecoded$post_IMI_07r,
#                                                      IMIrecoded$post_IMI_14r,
#                                                      IMIrecoded$post_IMI_18,
#                                                      IMIrecoded$post_IMI_27))
#IMIrecoded$post_IMI_subscale_tension <- rowMeans(cbind(IMIrecoded$post_IMI_05,
#                                                       IMIrecoded$post_IMI_08,
#                                                       IMIrecoded$post_IMI_16r,
#                                                       IMIrecoded$post_IMI_19))
#IMIrecoded$post_IMI_subscale_value <- rowMeans(cbind(IMIrecoded$post_IMI_01,
#                                                     IMIrecoded$post_IMI_04,
#                                                     IMIrecoded$post_IMI_12,
#                                                     IMIrecoded$post_IMI_20,
#                                                     IMIrecoded$post_IMI_24,
#                                                     IMIrecoded$post_IMI_26,
#                                                     IMIrecoded$post_IMI_28))


################################################################################
#Construct dataframes for following analyses 
################################################################################

# Dataframe containing only outcome variables of questionnaires
Data_replication<- as.data.frame(cbind(ALSrecoded$post_ALS_subscale_Deprs,
                            ALSrecoded$post_ALS_subscale_Anger,
                            ALSrecoded$post_ALS_subscale_Anxdp,
                            ALSrecoded$post_ALS_subscale_Anxty,
                            ALSrecoded$post_ALS_subscale_Hypom,
                            ALSrecoded$post_ALS_subscale_Biphs,
                            ALSrecoded$post_ALS_Total,
                            TASrecoded$post_TAS_subscale_ident,
                            TASrecoded$post_TAS_subscale_descr,
                            TASrecoded$post_TAS_subscale_thnkng,
                            TASrecoded$post_TAS_Total,
                            BDIrecoded$post_BDI_Total,
                            STAIrecoded$post_STAI_Total,
                            EFP_post$Probanden_ID))

colnames(Data_replication) <- c("ALSDeprs", "ALSAnger", "ALSAnxdp", "ALSAnxty", "ALSHypom", 
                     "ALSBiphs", "ALSTotal", "TASident", "TASdescr", 
                     "TASthnkng", "TASTotal", "BDITotal", "Probanden_ID")  

Data_replication<- Data_replication %>% mutate_at(c(1:12), as.numeric)
Data_replication<- na.omit(Data_replication)

###################################################################################################################################################################
#
# Sample descriptives for questionnaires 
#
###################################################################################################################################################################

#mydata.long <- Data_replication %>%
#  pivot_longer(-Condition, names_to = "variables", values_to = "value")

#mydata.long$value <-as.numeric(mydata.long$value)
#mydata.long$Condition <-as.factor(mydata.long$Condition)
#mydata.long$variables <-as.factor(mydata.long$variables)

#Gives mean and SD for each outcome variable and each group at time-point pre
#replication study contains the following particpants (EFP__):2,4,5,10,12,14,15,17,18,19,23,29,57,58,59,60
#hence, the following analyses include only the following subset of the dataframe
df_replication.sum <- subset(Data_replication, Probanden_ID=="EFP02" |
                   Probanden_ID=="EFP04"| # completer of training, questionnaire data of post-assessment lost due to technical error
                   Probanden_ID=="EFP05"|
                   Probanden_ID=="EFP10"|
                   Probanden_ID=="EFP12"|
                   Probanden_ID=="EFP14"|
                   Probanden_ID=="EFP15"|
                   Probanden_ID=="EFP17"|
                   Probanden_ID=="EFP18"|
                   Probanden_ID=="EFP19"|
                   Probanden_ID=="EFP23"| # completer of training, interrupted post-scan due to dissociation and technical problem during scanning
                   Probanden_ID=="EFP29"|
                   Probanden_ID=="EFP57"|
                   Probanden_ID=="EFP58"|
                   Probanden_ID=="EFP59"|
                   Probanden_ID=="EFP60")%>%
  select(ALSDeprs, ALSAnger, ALSAnxdp, ALSAnxty, ALSHypom, ALSBiphs, ALSTotal, TASident, TASdescr, TASthnkng, TASTotal, BDITotal) %>% # select variables to summarise
  summarise_each(funs(
    mean = mean, 
    sd = sd))
df_replication.sum

##############################################################################################################################
#ALSDeprs_mean ALSAnger_mean ALSAnxdp_mean ALSAnxty_mean ALSHypom_mean ALSBiphs_mean ALSTotal_mean TASident_mean TASdescr_mean
#19      9.333333      17.06667      12.93333      14.93333      12.86667      86.13333      22.73333      19.13333
#TASthnkng_mean TASTotal_mean BDITotal_mean ALSDeprs_sd ALSAnger_sd ALSAnxdp_sd ALSAnxty_sd ALSHypom_sd ALSBiphs_sd ALSTotal_sd
#13.66667      55.53333      30.46667    6.403124      7.0677    4.905779    4.382867     7.70405    5.817052    30.27698
#TASident_sd TASdescr_sd TASthnkng_sd TASTotal_sd BDITotal_sd
#7.166058    3.943651     3.811012    11.32549    13.91744

################################################################################
#sessionInfo()
################################################################################
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] readxl_1.4.2    xfun_0.37       arsenal_3.6.3   ggpubr_0.6.0    afex_1.2-1      rstatix_0.7.2   reshape2_1.4.4  forcats_1.0.0   stringr_1.5.0   dplyr_1.1.0    
# [11] purrr_1.0.1     readr_2.1.4     tidyr_1.3.0     tibble_3.1.8    ggplot2_3.4.1   tidyverse_1.3.2 pacman_0.5.1    lmerTest_3.1-3  lme4_1.1-31     Matrix_1.5-1   
# 
# loaded via a namespace (and not attached):
#   [1] httr_1.4.4          jsonlite_1.8.4      splines_4.2.2       carData_3.0-5       modelr_0.1.10       assertthat_0.2.1    googlesheets4_1.0.1 cellranger_1.1.0   
# [9] numDeriv_2016.8-1.1 pillar_1.8.1        backports_1.4.1     lattice_0.20-45     glue_1.6.2          ggsignif_0.6.4      rvest_1.0.3         minqa_1.2.5        
# [17] colorspace_2.1-0    plyr_1.8.8          pkgconfig_2.0.3     broom_1.0.3         haven_2.5.1         scales_1.2.1        tzdb_0.3.0          timechange_0.2.0   
# [25] googledrive_2.0.0   mgcv_1.8-41         generics_0.1.3      farver_2.1.1        car_3.1-1           ellipsis_0.3.2      withr_2.5.0         cli_3.6.0          
# [33] magrittr_2.0.3      crayon_1.5.2        fs_1.6.1            fansi_1.0.4         nlme_3.1-160        MASS_7.3-58.1       xml2_1.3.3          tools_4.2.2        
# [41] hms_1.1.2           gargle_1.3.0        lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.2        ggsci_2.9           compiler_4.2.2      rlang_1.0.6        
# [49] grid_4.2.2          nloptr_2.0.3        rstudioapi_0.14     labeling_0.4.2      boot_1.3-28         gtable_0.3.1        abind_1.4-5         DBI_1.1.3          
# [57] R6_2.5.1            lubridate_1.9.2     utf8_1.2.3          stringi_1.7.12      parallel_4.2.2      Rcpp_1.0.10         vctrs_0.5.2         dbplyr_2.3.0       
# [65] tidyselect_1.2.0   
