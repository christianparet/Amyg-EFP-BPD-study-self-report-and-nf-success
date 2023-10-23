#dropout analysis to compare attrition rates between study completers vs. non-completers

setwd("//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data") # Set path according expected file structure

# Install and load necessary packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", "rmarkdown")
}
pacman::p_load(tidyverse, dplyr, mice, arsenal, xfun, psych, readxl, rstatix, ggpubr, WRS2, moments)

# Read the excel file
descriptives <- read_excel("descriptives.xlsx")
# Rename columns
descriptives <- rename(descriptives, Probanden_ID = 'subjectID')

# Save file for dropout analysis
dropout_analysis <- descriptives

# Remove unnecessary variables
dropout_analysis$`Date of birth` <- NULL
dropout_analysis$`date of inclusion in study aka pre-scan` <- NULL

variables_to_compare <- c("SSRI_total", "SerotoninAntagonist_total", "SSNRI_total", 
                          "tetraAD_total", "triAD_total", "conventAntiPsy_total", 
                          "atypAntiPsy_total", "otherMed_total",
                          "BPD F60.31", "Major Depression F32", "MDD remitted F32", "MDD recurrent F33",
                          "Bipolar disorder", "AffectiveDisorder", "AnxietyDisorder",
                          "PTBS", "Dissociative and conversion disorders F44", 
                          "EatingDisorder",
                          "ADHD", "ADD", 
                          "schizophrenia F20", "Obsessive compulsive disorder F42",
                          "Cannabis abuse 12.1", "Cannabis abuse 12.1 remitted", 
                          "substance abusus F15.1", "substance abusus F15.1 remitted", 
                          "alcohol abusus F10.1 remitted", "benzodiazepine abusus F13.2",
                          "Nicotine dependence F17.2", "Avoidant Personality disorder F60.6",
                          "otherCom", "otherComorbidities_all")

# Initialize a results dataframe
results_df <- data.frame(Variable = character(0), Test = character(0), 
                         Test_Statistic = numeric(0), P_Value = numeric(0), 
                         Significance_Level = numeric(0), Note = character(0))

# Significance level
alpha <- 0.05

# Perform tests and save results
for (variable in variables_to_compare) {
  # Perform Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(dropout_analysis[[variable]])
  
  if (shapiro_test$p.value >= alpha) {
    # If data is approximately normal, perform t-test
    t_test <- t.test(dropout_analysis[[variable]] ~ finished, data = dropout_analysis)
    results_df <- rbind(results_df, c(variable, "t-test", t_test$statistic, t_test$p.value, alpha, ""))
  } else {
    # If data is not normal, check if there are sufficient observations for Wilcoxon test
    if (length(unique(dropout_analysis$finished)) == 2 && sum(dropout_analysis$finished == 0) >= 3 && sum(dropout_analysis$finished == 1) >= 3) {
      wilcox_test <- wilcox.test(dropout_analysis[[variable]] ~ finished, data = dropout_analysis)
      results_df <- rbind(results_df, c(variable, "Wilcoxon", wilcox_test$statistic, wilcox_test$p.value, alpha, ""))
    } else {
      results_df <- rbind(results_df, c(variable, "N/A", NA, NA, alpha, "Insufficient data for Wilcoxon test"))
    }
  }
}

# Create a modified results_df with the first row as descriptive titles
modified_results_df <- rbind(c("Variable", "Test", "Test_Statistic", "P_Value", "Significance_Level", "Note"), results_df)

# Specify the full path to the results CSV file
output_dropout_analysis <- file.path("//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/EFPTest/Data_analysis/open_materials_code_data_clinical_study/questionnaires_and_descriptives/tables and figures", "dropout_analysis_results.csv")

# Write modified results_df to a CSV file using write.csv
write.csv(modified_results_df, file = output_dropout_analysis, row.names = FALSE, quote = FALSE)

# Print report of significant results
significant_results <- results_df[results_df$P_Value < alpha, ]
if (nrow(significant_results) > 0) {
  cat("Significant Results:\n")
  print(significant_results)
} else {
  cat("No significant results found.\n")
}


########################################################################################################################
####Creating df with all participants and compare psychometric data of completers vs dropouts at inclusion of study ####
########################################################################################################################

# descriptives <- read_excel(file.choose())

library(dplyr)
library(tidyverse)
library(readxl)
library(rstatix)
library(car)

EFP_data_pre <- read_excel("PRÄ.xlsx")


#rename columns
colnames(EFP_data_pre) <- paste("pre", colnames(EFP_data_pre), sep = "_")
EFP_data_pre <- rename(EFP_data_pre, Probanden_ID = 'pre_Probanden ID')
EFP_data_pre <- rename(EFP_data_pre, finished = 'pre_finished')

#remove unnecessary variables 
EFP_data_pre$pre_Zeitstempel <- NULL
EFP_data_pre$`pre_Datensatz-Ursprung`<- NULL
EFP_data_pre$Zeitstempel<- NULL
EFP_data_pre$`Datensatz-Ursprung` <- NULL
EFP_data_pre$`Date of birth`<-NULL

#save dataframe 
save(EFP_data_pre,file="EFP_data_pre.Rda")

#load("EFP_data_pre.Rda")

#Construct separate dataframes for each questionnaire

ALScomplete_pre<-  EFP_data_pre%>%
  select(contains('ALS'))

STAIcomplete_pre<-  EFP_data_pre%>%
  select(contains('STAI'))

BDIcomplete_pre<-  EFP_data_pre%>%
  select(contains('BDI'))

TAScomplete_pre<-  EFP_data_pre%>%
  select(contains('TAS'))

#################################################################################
#BDI
#################################################################################

#BDI Recoding
BDIcomplete_pre$pre_BDI_16recoded <- car::recode(BDIcomplete_pre$pre_BDI_16,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4") 
BDIcomplete_pre$pre_BDI_18recoded <- car::recode(BDIcomplete_pre$pre_BDI_18,"2=2; 3=2; 4=3; 5=3; 6=4; 7=4")

BDI_pre_recoded <- BDIcomplete_pre %>%mutate_at(vars(-pre_BDI_16recoded,
                                            -pre_BDI_18recoded),~ifelse(. == 1, 0, ifelse(. == 2, 1, ifelse(. == 3, 2, ifelse(. == 4, 3, .))))) 

#remove old BDI_16 and BDI_18
BDI_pre_recoded$pre_BDI_16 <- NULL
BDI_pre_recoded$pre_BDI_18 <- NULL


#Sumscore BDI
BDI_pre_recoded$pre_BDI_Total <- BDI_pre_recoded %>%
  select(contains('pre_BDI')) %>%
  rowSums()

#################################################################################
#ALS
#################################################################################

#ALS Recoding & Subscales
ALS_recoded_pre <- ALScomplete_pre %>%
  mutate_all(~ifelse(. == 1, 3, ifelse(. == 2, 2, ifelse(. == 3, 1, ifelse(. == 4, 0, .)))))

##Forms subscale ALSDeprs from dataset ALS_recoded_pre by summing relevant items
ALS_recoded_pre$pre_ALS_subscale_Deprs <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_01, pre_ALS_08, pre_ALS_09, pre_ALS_13, pre_ALS_19, pre_ALS_25, pre_ALS_31, pre_ALS_40, pre_ALS_42, pre_ALS_46, pre_ALS_54)))

##subscale Hypomania
ALS_recoded_pre$pre_ALS_subscale_Hypom <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_02, pre_ALS_06, pre_ALS_07, pre_ALS_18, pre_ALS_27, pre_ALS_32, pre_ALS_35, pre_ALS_36, pre_ALS_39, pre_ALS_45, pre_ALS_52, pre_ALS_53)))

##subscale Biphs
ALS_recoded_pre$pre_ALS_subscale_Biphs <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_11, pre_ALS_24, pre_ALS_29, pre_ALS_30, pre_ALS_34, pre_ALS_43, pre_ALS_47, pre_ALS_48, pre_ALS_51)))

##subscale Anxiety
ALS_recoded_pre$pre_ALS_subscale_Anxty <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_04, pre_ALS_05, pre_ALS_12, pre_ALS_20, pre_ALS_26, pre_ALS_28, pre_ALS_44)))

##subscale Anger
ALS_recoded_pre$pre_ALS_subscale_Anger <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_14, pre_ALS_15, pre_ALS_21, pre_ALS_23, pre_ALS_33, pre_ALS_41, pre_ALS_50)))

##subcale Anxdp
ALS_recoded_pre$pre_ALS_subscale_Anxdp <- rowSums (subset(ALS_recoded_pre, select = c(pre_ALS_03, pre_ALS_10, pre_ALS_16, pre_ALS_17, pre_ALS_22, pre_ALS_37, pre_ALS_38, pre_ALS_49)))

##ALS total pre sumscore
ALS_recoded_pre$pre_ALS_Total <- ALS_recoded_pre %>%
  select(contains('pre_ALS_subscale')) %>%
  rowSums()

#################################################################################
#STAI
#################################################################################

#STAI Recoding & Subscales
STAI_recoded_pre <- STAIcomplete_pre %>%
  mutate_at(vars(pre_STAI_01, pre_STAI_06, pre_STAI_07, pre_STAI_10, pre_STAI_13, pre_STAI_16, pre_STAI_19), ~ifelse(. == 1, 4, ifelse(. == 2, 3, ifelse(. == 3, 2, ifelse(. == 4, 1, .)))))

#Calculate sum score for STAI
STAI_recoded_pre$pre_STAI_Total <- rowSums(STAI_recoded_pre)

#################################################################################
#TAS
#################################################################################

##TAS Recoding & Subscales
TAS_withoutdaydream_pre <- subset(TAScomplete_pre, # remove daydreaming subscale because of negative correlation with other subscales in German population, https://doi.org/10.13109/zptm.2000.46.4.368
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
                                        pre_TAS_26))

TAS_recoded_pre <- TAS_withoutdaydream_pre %>%
  mutate_at(vars(pre_TAS_09, 
                 pre_TAS_11, 
                 pre_TAS_12,
                 pre_TAS_13, 
                 pre_TAS_15, 
                 pre_TAS_21, 
                 pre_TAS_24), 
            ~ifelse(. == 1, 5, ifelse(. == 2, 4, ifelse(. == 3, 3, ifelse(. == 4, 2, ifelse(. == 5, 1,.))))))

#Subscale identification of emotion
TAS_recoded_pre$pre_TAS_subscale_ident <- rowSums(subset(TAS_recoded_pre, select = c(pre_TAS_04, pre_TAS_10, pre_TAS_14, pre_TAS_17, pre_TAS_20, pre_TAS_25, pre_TAS_26))) 

#Subscale description of emotion
TAS_recoded_pre$pre_TAS_subscale_descr <- rowSums(subset(TAS_recoded_pre, select = c(pre_TAS_03, pre_TAS_08, pre_TAS_12, pre_TAS_22, pre_TAS_23)))

#Subscale thinking style
TAS_recoded_pre$pre_TAS_subscale_thnkng <- rowSums(subset(TAS_recoded_pre, select = c(pre_TAS_09, pre_TAS_11, pre_TAS_13, pre_TAS_15, pre_TAS_21, pre_TAS_24))) 

#TAS total sum score 
TAS_recoded_pre$pre_TAS_Total <- TAS_recoded_pre %>%
  select(contains('pre_TAS_subscale')) %>%
  rowSums()

################################################################################
#Construct dataframes for following analyses 
################################################################################

# Dataframe containing only outcome variables of questionnaires
Data2_pre<- as.data.frame(cbind(ALS_recoded_pre$pre_ALS_subscale_Deprs,
                            ALS_recoded_pre$pre_ALS_subscale_Anger,
                            ALS_recoded_pre$pre_ALS_subscale_Anxdp,
                            ALS_recoded_pre$pre_ALS_subscale_Anxty,
                            ALS_recoded_pre$pre_ALS_subscale_Hypom,
                            ALS_recoded_pre$pre_ALS_subscale_Biphs,
                            ALS_recoded_pre$pre_ALS_Total,
                            TAS_recoded_pre$pre_TAS_subscale_ident,
                            TAS_recoded_pre$pre_TAS_subscale_descr,
                            TAS_recoded_pre$pre_TAS_subscale_thnkng,
                            TAS_recoded_pre$pre_TAS_Total,
                            BDI_pre_recoded$pre_BDI_Total,
                            STAI_recoded_pre$pre_STAI_Total,
                            EFP_data_pre$finished))

colnames(Data2_pre) <- c("ALSDeprs", "ALSAnger", "ALSAnxdp", "ALSAnxty", "ALSHypom", 
                     "ALSBiphs", "ALSTotal", "TASident", "TASdescr", 
                     "TASthnkng", "TASTotal", "BDITotal", "STAITotal", 
                     'finished')  

Data2_pre<- Data2_pre %>% mutate_at(c(1:13), as.numeric)
Data2_pre<- na.omit(Data2_pre)

###################################################################################################################################################################
#
# Sample descriptives for questionnaires 
#
###################################################################################################################################################################

#compare the mean age of both groups
EFP_descriptives_pre <- read_excel("descriptives.xlsx")
age.sum<- EFP_descriptives_pre %>%
  group_by(finished)%>%
  select(Age_inclusion) %>% # select variables to summarise
  summarise_each(funs(
    mean = mean, 
    sd = sd))

# Convert "finished" to a factor
EFP_descriptives_pre$finished <- factor(EFP_descriptives_pre$finished, levels = c(0, 1), labels = c("non-completer", "completer"))

# Shapiro-Wilk test for normality
shapiro_test_group1 <- shapiro.test(EFP_descriptives_pre$Age_inclusion[EFP_descriptives_pre$finished == "completer"])
shapiro_test_group2 <- shapiro.test(EFP_descriptives_pre$Age_inclusion[EFP_descriptives_pre$finished == "non-completer"])

# Check normality assumptions
if (shapiro_test_group1$p.value < 0.05 || shapiro_test_group2$p.value < 0.05) {
  # If either group violates normality, use Wilcoxon rank-sum test (Mann-Whitney U-test)
  wilcox_test_result <- wilcox.test(Age_inclusion ~ finished, data = EFP_descriptives_pre)
  print("Normality assumption violated - Using Wilcoxon rank-sum test (Mann-Whitney U-test):")
  print(wilcox_test_result)
} else {
  # If assumptions are met, use the standard t-test
  t_test_result <- t.test(Age_inclusion ~ finished, data = EFP_descriptives_pre)
  print("Assumptions met - Using standard t-test:")
  print(t_test_result)
}
# [1] "Assumptions met - Using standard t-test:"
# 
# Welch Two Sample t-test
# 
# data:  Age_inclusion by finished
# t = -2.0329, df = 47.724, p-value = 0.04764
# alternative hypothesis: true difference in means between group non-completer and group completer is not equal to 0
# 95 percent confidence interval:
#   -1.97793900 -0.01075759
# sample estimates:
#   mean in group non-completer     mean in group completer 
# 20.04667                    21.04102

###############################################################################
#prepare the data to compare outcome variable and each group at time-point pre
mydata.long <- Data2_pre %>%
 pivot_longer(-finished, names_to = "variables", values_to = "value")
mydata.long$value <-as.numeric(mydata.long$value)
mydata.long$finished <-as.factor(mydata.long$finished)
mydata.long$variables <-as.factor(mydata.long$variables)

#Gives mean and SD for each outcome variable and each group at time-point pre
df.sum<- Data2_pre %>%
  group_by(finished)%>%
  select(ALSDeprs, ALSAnger, ALSAnxdp, ALSAnxty, ALSHypom, ALSBiphs, ALSTotal, TASident, TASdescr, TASthnkng, TASTotal, BDITotal, STAITotal) %>% # select variables to summarise
  summarise_each(funs(
    mean = mean, 
    sd = sd))

df_total.sum<- Data2_pre %>%
  select(ALSDeprs, ALSAnger, ALSAnxdp, ALSAnxty, ALSHypom, ALSBiphs, ALSTotal, TASident, TASdescr, TASthnkng, TASTotal, BDITotal, STAITotal) %>% # select variables to summarise
  summarise_each(funs(
    mean = mean, 
    sd = sd))
df_total.sum

#compares each outcome variable between groups at time-point pre

alpha <- 0.05

stat.test <- mydata.long %>%
  group_by(variables) %>%
  do({
    # Shapiro-Wilk normality check for each group
    normality_check <- shapiro.test(.$value)
    
    if (normality_check$p.value >= alpha) {
      # If data is approximately normal, perform t-test
      t_test_result <- t.test(value ~ finished, data = ., var.equal = TRUE)
      t_test_result$p.value <- p.adjust(t_test_result$p.value, method = "BH")
      t_test_result$test_type <- "t-test"
      t_test_result$normality_check <- "Passed Shapiro-Wilk"
      
      # Create a data frame for the t-test result
      t_test_result_df <- data.frame(
        test_type = t_test_result$test_type,
        test_statistic = t_test_result$statistic,
        p_value = t_test_result$p.value,
        normality_check = t_test_result$normality_check
      )
      
      t_test_result_df
    } else {
      # If data is not normal, perform Wilcoxon rank-sum test
      wilcox_test_result <- wilcox.test(value ~ finished, data = .)
      wilcox_test_result$p.value <- p.adjust(wilcox_test_result$p.value, method = "BH")
      wilcox_test_result$test_type <- "Wilcoxon"
      wilcox_test_result$normality_check <- "Failed Shapiro-Wilk"
      
      # Create a data frame for the Wilcoxon test result
      wilcox_test_result_df <- data.frame(
        test_type = wilcox_test_result$test_type,
        test_statistic = wilcox_test_result$statistic,
        p_value = wilcox_test_result$p.value,
        normality_check = wilcox_test_result$normality_check
      )
      
      wilcox_test_result_df
    }
  }) %>%
  add_significance()
stat.test

write.csv(stat.test, file = "dropout_analysis_symptom_severity_raw.csv", row.names = FALSE)
write.csv(df.sum, file = "dropout_analysis_symptom_description.csv", row.names = FALSE)

#######################################################################################################
#compare symptom severity based on self-reported measures on the ward for the week of study inclusion##
#######################################################################################################

# # load and read the excel file
# clinicaldata_path <- "Y:/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data/AZ questionnaires (imputation+df)/clinical data.xlsx"
# clinicaldata <- read_excel(clinicaldata_path)
# clinicaldata <- clinicaldata[clinicaldata$week == 1, ]
# filtered_clinicaldata <- clinicaldata %>%
#   filter(finished %in% c(0, 1))
# #Gives mean and SD for each self-reported measure and each group in the week of study-inclusion
# # Grouped Mann-Whitney U test
# 
# # Variables of interest
# variables_of_interest <- c("BSLMean", "BSLSum", "DTS_Total_freq", "DTS_Total_sev", "BDI")
# 
# 
# # Function to perform the appropriate test based on normality
# perform_tests_based_on_normality <- function(variable_name, data) {
#   # Split the data into two groups based on the "finished" variable
#   group_0 <- data[data$finished == 0, variable_name]
#   group_1 <- data[data$finished == 1, variable_name]
#   
#   # Clean the data by converting to numeric and treating "NA" as missing values
#   group_0 <- as.numeric(as.character(group_0))
#   group_1 <- as.numeric(as.character(group_1))
#   
#   # Remove rows with missing values
#   group_0 <- group_0[!is.na(group_0)]
#   group_1 <- group_1[!is.na(group_1)]
#   
#   # Perform Shapiro-Wilk normality test for both groups
#   shapiro_0 <- tryCatch(shapiro.test(group_0), error = function(e) e)
#   shapiro_1 <- tryCatch(shapiro.test(group_1), error = function(e) e)
#   
#   # Check normality for both groups and decide which test to use
#   if (inherits(shapiro_0, "htest") && inherits(shapiro_1, "htest") &&
#       shapiro_0$p.value > 0.05 && shapiro_1$p.value > 0.05) {
#     # If both groups are normally distributed, use a t-test
#     t_test_result <- t.test(group_0, group_1, var.equal = TRUE)
#     return(list(Test = "t-test", Result = t_test_result))
#   } else {
#     # If at least one group is not normally distributed, use Mann-Whitney U test
#     mwu_result <- wilcox.test(group_0, group_1)
#     return(list(Test = "Mann-Whitney U Test", Result = mwu_result))
#   }
# }
# 
# # Loop over the variables of interest and perform the appropriate tests
# results <- list()
# for (variable_name in variables_of_interest) {
#   tryCatch({
#     test_result <- perform_tests_based_on_normality(variable_name, filtered_clinicaldata)
#     results[[variable_name]] <- test_result
#   }, error = function(e) {
#     cat("Error for variable:", variable_name, "\n")
#     cat("Error message:", conditionMessage(e), "\n\n")
#   })
# }
# 
# # Print the results
# for (variable_name in variables_of_interest) {
#   if (exists(paste("results[['", variable_name, "']]"))) {
#     cat("Variable:", variable_name, "\n")
#     cat("Test:", results[[variable_name]]$Test, "\n")
#     cat("Results:\n")
#     print(results[[variable_name]]$Result)
#     cat("\n")
#   }
# }

# Error for variable: BSLMean 
# Error message: not enough (non-missing) 'x' observations 
# 
# Error for variable: BSLSum 
# Error message: not enough (non-missing) 'x' observations 
# 
# Error for variable: DTS_Total_freq 
# Error message: not enough (non-missing) 'x' observations 
# 
# Error for variable: DTS_Total_sev 
# Error message: not enough (non-missing) 'x' observations 
# 
# Error for variable: BDI 
# Error message: not enough (non-missing) 'x' observations 
