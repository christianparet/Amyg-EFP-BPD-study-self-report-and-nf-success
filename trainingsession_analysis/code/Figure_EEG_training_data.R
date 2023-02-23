### the goal of this script is to get means and sd of each completing participant's NF-training sessions.
### Namely of the variables: personal effect size, success index, neurofeedback mean and baseline mean

# Internal note to authors from CP, 01.07.2022
# clean code for figures
# add comments, e.g. to explain what the grey shading stands for (sd?)
#

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(tidyverse,
               reshape2,
               rstatix)

### set the working directory and importing the data ###
setwd('Y:/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data')
session_avg <-
  read.table(
    'Pooleddata_blockwise_task-efpnftraining.txt',
    header = TRUE
  )

# Read and edit data frame -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# exclude drop-outs from 'session_avg' to create dataframe with only completers called 'session_avg_completer'
session_avg_completer <- subset(
  session_avg,
  SubjectID == "EFP02" |
    SubjectID == "EFP04" |
    SubjectID == "EFP05" |
    SubjectID == "EFP07" |
    SubjectID == "EFP10" |
    SubjectID == "EFP11" |
    SubjectID == "EFP12" |
    SubjectID == "EFP14" |
    SubjectID == "EFP15" |
    SubjectID == "EFP17" |
    SubjectID == "EFP18" |
    SubjectID == "EFP19" |
    SubjectID == "EFP23" |
    SubjectID == "EFP33" |
    SubjectID == "EFP34"
)

### group the resulting data by SubjectID and by SessionID
# using the summarize function: 'mean()' and 'sd()' are then calculated for the columns of interest
# this results in 'BlockID' being removed from the resulting dataframe,
# as the block-wise values are averaged per participant-session into mean and sd-values
ses_avg_compl_grouped <-
  session_avg_completer %>% group_by(SubjectID, SessionID) %>% summarise(
    session_success_index = mean(success_index),
    session_success_index_sd = sd(success_index),
    session_personal_effect_size = mean(personal_effect_size),
    session_personal_effect_size_sd = sd(personal_effect_size),
    session_mean_efp_nf = mean(mean_efp_neurofeedback),
    session_mean_efp_nf_sd = sd(mean_efp_neurofeedback),
    session_mean_efp_bl = mean(mean_efp_baseline),
    session_mean_efp_bl_sd = sd(mean_efp_baseline),
    session_mean_vol_nf = mean(mean_vol_neurofeedback),
    session_mean_vol_nf_sd = sd(mean_vol_neurofeedback),
  )

View(ses_avg_compl_grouped)

##Outliers
outlier_graph <- ses_avg_compl_grouped %>%
  group_by(SubjectID, SessionID) %>%
  identify_outliers(session_success_index)

# Plot the average session values over time -------------------------------------------------------------------------------------------------------------------------------------------------------
# plot values for success index, personal effect size, mean neurofeedback, mean baseline
# make SubjectID a factor in order to visualize values for each participant separately
ses_avg_compl_grouped <-
  within(ses_avg_compl_grouped, SubjectID <- factor(SubjectID))

### using ggplot, define 'ses_avg_compl_grouped' as the data to be plotted grouped by subject

# # plotted variable: mean of success index----------------------------------------------------------------------------------------------------------------------------------------------------------
# graphs_success_index <-
#   ggplot(data = ses_avg_compl_grouped,
#          aes(x = SessionID, y = session_success_index, group = SubjectID))
# 
# # plot as a simple scatterplot
# graphs_success_index + geom_point()
# # plot as a simple spaghetti plot with different color for each subject and even session numbers on x-axis
# graphs_success_index + geom_line(aes(colour = SubjectID)) + scale_x_continuous(breaks =
#                                                                                  c(2, 4, 6, 8, 10))
# # plot as a simple spaghetti plot which ends at session 10 (only one subject had 12 sessions)
# graphs_success_index + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10)) + scale_x_continuous(breaks =
#                                                                                                                     c(2, 4, 6, 8, 10))
# # plot as a spaghetti plot with a 10-session limit, group mean plotted with regression line and standard error shading
# # added a line using locally weighted regression (lowess) to "smooth" over all the variability and give a sense of the overall or average trend.
# graphs_success_index + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10)) + stat_smooth(aes(group = 1)) + stat_summary(
#   aes(group = 1),
#   geom = "point",
#   fun = mean,
#   shape = 17,
#   size = 2)+
#   scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
#   ylab("Session Success Index")+
#   xlab("Session")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12))+
#   theme(
#     # Remove panel border
#     panel.border = element_blank(),  
#     # Remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Remove panel background
#     panel.background = element_blank(),
#     # Add axis line
#     axis.line = element_line(colour = "grey"))
# 
# #group mean plotted with regression line and standard error shading, with a 10-session limit 
# Plot1<- graphs_success_index + coord_cartesian(xlim = c(1, 10)) + stat_smooth(aes(group = 1)) + stat_summary(
#   aes(group = 1),
#   geom = "point",
#   fun = mean,
#   shape = 17,
#   size = 2) + 
#   scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
#   ylab("Success Index")+
#   xlab("Session")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12))+
#   theme(
#     # Remove panel border
#     panel.border = element_blank(),  
#     # Remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Remove panel background
#     panel.background = element_blank(),
#     # Add axis line
#     axis.line = element_line(colour = "grey"))

##plotted variable: mean of personal effect size ---------------------------------------------------------------------------------------------------------------------------------------------------
graphs_pes <-
  ggplot(data = ses_avg_compl_grouped,
         aes(x = SessionID, y = session_personal_effect_size, group = SubjectID))
#spaghetti plot (subject colors, limited and even-numbered x-axis)
graphs_pes + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10)) + scale_x_continuous(breaks =
                                                                                                          c(2, 4, 6, 8, 10))
# spaghetti plot (subject colors, limited and even-numbered x-axis) + group mean plotted with regression line and standard error shading
graphs_pes + coord_cartesian(xlim = c(1, 10))+
  stat_smooth(aes(group = 1)) + stat_summary(
  aes(group = 1),
  geom = "point",
  fun = mean,
  shape = 17,
  size = 2) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
  ylab("Personal Effect Size")+
  xlab("Session")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey"))

# group mean plotted with regression line and standard error shading
plot2<- graphs_pes + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10))+
  stat_smooth(aes(group = 1)) + stat_summary(
    aes(group = 1),
    geom = "point",
    fun = mean,
    shape = 17,
    size = 2) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
  ylab("Personal effect size")+
  xlab("Session")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"))

##plotted variable: mean neurofeedback ------------------------------------------------------------------------------------------------------------------------------------------------------------
graphs_mean_nf <-
  ggplot(data = ses_avg_compl_grouped, aes(x = SessionID, y = session_mean_vol_nf, group = SubjectID))
#spaghetti plot (subject colors, limited and even-numbered x-axis)
graphs_mean_nf + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10)) + scale_x_continuous(breaks =
                                                                                                              c(2, 4, 6, 8, 10))
# spaghetti plot (subject colors, limited and even-numbered x-axis) + group mean plotted with regression line and standard error shading
graphs_mean_nf  + coord_cartesian(xlim = c(1, 10)) + stat_smooth(aes(group = 1)) + stat_summary(
  aes(group = 1),
  geom = "point",
  fun = mean,
  shape = 17,
  size = 2) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
  ylab("Session mean NF")+
  xlab("Session")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(
    # Remove panel border
    panel.border = element_blank(),
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"))

# ##plotted variable: mean baseline -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# graphs_mean_bl <-
#   ggplot(data = ses_avg_compl_grouped, aes(x = SessionID, y = session_mean_bl, group = SubjectID))
# #spaghetti plot (subject colors, limited and even-numbered x-axis)
# graphs_mean_bl + geom_line(aes(colour = SubjectID)) + coord_cartesian(xlim = c(1, 10)) + scale_x_continuous(breaks =
#                                                                                                               c(2, 4, 6, 8, 10))
# # spaghetti plot (subject colors, limited and even-numbered x-axis) + group mean plotted with regression line and standard error shading
# graphs_mean_bl + coord_cartesian(xlim = c(1, 10)) + stat_smooth(aes(group = 1)) + stat_summary(
#   aes(group = 1),
#   geom = "point",
#   fun = mean,
#   shape = 17,
#   size = 2) +
#   scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
#   scale_x_continuous(breaks = c(2, 4, 6, 8, 10))+
#   ylab("Session mean BL")+
#   xlab("Session")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12))+
#   theme(
#     # Remove panel border
#     panel.border = element_blank(),
#     # Remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Remove panel background
#     panel.background = element_blank(),
#     # Add axis line
#     axis.line = element_line(colour = "grey"))
# 
