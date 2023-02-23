####################################################################
# Code to analyze EEG-neurofeedback training data from study EFPTest
# Zopfs, Sicorello, Paret, ZI Mannheim, 2022
####################################################################
#load and install packages
####################################################################
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(tidyverse,
               clubSandwich,
               lme4,
               lmerTest,
               parameters,
               piecewiseSEM)


# Read and edit data frame -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# EFP NF training data
EFP_3L_all <- read.delim("Y:/Projects/EFPTest/Data_analysis/protected_materials_code_data_clinical_study/data/Pooleddata_blockwise_task-efpnftraining.txt")

# Rename variables
# EFP_3L_all$subjectID <- EFP_3L_all$SubjectID

# Choose completers of experimental group (see CSV "Datenpflege-Log and QC")
# Subset of completers
EFP_3L <- subset(EFP_3L_all, SubjectID=="EFP02" |
                   SubjectID=="EFP04"| # completer of training, questionnaire data of post-assessment lost due to technical error
                   SubjectID=="EFP05"|
                   SubjectID=="EFP07"|
                   SubjectID=="EFP10"|
                   SubjectID=="EFP11"|
                   SubjectID=="EFP12"|
                   SubjectID=="EFP14"|
                   SubjectID=="EFP15"|
                   SubjectID=="EFP17"|
                   SubjectID=="EFP18"|
                   SubjectID=="EFP19"|
                   SubjectID=="EFP23"| # completer of training, no post-scan due to dissociation and technical problem during scanning
                   SubjectID=="EFP33"|
                   SubjectID=="EFP34")
table(EFP_3L$SubjectID)

# Centering SessionID on first run
EFP_3L$session <- EFP_3L$SessionID - 1

# Predicting outcome personal_effect_size with completers -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

PES.M <- lmer(personal_effect_size ~
                session +                                               # Fixed effect: Session
                (1 + session | SubjectID) +                             # Random effect: Sessions nested in subjects
                (1 | SubjectID:session),                                # Random effect: Measurements (blocks) nested in session
              data = EFP_3L,
              REML = TRUE)

qqnorm(resid(PES.M))
qqline(resid(PES.M))
# is normally distributed

summary(PES.M)
# Fixed effects:
#   Estimate      Std. Error       df       t value Pr(>|t|)   
# (Intercept) -0.30651    0.08457 14.53722  -3.624  0.00261 **
#   session      0.04984    0.02099 14.17157   2.375  0.03220 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

rsquared(PES.M)
# Response   family     link method   Marginal Conditional
# 1 personal_effect_size gaussian identity   none 0.04185968   0.3453141

# Return model coefficients
#coef(PES.final)
#?coef()
#ranef(PES.final)

##################################################################
#sessionInfo()
##################################################################
# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] piecewiseSEM_2.1.2 parameters_0.18.1  lmerTest_3.1-3     lme4_1.1-29        Matrix_1.4-1       clubSandwich_0.5.7
# [7] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
# [13] tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.1    pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-157        fs_1.5.2            lubridate_1.8.0     insight_0.17.1      RColorBrewer_1.1-3 
# [6] httr_1.4.3          numDeriv_2016.8-1.1 tools_4.2.1         backports_1.4.1     utf8_1.2.2         
# [11] R6_2.5.1            DBI_1.1.3           colorspace_2.0-3    withr_2.5.0         tidyselect_1.1.2   
# [16] emmeans_1.7.5       compiler_4.2.1      cli_3.3.0           rvest_1.0.2         xml2_1.3.3         
# [21] sandwich_3.0-2      bayestestR_0.12.1   scales_1.2.0        mvtnorm_1.1-3       digest_0.6.29      
# [26] minqa_1.2.4         pkgconfig_2.0.3     htmltools_0.5.2     dbplyr_2.2.1        fastmap_1.1.0      
# [31] htmlwidgets_1.5.4   rlang_1.0.3         readxl_1.4.0        rstudioapi_0.13     visNetwork_2.1.0   
# [36] generics_0.1.2      zoo_1.8-10          jsonlite_1.8.0      car_3.1-0           magrittr_2.0.3     
# [41] Rcpp_1.0.8.3        munsell_0.5.0       fansi_1.0.3         abind_1.4-5         lifecycle_1.0.1    
# [46] stringi_1.7.6       multcomp_1.4-19     carData_3.0-5       MASS_7.3-57         grid_4.2.1         
# [51] crayon_1.5.1        lattice_0.20-45     haven_2.5.0         splines_4.2.1       hms_1.1.1          
# [56] pillar_1.7.0        boot_1.3-28         estimability_1.3    codetools_0.2-18    reprex_2.0.1       
# [61] glue_1.6.2          modelr_0.1.8        vctrs_0.4.1         nloptr_2.0.3        tzdb_0.3.0         
# [66] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1    datawizard_0.4.1    xtable_1.8-4       
# [71] broom_1.0.0         coda_0.19-4         survival_3.3-1      DiagrammeR_1.0.9    TH.data_1.1-1  