# Content of directory trainingsession_analysis
	## What this is about
	This repository has code for analysis of neurofeedback data, that is, the Amyg-EFP signal and the feedback that the training was based on. The purpose of the code explained below is the analysis of learning success to modulate the Amyg-EFP across the training sessions.

	## File structure
	- This Readme file with an overview of project and content
	- Code directory with code to aggregate sourcedata and for statistical data analysis
	- Aggregated data directory with tables of subject data
	- Figures directory of graphical outputs
	- Sourcedata directory (unzip before use) with the EEG-neurofeedback logfiles containing Amyg-EFP value (arbitrary scale) and feedback volume intensity (0-1, 0=silent, 1=max. volume) of the training sessions

	## EEG-neurofeedback training data
	Analysis of within-neurofeedback session Amyg-EFP data.
 	- Data aggregation: Matlab-program EEG_training_data_aggregation.m retrieves Amyg-EFP and feedback volume information from logfiles and produces aggregated tables of oucome measures. The program calculates neurofeedback success measures such as personal effect size (Paret et al., 2019) and common language effect size (CLES, McGraw & Wong, 1992). Data tables (txt-files) produced with this program can be found in the aggregated data directory.
 	- Statistical analysis: R-program EFP_MLM_PES_publish.R estimates the multilevel model (MLM) and assesses significance. An MLM reflecting the nested data structure (sessions in participants) to assess significance of a linear Session effect (after mean-centering). That is, an incremental increase of Amyg-EFP activation across sessions is assumed. The MLM includes a Subject random effect, a linear Session random effect and a random Subject x Session interaction. 
 	- Graphical display: Figure_EEG_training_data.R produces plots showing mean-subject-timecourses of Amyg-EFP values and feedback values, averaged across session.

# Code dictionary
Concerns table: aggregated data/Pooleddata_blockwise_task-efpnftraining.txt

Variable	Meaning
SubjectID	subject ID
SessionID	session number
BlockID	block number within session
success_index	numerical success measure calculated online by neurofeedback software
personal_effect_size	standardized mean difference between regulation block and foregoing baseline block (Paret et al., 2019), interpretable according to Cohen's d
common_language_effect_size	CLES according to McGraw et al (1992)
mean_efp_neurofeedback	mean value from regulation blocks
mean_efp_baseline mean value from baseline blocks
mean_vol_neurofeedback	mean volume of auditory feedback (0-10)

# Software
Matlab R2022a
R version 4.2.1
Information about software dependencies such as toolboxes and computing environments for R-based analysis scripts is available from session_info output, which has been copy-pasted at the end of the script.

# References
McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. Psychological Bulletin, 111(2), 361–365. https://doi.org/10.1037/0033-2909.111.2.361
Paret, C., Goldway, N., Zich, C., Keynan, J.N., Hendler, T., Linden, D., Cohen Kadosh, K. (2019). Current progress in real-time functional magnetic resonance-based neurofeedback: Methodological challenges and achievements. NeuroImage 202, 116107. https://doi.org/10.1016/j.neuroimage.2019


