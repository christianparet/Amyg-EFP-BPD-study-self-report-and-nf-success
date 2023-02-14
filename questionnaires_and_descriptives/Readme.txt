# Content of repository
	## What this is about
	Data and analysis code of clinical and psychological outcome measures that were assessed before (pre) and after (post) Amyg-EFP neurofeedback training.
	## File structure
	- this Readme file with an overview of project and content
	- code directory:
		- 2_descriptives pre.R: Analysis of pre-assessment demographic, descriptive and questionnaire data
			- Preprocessing of questionnaire data: aggregation, recoding of inversely coded items, building total and subscale scores
			- Statistical comparison of treatment and control group on sample characteristics (including clinical data and medication)
		- 3_comparison pre to post.R Analysis of differences between pre- and post-assessment of questionnaire data/clinical outcome measures
				- Preprocessing (see above)
				- Statistical comparisons of differences between group and measurement timepoints
	- data directory:		
		- R dataframe EFP_data.Rda: De-identified data for analysis with R statistics software	

# Data dictionary
	- descriptives.xlsx: this table contains all the clinical information summarized from the adolescent center patient records in order to decribe the study sample in clinical terms
	  it contains the following information about each participant
		- age at inclusion, date of birth
		- number of BPD criteria
		- psychiatric ICD-10 diagnoses (column F until AO)
		- medication during the study (column AP until CI)
		- finished (0: patient finished study without usable fMRI data from the concluding MRI-scan; 1: patient finished the study with )
		- group (0: belongs to control group - didn't receive NF training; 1: belongs to treatment group - did receive EEG-EFP-NF)

## Questionnaires:
Abbreviation	Name
ALS	Affect Lability Scale
BDI	Beck Depression Inventory version 2
IMI	Intrinsic Motivation Inventory<
STAI	State-Trait Anxiety Inventory
TAS	Toronto Alexithymia Scale

# Software
Matlab R2022a
R version 4.2.1
Information about software dependencies such as toolboxes and computing environments for R-based analysis scripts is available from session_info output, which has been copy-pasted at the end of the script.

# References
McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. Psychological Bulletin, 111(2), 361–365. https://doi.org/10.1037/0033-2909.111.2.361
Paret, C., Goldway, N., Zich, C., Keynan, J.N., Hendler, T., Linden, D., Cohen Kadosh, K. (2019). Current progress in real-time functional magnetic resonance-based neurofeedback: Methodological challenges and achievements. NeuroImage 202, 116107. https://doi.org/10.1016/j.neuroimage.2019

# Authors and contact
Christian Paret, ZI Mannheim, christian.paret@zi-mannheim.de
Malte Zopfs, Malte Zopfs

# Year of creation 
2022
