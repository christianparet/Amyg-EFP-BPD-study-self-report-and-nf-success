# Content of repository

## What this is about
Data and analysis code of clinical and psychological outcome measures that were assessed before (pre) and after (post) Amyg-EFP neurofeedback training.
## File structure
1) this Readme file with an overview of project and content
2) code directory:
  - 2_descriptives pre.R: Analysis of pre-assessment demographic, descriptive and questionnaire data
    - Preprocessing of questionnaire data: aggregation, recoding of inversely coded items, building total and subscale scores
    - Statistical comparison of treatment and control group on sample characteristics (including clinical data and medication)
  - 3_comparison pre to post.R Analysis of differences between pre- and post-assessment of questionnaire data/clinical outcome measures
    - Preprocessing (see above)
    - Statistical comparisons of differences between group and measurement timepoints
# Data dictionary

## Questionnaires:
ALS: Affect Lability Scale

BDI:   Beck Depression Inventory version 2

IMI:   Intrinsic Motivation Inventory

STAI:   State-Trait Anxiety Inventory

TAS:   Toronto Alexithymia Scale
# Software
Matlab R2022a

R version 4.2.1

Information about software dependencies such as toolboxes and computing environments for R-based analysis scripts is available from session_info output, which has been copy-pasted at the end of the script.
