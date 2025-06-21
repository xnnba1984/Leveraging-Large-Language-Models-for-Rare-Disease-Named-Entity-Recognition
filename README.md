# Leveraging-Large-Language-Models-for-Rare-Disease-Named-Entity-Recognition

This repository contains the code and data that supports all results in the manuscript "Leveraging Large Language Models for Rare Disease Named Entity Recognition".

File | Purpose | Key outputs
-----|-----|-----|
data_clean_rare.R | Simulates two correlated test statistics under the global null and compares different multiplicity control methods: NoAdj, Bonferroni, Holm, Dunnett for three error metrics (FWER, FMER, MSFP). | Table summary of false-positive rates; Figure 5. ​
zero_few_shot.R | Optimizes allocation, derives critical and p values for FWER/FM​ER/MSFP, and finds the minimal total N that yields ≥80 % power across grids of synergy (s) and endpoint correlation (ρ). | Figure 7A (sample-size curves); Figure 7B (optimal allocation). ​
RAG_error.R | Calculates critical values by generalized Dunnett's procedure to control multiple testing; computes single-test p-value thresholds for the three error criteria. | Figure 6. ​
fine_tune.R | Analyzes the PDX dataset: extracts arm-level correlations, estimates normalized effect sizes & synergy, and applies the allocation optimization to each drug pair. | Table 1 (false-positive control) and Table 2 (optimal N & allocation). ​
embedding.R | Investigates how arm correlations and allocation ratios shape FWER/FM​ER/MSFP when no multiplicity adjustment is applied. | Figures 3 and 4. 
cost.R | Investigates how arm correlations and allocation ratios shape FWER/FM​ER/MSFP when no multiplicity adjustment is applied. | Figures 3 and 4. ​
external_DB.R | Investigates how arm correlations and allocation ratios shape FWER/FM​ER/MSFP when no multiplicity adjustment is applied. | Figures 3 and 4. ​
figure.R | Investigates how arm correlations and allocation ratios shape FWER/FM​ER/MSFP when no multiplicity adjustment is applied. | Figures 3 and 4. ​​
