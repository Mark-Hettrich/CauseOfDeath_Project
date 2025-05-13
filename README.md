# CauseOfDeath_Project

Here is the GitHub repository where we stored all of our code.

Please make sure to add the cps_00006.csv file to 'Datasets' -> 'Population Data Merge' locally to ensure that the code works. The file is to large to push on GitHub, and should be recieved by mail.
Additionally please make sure to open the Here.R Script and execute the code, so that the here() function we used across the project works reliably. 

The following listed files are the most essential files for this project. Other files only serve exploratory purposes and are to be ignored.

Important Files and their location:
- Early Analysis:
  1) Univariate Analysis -> See the "Cause".Rmd files for Univariate Analysis
  2) Bivariate Analysis von Xinyu Meng -> bivariate_analysis_XinyuMeng.Rmd for bivariate analysis
  3) Hypothesis test -> See "Cause"_Hypotest.Rmd files for Hypothesis tests
- Multinomial Model:
  1) Models -> Petro Models -> Multinomial.Rmd for Multinomial Model
- Merging of Population Data: Datasets -> Population Data Merge
  1) DataMerge_AgeGroups.R was used to merge the CPS & CDC Data with 10-Year-Age-Groups
  2) DataMerge_sup_incl.R was used to merge the CPS & CDC Data with Single-Year-Ages
- Negative Binomial Model: Models -> Negative_Binomial.Rmd for Data transformation Plots, calculation of overdispersion for Poisson model & Negative binomial model
- AFT Model: Models -> Accelerated Failure Time.Rmd for testing of Proportional Hazards assumption & AFT Model
