# Coyote Use at the U.S.-Mexico Border from 1994 to 2010 using the Mexican Migration Project (MMP174)

This repository contains the necessary R scripts to replicate the inferential analysis in *Coyotaje in an Uncertain Social Environment: Prior Coyote Experience as a Form of Migration-Specific Human Capital*. The below description provides the name and the descipition for each dataset and R script. Please note that in order to replicate the analyses and create the post-processing dataset, please see the [Meso-American Migration Project](https://mmp.research.brown.edu/mmp) housed at Brown University, which was formally called the **Mexican Migration Project**. You will need to create an account to access the neccessary files. It's important to note that at the time of the inferential analyses presented in the manuscript, the current dataset was MMP174, where 174 denotes the number of communities sampled at the time. 

## R Scripts
- MMP_Processing_062625.R
  - This R script takes the raw MMP files, cleans the variables, and generates the post-processing dataset. 
- MMP_Multivariable_Analysis_072325.R
  - This file estimates all inferential models (e.g., multivariable logistic regressions with community clustered standard errors) presented in the main manuscript and appendix. 

## Data Files
- Final_MMP_Analytical_Dataset_2025-07-23.csv
  - This csv files contains the analytical sample used in the manuscript alongside the respective variables. 

## Graphs
- Predicted_Coyote_Plot_072325.png
  - This is the predicted probability graph shown in the main manuscript in which plots the predicted probability of crossing with a coyote on the most recent crossing trip across different levels of prior coyote experience and migration-specific human capital.
