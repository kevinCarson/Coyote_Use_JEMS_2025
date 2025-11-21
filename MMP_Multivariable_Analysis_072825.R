#-------------------------------------------------------------------------------#

#     R Code for Inferential Analyses for "Coyotaje in an Uncertain Social Environment: Prior Coyote Experience as a Form of Migration-Specific Human Capital"
#     Code Written By: Kevin A. Carson
#     Last Updated: 07-23-2025

#-------------------------------------------------------------------------------#
rm(list = ls()) #clearing memory
require(pacman) #requiring pacman package
pacman::p_load(dplyr, weights, logistf, regclass, pscl, lme4, marginaleffects,
               sandwich, lmtest, haven, tidyverse, miceadds, MASS,
               margins , mice, emmeans, sjPlot, catregs, ggplot2,psych) #loading packages

sessionInfo()
###### Session Information for Replication of Results
# R version 4.2.2 (2022-10-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.6.1
#
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
#
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
# [1] splines   stats4    stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] sjPlot_2.8.14        catregs_0.1.0        caret_6.0-93         lattice_0.20-45      emmeans_1.8.7        margins_0.3.26
# [7] MASS_7.3-58.1        miceadds_3.16-18     mice_3.15.0          forcats_0.5.2        stringr_1.4.1        purrr_0.3.5
# [13] readr_2.1.3          tidyr_1.2.1          tibble_3.1.8         ggplot2_3.4.3        tidyverse_1.3.2      haven_2.5.1
# [19] lmtest_0.9-40        zoo_1.8-11           sandwich_3.0-2       lme4_1.1-32          Matrix_1.5-1         pscl_1.5.5
# [25] regclass_1.6         randomForest_4.7-1.1 rpart_4.1.19         VGAM_1.1-8           bestglm_0.37.3       leaps_3.1
# [31] logistf_1.24.1       weights_1.0.4        Hmisc_5.0-1          dplyr_1.0.10         pacman_0.5.1
#
# loaded via a namespace (and not attached):
# [1] utf8_1.2.2           tidyselect_1.2.0     htmlwidgets_1.6.1    grid_4.2.2           pROC_1.18.0          devtools_2.4.5
# [7] jtools_2.2.2         munsell_0.5.0        codetools_0.2-18     statmod_1.5.0        future_1.29.0        miniUI_0.1.1.1
# [13] withr_2.5.0          colorspace_2.0-3     highr_0.9            knitr_1.41           rstudioapi_0.14      listenv_0.8.0
# [19] labeling_0.4.2       farver_2.1.1         coda_0.19-4          parallelly_1.32.1    vctrs_0.5.1          generics_0.1.3
# [25] TH.data_1.1-2        ipred_0.9-13         xfun_0.35            timechange_0.1.1     R6_2.5.1             RcppZiggurat_0.1.6
# [31] cachem_1.0.6         assertthat_0.2.1     promises_1.2.0.1     scales_1.2.1         multcomp_1.4-25      nnet_7.3-18
# [37] googlesheets4_1.0.1  gtable_0.3.1         formula.tools_1.7.1  globals_0.16.2       processx_3.8.0       timeDate_4021.106
# [43] rlang_1.1.1          ModelMetrics_1.2.2.2 gargle_1.2.1         broom_1.0.1          checkmate_2.1.0      reshape2_1.4.4
# [49] prediction_0.3.14    modelr_0.1.10        backports_1.4.1      Rfast_2.0.6          httpuv_1.6.9         usethis_2.1.6
# [55] tools_4.2.2          lava_1.7.0           ellipsis_0.3.2       RColorBrewer_1.1-3   sessioninfo_1.2.2    Rcpp_1.0.9
# [61] plyr_1.8.8           base64enc_0.1-3      prettyunits_1.1.1    ps_1.7.2             urlchecker_1.0.1     cluster_2.1.4
# [67] fs_1.5.2             magrittr_2.0.3       data.table_1.14.6    reprex_2.0.2         googledrive_2.0.0    mvtnorm_1.1-3
# [73] sjmisc_2.8.9         pkgload_1.3.2        mime_0.12            hms_1.1.2            evaluate_0.18        xtable_1.8-4
# [79] glmtoolbox_0.1.7     sjstats_0.18.2       readxl_1.4.1         gridExtra_2.3        ggeffects_1.2.3      shape_1.4.6
# [85] compiler_4.2.2       rpart.plot_3.1.1     crayon_1.5.2         minqa_1.2.5          htmltools_0.5.4      later_1.3.0
# [91] mgcv_1.8-41          tzdb_0.3.0           Formula_1.2-4        lubridate_1.9.3      DBI_1.1.3            sjlabelled_1.2.0
# [97] dbplyr_2.2.1         boot_1.3-28          cli_3.4.1            mitools_2.4          huxtable_5.5.2       gdata_2.18.0.1
# [103] parallel_4.2.2       insight_0.19.3       gower_1.0.0          pkgconfig_2.0.3      numDeriv_2016.8-1.1  foreign_0.8-83
# [109] recipes_1.0.3        xml2_1.3.3           foreach_1.5.2        hardhat_1.2.0        estimability_1.4.1   prodlim_2019.11.13
# [115] rvest_1.0.3          callr_3.7.3          digest_0.6.30        pls_2.8-1            grpreg_3.4.0         rmarkdown_2.19
# [121] cellranger_1.1.0     htmlTable_2.4.1      operator.tools_1.6.3 shiny_1.7.4          gtools_3.9.4         nloptr_2.0.3
# [127] lifecycle_1.0.3      nlme_3.1-163         jsonlite_1.8.3       fansi_1.0.3          pillar_1.8.1         pkgbuild_1.4.0
# [133] fastmap_1.1.0        httr_1.4.4           survival_3.5-7       remotes_2.4.2.1      glue_1.6.2           bayestestR_0.13.1
# [139] iterators_1.0.14     pander_0.6.5         glmnet_4.1-7         profvis_0.3.7        class_7.3-20         stringi_1.7.8
# [145] performance_0.10.4   memoise_2.0.1        future.apply_1.10.0

#-----------------------------------------------------------------------------#
# Loading in Processed and Cleaned Dataset
#-----------------------------------------------------------------------------#
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data") #setting working directory
data1 <- read.csv("Cleaned_Procesed_MMP_062825.csv", stringsAsFactors = F) #reading in dataframe
nrow(data1) #number of observations: 13,252
data1[data1==9999 | data1==8888]<-NA #making all of the missing variable indicators NA

#-----------------------------------------------------------------------------#
# Final Data Cleaning Before The Multivariable Analysis
#-----------------------------------------------------------------------------#
data <- data1 %>% #the currently loaded data
  #removing crossing attempts prior to the IRCA implementation and before
  #the consequence through delivery system
  filter( Cross_Year > 1993 & Cross_Year < 2011) %>% 
  #removing all women respondents (low proportion)
  filter(Sex == 1) %>%
  #creating new variables
  mutate(San_Deigo = ifelse(California == 1,1,0), #Crossing in the San Diego Border Sector
         Tucson = ifelse(Arizona == 1,1,0), #Crossing in the Tucson Border Sector
         Safeguard = ifelse(Arizona == 1 & Cross_Year > 1996,1,0), #Crossing after Operation Safeguard
         Gatekeeper = ifelse(California == 1 & Cross_Year > 1993,1,0), #Crossing after Operation Gatekeeper
         Hold_The_Line = ifelse(BIG_BEND == 1 & Cross_Year > 1992,1,0), #Crossing after Operation Hold the Line
         Operation_RG = ifelse(RIO_GRANDE == 1 & Cross_Year > 1996,1,0), #Crossing after Operation Rio Grande
         PTD = ifelse(Cross_Year > 1993, 1,0),
         rural = ifelse(size > 3000,1,0)) #Crossing after the Implementation of Prevention through Deterrence
nrow(data) #the number of respondents
summary(data) #the summary of the data file

table(table(data$MMP_id)) #a distribution of the number of crossing trips per migrant
                          #most people have less than 4


#-----------------------------------------------------------------------------#
# Extracting Final Empirical Dataset
#-----------------------------------------------------------------------------#
table(data$First) #First Crossing Trip = 1; else 0
data2 <- data %>%  #the potential cases
  filter(First == 0) %>%  #getting only the experience migrants crossings in the dataset
  #obtaining the variables we need for analysis
  dplyr::select(Coyote, Age, Farm, Educ, Land, Parent, Sibling, Prevalence,
                Child_con, First, Exper, Appreh, Prior, Pre_Coy, Linewatch,
                RIO_GRANDE, BIG_BEND, DEL_RIO, Arizona, California, Texas, 
                Cross_Year, Survey_Year, MMP_id, Community, Weight,
                communityType,era,propertyOwn,rural,MMP_id,CoyNo,Linewatch,
                Last_Coy) %>%
  #creating new variables
  mutate(San_Deigo = ifelse(California == 1,1,0),
         Tucson = ifelse(Arizona == 1,1,0),
         Safeguard = ifelse(Arizona == 1 & Cross_Year > 1996,1,0),
         Gatekeeper = ifelse(California == 1 & Cross_Year > 1993,1,0),
         Hold_The_Line = ifelse(BIG_BEND == 1 & Cross_Year > 1992,1,0),
         Operation_RG = ifelse(RIO_GRANDE == 1 & Cross_Year > 1996,1,0),
         PTD = ifelse(Cross_Year > 1993, 1,0)) 
data2 <- drop_na(data2) #list-wise deletion to remove missing information
mmp.ids <- unique(data2$MMP_id) #the current I
last.crosses <- list() #creating a list to store for each migrant, the last crossing
                       #trip in the period under analysis
for(i in 1:length(mmp.ids)){ #for all migrants in the dataset
  migrant.crossings.i <- data2[data2$MMP_id==mmp.ids[i],] #getting the current migrants information
  last.crosses[[i]] <- migrant.crossings.i[nrow(migrant.crossings.i),] #extracting the most recent crossing attempt
}
data3 <- do.call(rbind,last.crosses) #combining the list into a dataframe
sapply(data3,function(i){sum(is.na(i))/length(i)})*100
#-----------------------------------------------------------------------------#
# Creating the non-coyote specific migration human capital and
# coyote specific migration human capital variables
#-----------------------------------------------------------------------------#
data3$noncoyoteMHC <- data3$Prior - data3$CoyNo
prop.table(table(data3$noncoyoteMHC)) #proportional distribution of non-coyote experience
data3$noncoyoteMHC[data3$noncoyoteMHC>1]<-1 #making it a binary (crossed without a coyote = 1; else = 0)
data3$coyoteMHC <- data3$CoyNo #coyote-specific MHC (number of trips without a coyote)
quantile(data3$coyoteMHC,0.975) #the value at the 97.5 quantile
data3$coyoteMHC[data3$coyoteMHC > quantile(data3$coyoteMHC,0.975)] <- quantile(data3$coyoteMHC,0.975)
#-----------------------------------------------------------------------------#
# Extracting the Descriptive Statistics
#-----------------------------------------------------------------------------#
print(describe(data3),digits=3) #printing the descriptive statistics
#Creating a Function to Compute the Proportion Correctly Classified
predicted_correctly <- function(observed, predicted){
  predicted <- ifelse(predicted > 0.50, 1,0) #if the predicted probabity is greater than 0.50
  cor_yes <- length(which(observed == 1 & predicted == 1)) #how many were predicted "yes" correctly
  cor_no <- length(which(observed == 0 & predicted == 0)) #how many were predicted "no" correctly
  return((cor_yes + cor_no) / length(observed)) #the number predicted correctly / the total numbers
}

#-----------------------------------------------------------------------------#
# Exporting Final Analytical Dataset
#-----------------------------------------------------------------------------#
nrow(data3) #number of observations
data3$communityType<-as.factor(data3$communityType) #the type of community
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data") #setting working directory
write.csv(data3, paste("Final_MMP_Analytical_Dataset_", Sys.Date(),".csv",sep = ""))


#-----------------------------------------------------------------------------#
# Inferential Analysis of MMP Dataset 
# - Logistic Regression with Probability of Coyote Use (Table 2 in the main manuscript)
#-----------------------------------------------------------------------------#
coy.model <- glm(Coyote ~ 
                Age #the age of the migrant (crossing year - 1)
               + Farm  #the occupation of the migrant (crossing year - 1)
               + Educ  #the years of education of the migrant (crossing year - 1)
               + Land  #land ownership of the migrant (crossing year - 1)
               + propertyOwn  #Home ownership of the migrant (crossing year - 1)
               + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
               + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
               + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
               + communityType #the Community Type of Migration (crossing year - 1)
               + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
               + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
               + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
               + noncoyoteMHC #The number of prior crossing attempts 
               + coyoteMHC #Key Dependent: Has the migrant used a coyote in the past?
               + Tucson  #Did the trip occur in the tucson sector
               + RIO_GRANDE #Did the trip occur in the rio grande sector
               + BIG_BEND #Did the trip occur in the big bend sector
               + DEL_RIO #Did the trip occur in the del rio sector
               + Linewatch #Did the trip occur after the prevention through deterrence era?
              ,data = data3 #the analytical dataset
              ,family = binomial(link = "logit")) #logistic regression

summary(coy.model) #summary of model
#community clustered asymptotic standard errors
communitySE <- sandwich::vcovCL(coy.model, cluster = data3$Community)
coeftest(coy.model, vcov. = communitySE) #results with community clustered SEs
AMEs <- avg_slopes(coy.model, vcov = communitySE) #Average marginal effects for each variable
AMEs

#-----------------------------------------------------------------------------#
# Model Fit Assessment
#-----------------------------------------------------------------------------#
glmtoolbox::hltest(coy.model) #Hosmer-Lemeshow Goodness-of-Fit Test
glmtoolbox::gvif(coy.model) #variance inflation factor
lrtest(coy.model) #likelihood ratio test
pscl::pR2(coy.model) #mcfadden r-squared
AIC(coy.model) #AIC
BIC(coy.model) #BIC
predicted_correctly(data3$Coyote, predicted = predict(coy.model, type = "response")) #Proportion predicted correctly
length(unique(data3$Community)) #the number of communities in the empirical dataset



#-----------------------------------------------------------------------------#
# Predicted Probabilities for Ideal Types (Figure 1 in the main manuscript)
#-----------------------------------------------------------------------------#
ideal.types <- avg_predictions(coy.model, variables = list("noncoyoteMHC" = c(0,1), 
                                                           "coyoteMHC" = c(0:max(data3$coyoteMHC))),
                               vcov = communitySE)
ideal.types #print out the predicted probabilities
ideal.types$noncoyoteMHC<-as.factor(ideal.types$noncoyoteMHC)
ideal.types$conf.high[ideal.types$conf.high>1]<-1 #making confidence intervals above 1 to be 1 
                                                  #(theoritically impossible for a probabilty to be greater than 1)


ggplot(ideal.types, aes(x = coyoteMHC, y = estimate, group = noncoyoteMHC)) +
  geom_ribbon(aes(ymin = pmax(conf.low, 0), ymax = conf.high, fill = noncoyoteMHC), alpha = 0.2, color = NA) +
  geom_line(aes(color = noncoyoteMHC), size = 1) +
  geom_point(aes(color = noncoyoteMHC), size = 3) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),labels = c("0" = "No", "1" = "Yes")) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),labels = c("0" = "No", "1" = "Yes")) +
  scale_x_continuous(limits = c(0, max(data3$coyoteMHC)), breaks = seq(0, max(data3$coyoteMHC), 1)) +
  scale_y_continuous(limits = c(0.40, 1), breaks = seq(0.40, 1, 0.05)) +
  labs(
    x = "Number of Prior Undocumented Trips with a Coyote",
    y = "Predicted Probability of Using a Coyote",
    color = "Crossed Without A \nCoyote in the Past", fill = NULL
  ) +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    axis.text.x  = element_text(size = 10, family = "Times New Roman"),
    axis.text.y  = element_text(size = 10, family = "Times New Roman"),
    legend.title = element_text(size = 12, family = "Times New Roman"),
    legend.text  = element_text(size = 10, family = "Times New Roman"))

  

#-----------------------------------------------------------------------------#
# Average Marginal Effects for the Analytical Dataset
#-----------------------------------------------------------------------------#
#Average Marginal Effects for Model for Prior Coyote Experience Across Non-Coyote MHC
#Also called, first differences
AME.across.MHC <- avg_slopes(coy.model, vcov = communitySE, variables = "coyoteMHC",
                             by = "noncoyoteMHC")
AME.across.MHC
#Second Differences Test of Significance
second.diff <- avg_slopes(coy.model, vcov = communitySE, variables = "coyoteMHC",
                          by = "noncoyoteMHC", hypothesis = "b2 - b1 = 0")
second.diff

#-----------------------------------------------------------------------------#

#         End of Main Paper Inferential Analyses

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
#   Appendix: Bootstrapped Standard Errors
#-----------------------------------------------------------------------------#
boot.strap.SE <- sandwich::vcovBS(coy.model, R = 2500, cores = 4)#Bootstrapped Standard Errors with 2500 Replications
lmtest::coeftest(coy.model, vcov. = boot.strap.SE)#Testing the Significance of Results
AME.coyote.exp <- avg_slopes(coy.model, vcov = boot.strap.SE, variables = "coyoteMHC")
AME.coyote.exp
AME.across.MHC <- avg_slopes(coy.model, vcov = boot.strap.SE, variables = "coyoteMHC",
                             by = "noncoyoteMHC")
AME.across.MHC


#-----------------------------------------------------------------------------#
#                     Appendix 
# Firth Penalized Maximum Likelihood Logisitc Regression with Intercept Correction
#-----------------------------------------------------------------------------#
firth.model <- logistf(Coyote ~ 
                         Age #the age of the migrant (crossing year - 1)
                       + Farm  #the occupation of the migrant (crossing year - 1)
                       + Educ  #the years of education of the migrant (crossing year - 1)
                       + Land  #land ownership of the migrant (crossing year - 1)
                       + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                       + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                       + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                       + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                       + communityType #the Community Type of Migration (crossing year - 1)
                       + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                       + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                       + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                       + noncoyoteMHC #The number of prior crossing attempts 
                       + coyoteMHC #Key Dependent: Has the migrant used a coyote in the past?
                       + Tucson  #Did the trip occur in the tucson sector
                       + RIO_GRANDE #Did the trip occur in the rio grande sector
                       + BIG_BEND #Did the trip occur in the big bend sector
                       + DEL_RIO #Did the trip occur in the del rio sector
                       + Linewatch #Did the trip occur after the prevention through deterrence era?
                       ,data = data3 
                       ,family = binomial(link = "logit")
                       ,flic = TRUE)
summary(firth.model)






#-----------------------------------------------------------------------------#
# Appendix: Inferential Analysis of MMP Dataset for Ratio Effect
#-----------------------------------------------------------------------------#
data3$ratiocoyote <- data3$CoyNo/data3$Prior
model.ratio <- glm(Coyote ~ 
                 Age #the age of the migrant (crossing year - 1)
               + Farm  #the occupation of the migrant (crossing year - 1)
               + Educ  #the years of education of the migrant (crossing year - 1)
               + Land  #land ownership of the migrant (crossing year - 1)
               + propertyOwn  #Home ownership of the migrant (crossing year - 1)
               + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
               + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
               + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
               + communityType #the Community Type of Migration (crossing year - 1)
               + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
               + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
               + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
               + ratiocoyote #Key Dependent: The ratio of coyote trips to total trips
               + Tucson  #Did the trip occur in the tucson sector
               + RIO_GRANDE #Did the trip occur in the rio grande sector
               + BIG_BEND #Did the trip occur in the big bend sector
               + DEL_RIO #Did the trip occur in the del rio sector
               + Linewatch #Did the trip occur after the prevention through deterrence era?
               ,data = data3 
               ,family = binomial(link = "logit")) #logistic regression
summary(model.ratio) #summary of model
glmtoolbox::hltest(model.ratio) #Hosmer-Lemeshow Goodness-of-Fit Test
glmtoolbox::gvif(model.ratio) #variance inflation factor
coeftest(model.ratio, vcov. = sandwich::vcovCL(model.ratio, cluster = data3$Community))#Community Clustered SEs
pscl::pR2(model.ratio)
avg_slopes(model.ratio,variables = "ratiocoyote") #Average marginal  effects for the ratio coyote variable






#-----------------------------------------------------------------------------#
# Appendix: Inferential Analysis of MMP Dataset for Ideal Types
#-----------------------------------------------------------------------------#
data3$ideal.type <- 0 # migrants who have no prior coyote experience and migrated without a coyote in the past
data3$ideal.type[data3$coyoteMHC!=0 & data3$noncoyoteMHC==0]<-1  # migrants who have only prior coyote experience 
data3$ideal.type[data3$coyoteMHC!=0 & data3$noncoyoteMHC==1]<-2  # migrants who have have prior coyote experience and migrated without a coyote in the past
data3$ideal.type<-as.factor(data3$ideal.type)
model.ideal.type <- glm(Coyote ~ 
                     Age #the age of the migrant (crossing year - 1)
                   + Farm  #the occupation of the migrant (crossing year - 1)
                   + Educ  #the years of education of the migrant (crossing year - 1)
                   + Land  #land ownership of the migrant (crossing year - 1)
                   + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                   + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                   + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                   + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                   + communityType #the Community Type of Migration (crossing year - 1)
                   + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                   + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                   + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                   + ideal.type #the ideal type varible
                   + Tucson  #Did the trip occur in the tucson sector
                   + RIO_GRANDE #Did the trip occur in the rio grande sector
                   + BIG_BEND #Did the trip occur in the big bend sector
                   + DEL_RIO #Did the trip occur in the del rio sector
                   + Linewatch #Did the trip occur after the prevention through deterrence era?
                   , data = data3
                   ,family = binomial(link = "logit")) #logistic regression

summary(model.ideal.type) #summary of model
coeftest(model.ideal.type, 
         vcov. = sandwich::vcovCL(model.ideal.type, cluster = data3$Community))#Community Clustered SEs
pscl::pR2(model.ideal.type)
avg_slopes(model.ideal.type,variables = "ideal.type",
           vcov = sandwich::vcovCL(model.ideal.type, cluster = data3$Community)) #Average marginal effects for ideal types
avg_slopes(model.ideal.type, variables = "ideal.type", hypothesis = "b1 - b2 = 0", 
           vcov = sandwich::vcovCL(model.ideal.type, cluster = data3$Community)) #Difference in average marginal effects
           #migrants with only prior coyote experience are more likley to use a coyote









#-----------------------------------------------------------------------------#
# Appendix 
# Inferential Analysis of MMP Dataset for Most Recent Trip Coyote Use
#-----------------------------------------------------------------------------#
model.last <- glm(Coyote ~ 
                      Age #the age of the migrant (crossing year - 1)
                    + Farm  #the occupation of the migrant (crossing year - 1)
                    + Educ  #the years of education of the migrant (crossing year - 1)
                    + Land  #land ownership of the migrant (crossing year - 1)
                    + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                    + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                    + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                    + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                    + communityType #the Community Type of Migration (crossing year - 1)
                    + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                    + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                    + Prior #The past undocumetd trips of the migrant who was a migrant (crossing year - 1)
                    + Appreh
                    + Last_Coy #Key Dependent: a binary variable (1 = yes, prior coyote experience; no = 0)
                    + Tucson  #Did the trip occur in the tucson sector
                    + RIO_GRANDE #Did the trip occur in the rio grande sector
                    + BIG_BEND #Did the trip occur in the big bend sector
                    + DEL_RIO #Did the trip occur in the del rio sector
                    + Linewatch #Did the trip occur after the prevention through deterrence era?
                    , data = data3
                    ,family = binomial(link = "logit")) #logistic regression
summary(model.last) #summary of model
glmtoolbox::hltest(model.last) #Hosmer-Lemeshow Goodness-of-Fit Test
glmtoolbox::gvif(model.last) #variance inflation factor
coeftest(model.last, vcov. = sandwich::vcovCL(model.last, cluster = data3$Community))#Community Clustered SEs
pscl::pR2(model.last)
avg_slopes(model.last,variables = "Last_Coy", vcov = sandwich::vcovCL(model.last, cluster = data3$Community)) #Average marginal  effects for the ratio coyote variable
avg_slopes(model.last, variables = "Prior", by = "Last_Coy", vcov = sandwich::vcovCL(model.last, cluster = data3$Community))
avg_slopes(model.last, hypothesis = "b2 - b1 = 0", variables = "Prior", 
           by = "Last_Coy", vcov = sandwich::vcovCL(model.last, cluster = data3$Community))








#-----------------------------------------------------------------------------#
# Appendix: A Test of Quasi-Complete Seperation in the Logisitc Regression
#-----------------------------------------------------------------------------#
library(detectseparation)
detect.sep <- glm(Coyote ~ 
                    Age #the age of the migrant (crossing year - 1)
                  + Farm  #the occupation of the migrant (crossing year - 1)
                  + Educ  #the years of education of the migrant (crossing year - 1)
                  + Land  #land ownership of the migrant (crossing year - 1)
                  + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                  + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                  + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                  + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                  + communityType #the Community Type of Migration (crossing year - 1)
                  + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                  + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                  + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                  + noncoyoteMHC #The number of prior crossing attempts 
                  + coyoteMHC #Key Dependent: Has the migrant used a coyote in the past?
                  + Tucson  #Did the trip occur in the tucson sector
                  + RIO_GRANDE #Did the trip occur in the rio grande sector
                  + BIG_BEND #Did the trip occur in the big bend sector
                  + DEL_RIO #Did the trip occur in the del rio sector
                  + Linewatch #Did the trip occur after the prevention through deterrence era?
                  ,data = data3
                  ,family = binomial(link = "logit")
                  ,method = "detect_separation") #logistic regression
detect.sep











#-----------------------------------------------------------------------------#
#                     Appendix 
# Inferential Analysis of MMP Dataset for Most Recent Trip Coyote Use and 
# Removing Apprehension Variable Given the High Rate of Missingness
#-----------------------------------------------------------------------------#
data.appreh <- data %>%  #the potential cases
  filter(First == 0) %>%  #getting only the experience migrants crossings in the dataset
  #obtaining the variables we need for analysis
  dplyr::select(Coyote, Age, Farm, Educ, Land, Parent, Sibling, Prevalence,
                Child_con, First, Exper, Prior, Pre_Coy, Linewatch,
                RIO_GRANDE, BIG_BEND, DEL_RIO, Arizona, California, Texas, 
                Cross_Year, Survey_Year, MMP_id, Community, Weight,
                communityType,era,propertyOwn,rural,MMP_id,CoyNo,Linewatch) %>%
  #creating new variables
  mutate(San_Deigo = ifelse(California == 1,1,0),
         Tucson = ifelse(Arizona == 1,1,0),
         Safeguard = ifelse(Arizona == 1 & Cross_Year > 1996,1,0),
         Gatekeeper = ifelse(California == 1 & Cross_Year > 1993,1,0),
         Hold_The_Line = ifelse(BIG_BEND == 1 & Cross_Year > 1992,1,0),
         Operation_RG = ifelse(RIO_GRANDE == 1 & Cross_Year > 1996,1,0),
         PTD = ifelse(Cross_Year > 1993, 1,0)) 
data.appreh <- drop_na(data.appreh) #list-wise deletion to remove missing information
mmp.ids <- unique(data.appreh$MMP_id) #the current I
last.crosses <- list() #creating a list to store for each migrant, the last crossing
#trip in the period under analysis
for(i in 1:length(mmp.ids)){ #for all migrants in the dataset
  migrant.crossings.i <- data.appreh[data.appreh$MMP_id==mmp.ids[i],] #getting the current migrants information
  last.crosses[[i]] <- migrant.crossings.i[nrow(migrant.crossings.i),] #extracting the most recent crossing attempt
}
data.appreh <- do.call(rbind,last.crosses) #combining the list into a dataframe
#-----------------------------------------------------------------------------#
# Creating the non-coyote specific migration human capital and
# coyote specific migration human capital variables
#-----------------------------------------------------------------------------#
data.appreh$noncoyoteMHC <- data.appreh$Prior - data.appreh$CoyNo
data.appreh$noncoyoteMHC[data.appreh$noncoyoteMHC>1]<-1 #making it a binary (crossed without a coyote = 1; else = 0)
data.appreh$coyoteMHC <- data.appreh$CoyNo #coyote-specific MHC (number of trips without a coyote)
data.appreh$coyoteMHC[data.appreh$coyoteMHC > quantile(data.appreh$coyoteMHC,0.975)] <- quantile(data.appreh$coyoteMHC,0.975)
data.appreh$communityType<-as.factor(data.appreh$communityType) #the type of community
nrow(data.appreh)
# Logistic Regression Model 
model.noappreh <- glm(Coyote ~ 
                    Age #the age of the migrant (crossing year - 1)
                  + Farm  #the occupation of the migrant (crossing year - 1)
                  + Educ  #the years of education of the migrant (crossing year - 1)
                  + Land  #land ownership of the migrant (crossing year - 1)
                  + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                  + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                  + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                  + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                  + communityType #the Community Type of Migration (crossing year - 1)
                  + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                  + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                  + coyoteMHC #Key Dependent: a binary variable (1 = yes, prior coyote experience; no = 0)
                  + noncoyoteMHC
                  + Tucson  #Did the trip occur in the tucson sector
                  + RIO_GRANDE #Did the trip occur in the rio grande sector
                  + BIG_BEND #Did the trip occur in the big bend sector
                  + DEL_RIO #Did the trip occur in the del rio sector
                  + Linewatch #Did the trip occur after the prevention through deterrence era?
                  , data = data.appreh 
                  , family = binomial(link = "logit")) #logistic regression
summary(model.noappreh) #summary of model
glmtoolbox::hltest(model.noappreh) #Hosmer-Lemeshow Goodness-of-Fit Test
glmtoolbox::gvif(model.noappreh) #variance inflation factor
coeftest(model.noappreh, vcov. = sandwich::vcovCL(model.noappreh, cluster = data.appreh$Community))#Community Clustered SEs
avg_slopes(model.noappreh, vcov = sandwich::vcovCL(model.noappreh, cluster = data.appreh$Community)) #Average marginal  effects for the relevent variables


















#-----------------------------------------------------------------------------#
#                     Appendix 
# Inferential Analysis of MMP Dataset for Most Recent Trip Coyote Use and 
# Keeping The Number of Prior Coyote Trips as the Original Variable
#-----------------------------------------------------------------------------#
original.model <- glm(Coyote ~ 
                   Age #the age of the migrant (crossing year - 1)
                 + Farm  #the occupation of the migrant (crossing year - 1)
                 + Educ  #the years of education of the migrant (crossing year - 1)
                 + Land  #land ownership of the migrant (crossing year - 1)
                 + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                 + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                 + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                 + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                 + communityType #the Community Type of Migration (crossing year - 1)
                 + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                 + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                 + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                 + noncoyoteMHC #The number of prior crossing attempts 
                 + CoyNo #Key Dependent: Has the migrant used a coyote in the past?
                 + Tucson  #Did the trip occur in the tucson sector
                 + RIO_GRANDE #Did the trip occur in the rio grande sector
                 + BIG_BEND #Did the trip occur in the big bend sector
                 + DEL_RIO #Did the trip occur in the del rio sector
                 + Linewatch #Did the trip occur after the prevention through deterrence era?
                 ,data = data3 #removing respondents with more than 10 crosses
                 ,family = binomial(link = "logit")) #logistic regression
summary(original.model) #summary of model
coeftest(original.model, vcov. = sandwich::vcovCL(original.model, cluster =data3$Community))#Community Clustered SEs
pscl::pR2(original.model)
predicted_correctly(data3$Coyote, predicted = predict(original.model, type = "response")) #Proportion predicted correctly












#-----------------------------------------------------------------------------#
# Appendix: Multiple Imputation on the Missing Cases Dataset (10 imputations)
#-----------------------------------------------------------------------------#
mice.data <- data %>%  #the potential cases
  filter(First == 0) %>%  #getting only the experience migrants crossings in the dataset
  #obtaining the variables we need for analysis
  dplyr::select(Coyote, Age, Farm, Educ, Land, Parent, Sibling, Prevalence,
                Child_con, First, Exper, Appreh, Prior, Pre_Coy, Linewatch,
                RIO_GRANDE, BIG_BEND, DEL_RIO, Arizona, California, Texas, 
                Cross_Year, Survey_Year, MMP_id, Community, Weight,
                communityType,era,propertyOwn,rural,MMP_id,CoyNo,Linewatch) %>%
  #creating new variables
  mutate(San_Deigo = ifelse(California == 1,1,0),
         Tucson = ifelse(Arizona == 1,1,0),
         Safeguard = ifelse(Arizona == 1 & Cross_Year > 1996,1,0),
         Gatekeeper = ifelse(California == 1 & Cross_Year > 1993,1,0),
         Hold_The_Line = ifelse(BIG_BEND == 1 & Cross_Year > 1992,1,0),
         Operation_RG = ifelse(RIO_GRANDE == 1 & Cross_Year > 1996,1,0),
         PTD = ifelse(Cross_Year > 1993, 1,0)) 
mmp.ids <- unique(mice.data$MMP_id) #the current I
last.crosses <- list() #creating a list to store for each migrant, the last crossing
#trip in the period under analysis
for(i in 1:length(mmp.ids)){ #for all migrants in the dataset
  migrant.crossings.i <- mice.data[mice.data$MMP_id==mmp.ids[i],] #getting the current migrants information
  last.crosses[[i]] <- migrant.crossings.i[nrow(migrant.crossings.i),] #extracting the most recent crossing attempt
}
mice.data <- do.call(rbind,last.crosses) #combining the list into a dataframe
mice.data$noncoyoteMHC <- mice.data$Prior - mice.data$CoyNo
mice.data$noncoyoteMHC[mice.data$noncoyoteMHC>1]<-1 #making it a binary (crossed without a coyote = 1; else = 0)
mice.data$coyoteMHC <- mice.data$CoyNo #coyote-specific MHC (number of trips without a coyote)
quantile(mice.data$coyoteMHC,0.975) #the value at the 97.5 quantile
mice.data$coyoteMHC[mice.data$coyoteMHC > quantile(mice.data$coyoteMHC,0.975)] <- quantile(mice.data$coyoteMHC,0.975)

mice.data <- mice.data %>% 
  dplyr::select(Coyote, Age, Farm, Educ, Land, propertyOwn, 
         Parent, Sibling, Prevalence, communityType, 
         Child_con, Exper, Appreh, noncoyoteMHC, coyoteMHC, 
         Tucson, RIO_GRANDE, BIG_BEND, DEL_RIO, Linewatch,Community)
 
n.mice.imputations <- 10
mice.coy.data <- mice::mice(mice.data, m = n.mice.imputations, method = c("pmm"), seed = 9999) #5 replicated datsets with seed set to 5 with predictive mean matching

# Logit Model (Key Dependent: Used Coyote (Yes = 1, No = 0))
mice.coy.logits <- with(mice.coy.data, 
                    glm(Coyote ~ 
                          Age #the age of the migrant (crossing year - 1)
                        + Farm  #the occupation of the migrant (crossing year - 1)
                        + Educ  #the years of education of the migrant (crossing year - 1)
                        + Land  #land ownership of the migrant (crossing year - 1)
                        + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                        + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                        + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                        + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                        + communityType #the Community Type of Migration (crossing year - 1)
                        + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                        + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                        + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                        + noncoyoteMHC #The number of prior crossing attempts 
                        + coyoteMHC #Key Dependent: Has the migrant used a coyote in the past?
                        + Tucson  #Did the trip occur in the tucson sector
                        + RIO_GRANDE #Did the trip occur in the rio grande sector
                        + BIG_BEND #Did the trip occur in the big bend sector
                        + DEL_RIO #Did the trip occur in the del rio sector
                        + Linewatch 
                        , family = binomial(link = "logit")))

#Summary of Pooling the Imputed Logit Results
summary(pool(mice.coy.logits))
mean(unlist(lapply(mice.coy.logits$analyses, extractAIC))) #The Mean AIC
mean(unlist(lapply(mice.coy.logits$analyses, function(i){pscl::pR2(i)[4]}))) #Mean McFadden R-Squared
lapply(1:n.mice.imputations,function(i){ 
  avg_slopes(mice.coy.logits$analyses[[i]],variables = "coyoteMHC",
             vcov = sandwich::vcovCL(mice.coy.logits$analyses[[i]], cluster = complete(mice.coy.data,i)$Community))
})








#-----------------------------------------------------------------------------#
# Appendix: Inferential Analysis of MMP Dataset in which
# Prior Coyote Experience is a Binary Variable
# Prior Undocumented Trips is Continous
#-----------------------------------------------------------------------------#
model.binary <- glm(Coyote ~ 
                     Age #the age of the migrant (crossing year - 1)
                   + Farm  #the occupation of the migrant (crossing year - 1)
                   + Educ  #the years of education of the migrant (crossing year - 1)
                   + Land  #land ownership of the migrant (crossing year - 1)
                   + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                   + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                   + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                   + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                   + communityType #the Community Type of Migration (crossing year - 1)
                   + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                   + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                   + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                   + Prior
                   + Pre_Coy#Key Dependent: The ratio of coyote trips to total trips
                   + Tucson  #Did the trip occur in the tucson sector
                   + RIO_GRANDE #Did the trip occur in the rio grande sector
                   + BIG_BEND #Did the trip occur in the big bend sector
                   + DEL_RIO #Did the trip occur in the del rio sector
                   + Linewatch #Did the trip occur after the prevention through deterrence era?
                   ,data = data3 
                   ,family = binomial(link = "logit")) #logistic regression
summary(model.binary) #summary of model
coeftest(model.binary, vcov. = sandwich::vcovCL(model.binary, cluster = data3$Community))#Community Clustered SEs
pscl::pR2(model.binary)
avg_slopes(model.binary, vcov = sandwich::vcovCL(model.binary, cluster = data3$Community)) #Average marginal  effects for the ratio coyote variable
avg_slopes(model.binary, hypothesis = "b2 - b1 = 0", variables = "Prior", 
           by = "Pre_Coy", vcov = sandwich::vcovCL(model.binary, cluster = data3$Community))




#-----------------------------------------------------------------------------#
# Appendix: Inferential Analysis of MMP Dataset in which
# Non-Coyote Migration Human Captial is Measured by a Continous Measure
#-----------------------------------------------------------------------------#
data3$noncoyoteMHC <- data3$Prior - data3$CoyNo
model.noncoyote.continous <- glm(Coyote ~ 
                      Age #the age of the migrant (crossing year - 1)
                    + Farm  #the occupation of the migrant (crossing year - 1)
                    + Educ  #the years of education of the migrant (crossing year - 1)
                    + Land  #land ownership of the migrant (crossing year - 1)
                    + propertyOwn  #Home ownership of the migrant (crossing year - 1)
                    + Parent #Binary for if the parent of the migrant was a migrant (crossing year - 1)
                    + Sibling #The number of siblings of the migrant who was a migrant (crossing year - 1)
                    + Prevalence  #the Community Prevalance of Migration (crossing year - 1)
                    + communityType #the Community Type of Migration (crossing year - 1)
                    + Child_con #The number of children of the migrant who was a migrant (crossing year - 1)
                    + Exper #The months of us experience of the migrant who was a migrant (crossing year - 1)
                    + Appreh #The past apprehensions of the migrant who was a migrant (crossing year - 1)
                    + noncoyoteMHC
                    + CoyNo#Key Dependent: The ratio of coyote trips to total trips
                    + Tucson  #Did the trip occur in the tucson sector
                    + RIO_GRANDE #Did the trip occur in the rio grande sector
                    + BIG_BEND #Did the trip occur in the big bend sector
                    + DEL_RIO #Did the trip occur in the del rio sector
                    + Linewatch #Did the trip occur after the prevention through deterrence era?
                    ,data = data3 
                    ,family = binomial(link = "logit")) #logistic regression
summary(model.noncoyote.continous) #summary of model
coeftest(model.noncoyote.continous, vcov. = sandwich::vcovCL(model.noncoyote.continous, 
                                                             cluster = data3$Community))#Community Clustered SEs
pscl::pR2(model.noncoyote.continous)
avg_slopes(model.noncoyote.continous, vcov = sandwich::vcovCL(model.noncoyote.continous, 
                                                              cluster = data3$Community)) #Average marginal  effects for the ratio coyote variable








#-----------------------------------------------------------------------------#

#         End of Inferential Appendix Analyses

#-----------------------------------------------------------------------------#

