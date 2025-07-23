#################################################################################
#     R Code for Processing for "Coyotaje in an Uncertain Social Environment: Evidence from Prior Coyote Experience"
#     Code Written By: *********
#     Last Updated: 08-24-24
#################################################################################
rm(list = ls()) #clear memory
library(dplyr) #loading R dplyr package
library(haven) #loading haven to output STATA files
library(data.table)
sessionInfo() #session information for Replication
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
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] dplyr_1.1.4       data.table_1.15.4 survival_3.5-7   
# 
# loaded via a namespace (and not attached):
# [1] tidyr_1.2.1         bit64_4.0.5         splines_4.2.2       modelr_0.1.10       RcppParallel_5.1.7  Formula_1.2-4      
# [7] statmod_1.5.0       pander_0.6.5        bayestestR_0.13.1   numDeriv_2016.8-1.1 pillar_1.9.0        backports_1.4.1    
# [13] lattice_0.20-45     glue_1.6.2          jtools_2.2.2        digest_0.6.30       minqa_1.2.5         colorspace_2.0-3   
# [19] sandwich_3.0-2      glmtoolbox_0.1.7    Matrix_1.5-1        pkgconfig_2.0.3     broom_1.0.1         sjPlot_2.8.14      
# [25] haven_2.5.1         purrr_1.0.2         xtable_1.8-4        mvtnorm_1.1-3       scales_1.3.0        lme4_1.1-32        
# [31] emmeans_1.8.7       tibble_3.2.1        generics_0.1.3      ggplot2_3.5.1       sjlabelled_1.2.0    ellipsis_0.3.2     
# [37] TH.data_1.1-2       pacman_0.5.1        cli_3.6.2           magrittr_2.0.3      crayon_1.5.2        estimability_1.4.1 
# [43] mice_3.15.0         fansi_1.0.3         nlme_3.1-163        MASS_7.3-58.1       forcats_0.5.2       RcppZiggurat_0.1.6 
# [49] tools_4.2.2         hms_1.1.2           lifecycle_1.0.3     multcomp_1.4-25     munsell_0.5.0       ggeffects_1.2.3    
# [55] Rfast_2.1.0         compiler_4.2.2      rlang_1.1.1         grid_4.2.2          nloptr_2.0.3        rstudioapi_0.14    
# [61] boot_1.3-28         gtable_0.3.1        codetools_0.2-18    sjstats_0.18.2      sjmisc_2.8.9        R6_2.5.1           
# [67] zoo_1.8-11          knitr_1.41          performance_0.10.4  bit_4.0.5           utf8_1.2.2          insight_0.19.3     
# [73] pscl_1.5.5          parallel_4.2.2      Rcpp_1.0.11         vctrs_0.6.5         tidyselect_1.2.0    xfun_0.35          
# [79] coda_0.19-4         lmtest_0.9-40  

################################################################################

# Creating a Pre-Processing Dataset That Stacks the Dataset for 30 Crossing Trips
# Downlaod Dataset Instructions Here: https://mmp.opr.princeton.edu/databases/instructions-en.aspx
# Downlaod Dataset Here: https://oprdata.princeton.edu/archive/mmp/ 

################################################################################
# downloading the data from MMP
stackMMPDATA <- TRUE #should the original MMP dataset be restacked? where each row
                     #is a crossing trip
if(stackMMPDATA==TRUE){
#-----------------------------------------------------------------------------#
  # This code chunk is slow, so it only should be processed when need be.
#-----------------------------------------------------------------------------#
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data") #setting working directory
MMP <- read.csv("mig174_03_01.csv", stringsAsFactors = F) #loading in the dataset

nrow(MMP) #number of observations = 9052
max.cross <- 30 #the maximum number of possible crossings
### obtaining the data and repeating the obsevations for 30 times
data <- MMP %>% 
    mutate(id=1:n()) %>%
    slice(rep(1:n(), each = max.cross)) %>% #repeating each row 30 times (Stacking the data)
    mutate(row_iden=rep(1:max.cross,n()/max.cross))#creating a row identifier

nrow(data) == nrow(MMP) * max.cross #if TRUE, rows were successfully repeated 30 times
#### Now to split the dataset up
data.split <- split(data,f=data$id)
  
data.split1 <- lapply(1:nrow(MMP),function(i){
    #year of  crossing attempt
    data.split[[i]]$cross_year <- as.vector(unlist(unlist(MMP[i,(paste("crsyr", 1:max.cross, "", sep = ""))])))
    #coyote used on crossing attempt
    data.split[[i]]$cross_how <- as.vector(unlist(unlist(MMP[i,(paste("crshow", 1:max.cross, "", sep = ""))])))
    #coyote used on crossing attempt
    data.split[[i]]$cross_coy <- as.vector(unlist(unlist(MMP[i,(paste("crscoy", 1:max.cross, "", sep = ""))])))
    #success of crossing attempt
    data.split[[i]]$success <- as.vector(unlist(unlist(MMP[i,(paste("crsyes", 1:max.cross, "", sep = ""))])))
    #number of deportations of crossing attempt
    data.split[[i]]$deport <- as.vector(unlist(unlist(MMP[i,(paste("crsdep", 1:max.cross, "", sep = ""))])))
    ##cost of coyote on the crossing attempt
    data.split[[i]]$cost <- as.vector(unlist(unlist(MMP[i,(paste("crscst", 1:max.cross, "", sep = ""))])))
    #state crossed on crossing attempt
    data.split[[i]]$state <- as.vector(unlist(unlist(MMP[i,(paste("crsst", 1:max.cross, "", sep = ""))])))
    ##place crossed on crossing attempt
    data.split[[i]]$place <- as.vector(unlist(unlist(MMP[i,(paste("crspl", 1:max.cross, "", sep = ""))])))
    #who payed the coyote on the crossing attempt
    data.split[[i]]$pay <- as.vector(unlist(unlist(MMP[i,(paste("crspay", 1:max.cross, "", sep = ""))])))
    data.split[[i]] <- data.table::as.data.table(data.split[[i]])
    })

data.stacked <- data.table::rbindlist(data.split1) #remerging the large dataset

#-----------------------------------------------------------------------------#
# Removing all crossing trips that did not cross illicitly and without year information
#-----------------------------------------------------------------------------#
data.stacked <- data.stacked[data.stacked$cross_coy != 8888]#dropping rows that did not cross illicitly
data.stacked <- data.stacked[data.stacked$cross_coy != 9999]#dropping rows that did not cross illicitly
data.stacked <- data.stacked[data.stacked$cross_how != 8888]#dropping rows that did not cross illicitly
data.stacked <- data.stacked[data.stacked$cross_year != 9999]#dropping rows that did not cross illicitly
nrow(data.stacked) #16625 observations
# Exporting Stacked Dataset
write.csv(data.stacked, "stacked_MMP_data_onlytrips_062625.csv")  #writing data as csv (exporting the data)

}

################################################################################

# Loading In Stacked Pre-Processing Dataset 

################################################################################
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data")
data <- fread("stacked_MMP_data_onlytrips_062625.csv", stringsAsFactors = F) #reading the data
nrow(data) #16625 observations
data$MMP_id <- data$commun + (data$hhnum * 10000)#create unique ID based on MMP suggestion

################################################################################

# Loading In All Other MMP Datasets with Migration Information

################################################################################
#-----------------------------------------------------------------------------#
# There are a couple of crossing trips in which the year of the survey is before
# the crossing trip. Since the surveys are based on past historical crossing trips
# of each respondent, this cannot happen. We code this as a MMP coding mistake
# a drop those cases (N = 25).
#-----------------------------------------------------------------------------#
dif <- data$cross_year-data$surveyyr  #year of trip - year of survey (should be positive)
dropRows <- ifelse((dif > 0 & dif < 10),1,0) #there are some differences that are 8011, this occurs 
                                      #when the respondent does not know the year of trip
old <- nrow(data)
data <- data[dropRows == 0] #removing those rows of coding mistakes
old - nrow(data)#the number of dropped rows

house <- read.csv("house174.csv", stringsAsFactors = F)#import data
house$MMP_id <- house$commun + (house$hhnum * 10000) #create unique ID based on MMP suggestion
house <- house[which(house$MMP_id %in% data$MMP_id), ] #dropping rows that didn't cross illegally

commun <- read.csv("commun174.csv", stringsAsFactors = F)#import data
natlhist <- read.csv("natlhist2019.csv", stringsAsFactors = F)#import data
natlyr <- read.csv("natlyear2019.csv", stringsAsFactors = F)#import data
prev <- read.csv("prevratio174.csv", stringsAsFactors = F)#import data

pers.1 <- read.csv("pers174.csv", stringsAsFactors = F)#import data
pers.1$MMP_id <- pers.1$commun + (pers.1$hhnum * 10000)#create unique ID based on MMP suggestion
pers.1 <- pers.1[which(pers.1$MMP_id %in% data$MMP_id), ] #dropping rows that didn't cross illegally

child <- subset(pers.1, pers.1$relhead == 3) #creating persons file for only children
spouse <- subset(pers.1, pers.1$relhead == 2) #creating persons file for only spouse
person <- subset(pers.1, pers.1$relhead == 1) #creating persons file for only hoh

life <- read.csv("life174.csv", stringsAsFactors = F)
life$MMP_id <- life$commun + (life$hhnum * 10000)#create unique ID based on MMP suggestion
life <- life[which(life$MMP_id %in% data$MMP_id), ] #dropping rows that didn't cross illegally

m1 <- merge(data, commun, by.x = "commun", by.y = "COMMUN") #Merging the Community Dataset
ncol(data) + ncol(commun) - 1 == ncol(m1) #Checking the Number of Observations
m2 <- merge(m1, house, by = "MMP_id", all.x =  T) #Merging the House Dataset
nrow(m2) #number of observations
################################################################################
# Exporting Final Merged Dataset 
################################################################################
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data")
write.csv(m2,"Preprocessed_MMP_Merged_Data_Files_062625.csv")
################################################################################

# Creating all variables for analysis

################################################################################

################################################################################

# Dependnet Variable: Coyote Use

################################################################################

################################################################################
# Crossing Mode for Crossing Attempt
################################################################################
# creating variable for crossing mode
m2$c_mode <- 1111 
m2$c_mode[m2$cross_coy == 1] <- 2#if coyote was used, value == 2
m2$c_mode[m2$cross_coy == 2 & m2$cross_how == 1] <- 0#if no coyote, and crossed alone
m2$c_mode[m2$cross_coy == 2 & m2$cross_how == 5] <- 0#if no coyote, and crossed with stranger 
m2$c_mode[m2$cross_coy == 2 & m2$cross_how == 2] <- 1#if no coyote, and crossed with friend
m2$c_mode[m2$cross_coy == 2 & m2$cross_how == 3] <- 1#if no coyote, and crossed with family
m2$c_mode[m2$cross_coy == 2 & m2$cross_how == 4] <- 1#if no coyote, and crossed with friends and family
m2$c_mode[m2$cross_coy == 9999 & m2$cross_how == 9999] <- 1111 #if unknown for both coyote usage and crossed with
m2$c_mode[m2$c_mode == 1111] <- NA #Setting missing values to NA
m3<-m2#copying the dataset
################################################################################
# Coyote Use
################################################################################
#creating a binary variable for crossing with a coyote (1 = yes, 0 = no, 9999 = NA (missing))
m3$cross_coy <- ifelse(m3$cross_coy == 1,1,0)
table(m3$cross_coy,useNA = "always") #should be no NAs here

################################################################################

# General Human Captial Variables

################################################################################
##### General Human Capital: Age, Education, Farm Occupation, Sex

#creating a dummy variable for farm origin (usual occupation in agriculture)
m3$farm <- NA #Farm Variable set to 0
m3$age_c <- NA #Age Variable set to 0
m3$educ <- NA #Education Variable set to 0
m3$marriage <- NA #Marriage Variable set to 0
m3$mex_work <- NA #Prior Work Experience Variable set to 0
life$jobdur[life$jobdur == 9999] <- NA
m3$own_land <- 0 #Creating a dummy variable for if the migrant owns land
m3$own_property <- 0 #creating a dummy variable for if the migrant owned a home (i.e., property)

#-----------------------------------------------------------------------------#
# Removing rows of information due to data concerns
#-----------------------------------------------------------------------------#
m3 <- subset(m3, m3$MMP_id != 10010002) #Deleting This Specific Individual From the Dataset
m3 <- subset(m3, m3$MMP_id !=  1470154) #Deleting This Specific Individual From the Dataset

#-----------------------------------------------------------------------------#
# Since this takes some time to run, we can print a progress bar for an update
# making a progress bar
#-----------------------------------------------------------------------------#
pb <- txtProgressBar(min=1,max=nrow(m3),style = 3)
for(i in 1:nrow(m3)){ #For all Individuals in the Dataset
  setTxtProgressBar(pb, i) #printing the progress
  lifei <- life[life$MMP_id == m3$MMP_id[i] & (m3$cross_year[i] - 1) == (life$year ) ,]
  #Farm Experience in the Year Prior to the Migration Trip
  m3$farm[i] <- ifelse(length(lifei$occup) == 0, NA,lifei$occup)
  #Age in the Year Prior to the Migration Trip
  m3$age_C[i] <- lifei$age
  #Education in the Year Prior to the Migration Trip
  m3$educ[i] <- lifei$educ
  #Martial Status in the Year Prior to the Migration Trip
  m3$marriage[i] <- lifei$married
  #Work Experience in the Year Prior to the Migration Trip
  m3$mex_work[i] <- sum(life$jobdur[life$year %in% min(life$year[life$MMP_id ==  m3$MMP_id[i]]):(m3$cross_year[i] -1 ) & life$MMP_id ==  m3$MMP_id[i]] , na.rm = T)
  # Did the migrant own land in the year prior to migration
  m3$own_land[i] <- lifei$land
  # Did the migrant own property in the year prior to migration
  m3$own_property[i] <- lifei$property
}
close(pb)
m3$own_land[m3$own_land > 1] <- 1
m3$own_property[m3$own_property > 1] <- 1
fm <- c(410:419) #Farm Values Based on MMP Appendices
m3$farm_occup <- 0 #Farm Occupation Set to 0
m3$farm_occup[which(m3$farm %in% fm)] <- 1 #If Value is In The Farm (Agriculture Values)
m3$farm_occup[m3$farm == 9999] <- NA #Missing Values set to NA
table(m3$farm_occup, m3$farm, useNA = "always")
m3$sex[m3$sex == 2] <- 0 #recoding females from 2 to 0
m3$educ[m3$educ == 9999] <- NA #if value is missing, make the value an NA
#-----------------------------------------------------------------------------#
# Removing rows of information due to individuals being younger than 15 (Angelucci 2012)
#-----------------------------------------------------------------------------#
m3 <- subset(m3, m3$age_C > 14) #dropping all individuals who are younger than 15 (Angelucci 2012)
m3$mex_work <- m3$mex_work / 100 #Dividing Prior Work Experience By 100 for (hundreds of month of experience)


################################################################################

# General Social Captial Variables

################################################################################

#using the flag variables creating by the MMP 
m3$mother <- 0  #dummy variable for mother is a migrant
m3$father <- 0  #dummy variable for father is a migrant
m3$parent <- 0  #dummy variable for parent is a migrant
m3$brother <- 0 #dummy variable for brother is a migrant
m3$sister <- 0 #dummy variable for sister is a migrant
m3$sibling <- 0 #dummy variable for sibling is a migrant

for(i in 1:nrow(m3)){#For all migrants
  #Mother migration experience prior to trip year
  m3$mother[i] <- life$mousmig[life$MMP_id == m3$MMP_id[i] & (m3$cross_year[i] - 1) == (life$year) ]
  #Father migration experience prior to trip year
  m3$father[i] <- life$fausmig[life$MMP_id == m3$MMP_id[i] & (m3$cross_year[i] - 1) == (life$year) ]
  #Parent migration experience prior to trip year
  m3$parent[i] <- ifelse(m3$father[i] == 1 | m3$mother[i] == 1,1,0) #if either of the parent is a migrant = 1, else 0
  #Brother migration experience prior to trip year
  m3$brother[i] <- life$brousmig[life$MMP_id == m3$MMP_id[i] & (m3$cross_year[i] - 1) == (life$year) ]
  #Sister migration experience prior to trip year
  m3$sister[i] <- life$sisusmig[life$MMP_id == m3$MMP_id[i] &(m3$cross_year[i] - 1) == (life$year) ]
  #Sibling migration experience prior to trip year
  m3$sibling[i] <- m3$sister[i] +  m3$brother[i]#Adding the number of siblings for are migrants
}
# Communal prevalence of migration
m3$prevalence <- NA #The communal prevalence of migration
for(i in 1:nrow(m3))#For all migrants
{
  if(((m3$cross_year[i] - 1) %in% prev$year)){
  #The prevalence of migration in the community in the year prior to migration
  m3$prevalence[i] <- prev$pratio[prev$commun == m3$commun.x[i] & prev$year == (m3$cross_year[i] - 1)]
}
}
m3$prevalence <- m3$prevalence / 100 #Diving the ratio by 100 to make it the proportion
################################################################################

# Need to add community population size here: COMPOP# 

################################################################################

m3$communitysize <- NA #community size in year t - 1
for(i in 1:nrow(m3)){
  
  communityi <- m3$commun.x[i]
  communityData <- filter(commun, COMMUN == communityi)
  yearC <- m3$cross_year[i] #getting the  year of crossing
  
  if(yearC %in% 1950:1959){ #if the crossing attempt ocurred within 1950-1959, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP50
  }
  if(yearC %in% 1960:1969){#if the crossing attempt ocurred within 1960-1969, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP60
  }
  if(yearC %in% 1970:1979){#if the crossing attempt ocurred within 1970-1979, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP70
  }
  if(yearC %in% 1980:1989){#if the crossing attempt ocurred within 1980-1989, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP80
  }
  if(yearC %in% 1990:1999){#if the crossing attempt ocurred within 1990-1999, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP90
  }
  if(yearC %in% 2000:2009){#if the crossing attempt ocurred within 2000-2009, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP00
  }
  if(yearC %in% 2010:2020){#if the crossing attempt ocurred within 2010-2020, since MMP uses a 10 year sliding window
    m3$communitysize[i] <- communityData$COMPOP10
  }
}

#following Massey 2016 (AJS),  measuring community type
m3$communityType <- NA
m3$communityType[m3$communitysize < 2501] <- 0 #a rural area: (<2501)
m3$communityType[m3$communitysize > 2500 & m3$communitysize < 10000 ] <- 1 #a	Town (2,501-9,999)
m3$communityType[m3$communitysize > 9999 & m3$communitysize < 100000 ] <- 2 #a	Small city (10,000 -99,999)
m3$communityType[m3$communitysize > 99999 ] <- 3 #a	Large Urban Area (100000+)

################################################################################

# Migration Specific Human Capital

################################################################################
##### Migration Specific Human Capital: US Experience, Prior US Trips, Prior Apprehensions

#creating a variable called prior US trips
m3$prior <- 0 #creating a new variable called prior
m3$prior[m3$row_iden > 1] <- m3$row_iden[m3$row_iden > 1] - 1 #prior is equal to the specific crossing attempt of i minus 1
m3$first <- 0 #A dummy variable for if this trip is the first trip
m3$first[m3$prior == 0] <- 1 #If prior migration is equal to 0, then the value is 1

#creating a variable called prior apprehension
m3$appreh <- NA #a dummy variable for prior apprehensions
m3$deport[m3$deport == 9999] <- NA #if value is missing make NA
m3$appreh[m3$row_iden == 1] <- 0 #if person only crossed once in total, no prior apprehensions
cbind(m3$MMP_id, m3$row_iden,m3$deport,m3$appreh) #looking at the apprehension data
m3 <- m3[with(m3, order(m3$MMP_id, m3$row_iden)),] #order this datafile
m3 <- m3 %>% 
  dplyr::relocate(row_iden, .before = commun.x)  #resorting the datafile
for(i in 1:nrow(m3)){ #for all migrants in the dataset
  if(m3$row_iden[i] > 1) #If prior migrations is greater than 1
  {
    if(m3$row_iden[i] == 2) #if this is the second migration
    { 
      if(m3$MMP_id[i-1] == m3$MMP_id[i]){
      m3$appreh[i] <- m3$deport[i-1]    }
      } 
    if(m3$row_iden[i] > 2)
    { m3$appreh[i] <- mean(m3$deport[m3$MMP_id == m3$MMP_id[i] & m3$row_iden < m3$row_iden[i]], na.rm = T)}
  }
  }
table(m3$appreh)
m3$appreh[is.nan(m3$appreh)]<-NA #If the value is missing, make it NA
m3$usexp_c <- 0 #Prior US experience in months
for(i in 1:nrow(m3)){ #for all migrants
  # the prior cumulative us experience in the year prior to the migration trip
  m3$usexp_c[i] <- life$uscumexp[life$MMP_id == m3$MMP_id[i] & (m3$cross_year[i] - 1) == (life$year) ]
}
m3$usexp_c[m3$usexp_c == 9999] <- NA #If the value is missing, make it NA
m3$usexp_c <- m3$usexp_c / 100 #Divide this value by 100

################################################################################

# Migration Specific Social Capital

################################################################################
##### Migration Specific Social Capital: Spouse a US Migrant, Child US Migrant
m3$child_mig <- 0 #the number of children who have us migration experience
for(i in 1:nrow(child)) #for all sample points in the child file
{
  for(j in 1:nrow(m3)) #for all respondents
  {
    if(m3$MMP_id[j] == child$MMP_id[i]) #If this ID mathces in the child and head of household dataset
    {
      #Essentially, does the child migrant have migration experience before the parent's crossing year
      ifelse(child$usyr1[i] <= (m3$cross_year[j] - 1) || child$usyrl[i] <= (m3$cross_year[j] - 1),
             m3$child_mig[j] <- 1, #value is 1
             ifelse(child$usyr1[i] == 9999 || child$usyrl[i] == 9999 & m3$child_mig[j] != 1 , #if the value is missing
                    m3$child_mig[j] <- NA, print(i))) #make it NA
      
    }
  }
}

m3$child_mig[m3$row_iden == 1] <- 0 #making the information on the first cross to be 0
m3$child_mig_continous <- 0 #a continous measure of the number of children who are US migrant
childIDs <- (unique(child$MMP_id)) #the unique child MMP IDS
for(i in 1:nrow(m3)) #for all migrants
{
  if(m3$MMP_id[i] %in% childIDs){ #if this mmp id is in the child ids
    dog <- subset(child, child$MMP_id == m3$MMP_id[i]) #subset to get migrant i information
    yearcr <- (m3$cross_year[i] - 1) #the year prior to the crossing year
    d <- pmin(dog$usyrl, dog$usyr1) #the min crossing year of the children
    before <- which(d <= yearcr) %>% length(.) #the number of children who have migration experience prior to crossing year
    m3$child_mig_continous[i] <- before #returning the number of trips
  }
}
m3$spouse_mig <- 0 #Does the spouse have US migration experience? 
for(i in 1:nrow(spouse)) #for all spouses
{
  for(j in 1:nrow(m3)) #for all migrants
  {
    if(m3$MMP_id[j] == spouse$MMP_id[i]) #if the ids do match
    {
      #Essentially, does the spouse  have migration experience before the parent's crossing year
      ifelse(spouse$usyr1[i] <= (m3$cross_year[j] - 1) || spouse$usyrl[i] <= (m3$cross_year[j] - 1),
             m3$spouse_mig[j] <- 1,#value is 1
             ifelse(spouse$usyr1[i] == 9999 || spouse$usyrl[i] == 9999 & m3$spouse_mig[j] != 1,#if the value is missing
                    m3$spouse_mig[j] <- NA, print(i))) #make it NA
    }
  }
}

m3$spouse_mig[m3$row_iden == 1] <- 0 #making the information on the first cross to be 0
################################################################################

# Control Variables Based on US Border Enforcement Efforts and Cost of Migration

################################################################################

##### Controls: Place of Crossing, Cohort Size, Coyote Cost
#place of crossing
table(m3$place)
table(m3$state)

##### Using Information Based on Singer and Massey 1998 coding
m3$place_crossed <- 8
#Tijuana (state = 2, place = 4)
m3$place_crossed[m3$state == 2 & m3$place == 4 ] <- 0
#Mexicali (state = 2, place = 2)
m3$place_crossed[m3$state == 2 & m3$place == 2] <- 1
#Juarez (state = 8, place = 37)
m3$place_crossed[m3$state == 8 & m3$place == 37] <- 2
#Nuevo Laredo (state = 28, place = 27)
m3$place_crossed[m3$state == 28 & m3$place == 27] <- 3
#Piedras Negras (state = 5, place = 25)
m3$place_crossed[m3$state == 5 & m3$place == 25] <- 4
#Nogales (state = 26, place = 43)
m3$place_crossed[m3$state == 26 & m3$place == 43] <- 5
#Reynosa (state = 28, place = 32)
m3$place_crossed[m3$state == 28 & m3$place == 32] <- 6
#Matamoros (state = 28, place = 22)
m3$place_crossed[m3$state == 28 & m3$place == 22] <- 7
#Unknown/Other (state = 9999, place = 9999)
m3$place_crossed[m3$state == 9999 & m3$place == 9999] <- 8
table(m3$place_crossed)

##### US Enforcement Efforts: Linewatch Hours, Drug Arrests

m3$linewatch <- NA #creating a new variable called line watch
m3$drug_arrests <- NA #creating a new variable called drug arrests
m3$agents <- NA #creating a new variable called agents
m3$budget <- NA #creating a new variable called budegts
m3$interest <- NA #creating a new variable called interest
m3$caught <- NA #creating a new variable called caught
m3$prob_app <- NA #creating a new variable called probability of apprehensions

#-----------------------------------------------------------------------------#
# Removing rows of information due to crossing year being outside of a certain range
#-----------------------------------------------------------------------------#
m3 <- subset(m3, m3$cross_year < 2018) #Removing cases based on missing macro-level information
m3 <- subset(m3, m3$cross_year > 1964) #Removing cases based on missing macro-level information

for(i in 1:nrow(m3)) #for every row in data.2
{
  m3$linewatch[i] <- natlhist$lwhrs[natlhist$year == m3$cross_year[i]] #linewatch hours are equal to the linewatch hours of that specific year
  m3$drug_a[i] <- natlhist$mxnardep[natlhist$year == m3$cross_year[i]]#drug arrests are equal to the drug arrests of that specific year
  m3$agents[i] <-  natlhist$bpeoffcr[natlhist$year == m3$cross_year[i]] #number of border enforcement agetns
  m3$budget[i] <- natlyr$bpebudgt[natlyr$year == m3$cross_year[i]] #border patrol budget
  m3$interest[i] <- natlyr$mexrlint[natlyr$year == m3$cross_year[i]] #mexico real interest rate
  m3$caught[i] <- natlhist$aliensmg[natlhist$year == m3$cross_year[i]] #number of migrants caught being smuggled
  m3$prob_app[i] <-  natlyr$probapp[natlyr$year == m3$cross_year[i]] #probabilty of apprehension in each year
 }
m3$drug_a <- m3$drug_a / 1000 #rescaling the variable
m3$linewatch.x <- m3$linewatch / 1000000 #rescaling the variable
m3$agents <- m3$agents / 100 #rescaling the variable
m3$caught <- m3$caught / 1000 #rescaling the variable
#### Neoclassical Models of Migration ##
m3$usunemploy <- 0 #Us unemployment dummy variable
for(i in 1:nrow(m3)){
  m3$usunemploy[i] <- natlyr$usunemp[natlyr$year == m3$cross_year[i]] #The US unemployment rate in the crossing year
  
}

################################################################################

# Exporting Dataset with All Cleaned Variables

################################################################################
m4 <- as.data.frame(cbind(
 MMP_id = m3$MMP_id, #unique participant ID,
 Community = m3$commun.x, #community number
 House_Num =  m3$hhnum.x, #household number
 Survey_Year =  m3$surveyyr.x, #survey year
 Attempt = m3$row_iden, #unique crossing attempt
 Cross_Mode =  m3$c_mode, #mode of crossing: key dependent
 Coyote = m3$cross_coy, #crossing with a coyote: key dependent
 Cross_Year = m3$cross_year, #year of crossing
 First = m3$first, #times crossed
 Cost = m3$cost,   #cost of coyote
 Cross_Place = m3$place_crossed,  #place of crossing
 State = m3$state, #crossing state
 Place = m3$place, #place crossed
 Age = m3$age_C,  #age at time of crossing 
 Labor = m3$mex_work, #mexican labor experience at time of crossing
 Parent = m3$parent, #parent a US migrant prior
 Linewatch = m3$linewatch,    #line watch hours at year of cross
 Drug = m3$drug_a, #drug arrests at the year of cross
 Prior = m3$prior, #prior crossing attempts
 Appreh = m3$appreh, #average number of prior apprehensions
 Educ = m3$educ, #years of education
 Farm = m3$farm_occup, #farm origin
 Married = m3$marriage, #martial status at cross attempt
 Sibling =  m3$sibling, #was the sibling a migrant
 Child_con = m3$child_mig_continous, #the number of children with migration experience
 Land = m3$own_land, #does the migrant own land
 Exper = m3$usexp_c, #us prior experience
 Spouse = m3$spouse_mig, #has spouse migrated prior to the cross?
 Prevalence = m3$prevalence, #Proportion of community aged 15+ with U.S. experience at time of trip.
 Child = m3$child_mig,#was the child a migrant to the trip 
 Sex = m3$sex, #sex
 Agents = m3$agents, #agents
 BP_Budget = m3$budget, #INS budget
 Caught = m3$caught, #Number of Coyotes Caught
 RLIT = m3$interest, #Mexico's Real Interest Rate
 Prob_App = m3$prob_app, #Probability of Apprehension
 Success = m3$success, #Was the Trip Successful
 US_unemploy = m3$usunemploy, #US unemployment rate
 Weight = m3$weight.x, #sampling weights
 communityType = m3$communityType, #the type of community in the dataset
 propertyOwn = m3$own_property,
 size = m3$communitysize
))


################################################################################

# Doing final data cleaning and processing!

################################################################################

#-----------------------------------------------------------------------------#
# Removing rows of information due to recall response bias due to time of crossing difference
#-----------------------------------------------------------------------------#
m4$difference <- m4$Survey_Year - m4$Cross_Year #difference between time of survey and
                                                #time of survey year (Deleting Cross)
m4$difference[m4$difference > 25] <- -100 #If crossing was more than 25 years, recode the value# to -100
m4 <- subset(m4,m4$difference != 25)


#-----------------------------------------------------------------------------#
# Creating Last Cross and First Crossing Attempt Indicators
#-----------------------------------------------------------------------------#
m4$last_cross <- 0 #creating a dummy variable for last cross
last.list <- matrix(0, nrow = length(unique(m4$MMP_id)), ncol = 2) #creating a m4frame to hold last crossing row identifier
last.list[,1] <- unique(m4$MMP_id) #inserting the MMP id to the m4set
#going through the last list m4frame, getting the max attempt value (ie. the last crossing attempt)
for(i in 1:nrow(last.list)){last.list[i,2] <- max(m4$Attempt[m4$MMP_id ==
                                                               last.list[i,1]])}
#going thorugh the last list m4frame, in the last observation per migrant,
#insert a one
for(i in 1:nrow(last.list)){m4$last_cross[last.list[i,1] == m4$MMP_id &
                                            m4$Attempt == last.list[i,2]] <- 1      }
table(m4$last_cross) #table the last cross variable
table(m4$Cross_Year, m4$Coyote) #cross tabling crossing year and coyote usage (1 = yes, 0 = no)

#-----------------------------------------------------------------------------#
# Creating An Variable for Crossing Enforcement Era
#-----------------------------------------------------------------------------#
m4$era <- 0 #UNDOCUMENTED ERA (post-Bracero) (1965-1985)
m4$era[m4$Cross_Year > 1985 & m4$Cross_Year < 1994] <- 1 #IRCA ERA (1986-1994)
m4$era[m4$Cross_Year > 1993] <- 2 #Post-Prevention through Deterrence ERA (Post 1995)

#-----------------------------------------------------------------------------#
# Creating the Previous Coyote Use Varible and the Number of Times Used a Coyote in the Past
#-----------------------------------------------------------------------------#
m4$Pre_Coy <- 0 #creating a binary dummy variable if the migrant has used a coyote on a prior trip
m4$CoyNo <- 0 #creating a continous dummy variable to hold the number of times a migrant has used a coyote before
m4$X<-1:nrow(m4)
for(i in 1:nrow(m4)) #going through every row in the m4set
{
  # if: it's not the first crossing trip, and the sum of the prior trips for the specific migrant
  # is greater than one (coyote = 1, no = 0), then a migrant has used a coyote before and Pre_Coy = 1
  if(m4$First[i] == 0 & sum(m4$Coyote[m4$MMP_id == m4$MMP_id[i] &
                                      m4$X < m4$X[i]], na.rm = T) > 0){
    m4$Pre_Coy[i] <- 1 #set value to 1
    m4$CoyNo[i] <- sum(m4$Coyote[m4$MMP_id == m4$MMP_id[i] &
                                   m4$X < m4$X[i]], na.rm = T)
  }
  
}
m4$Pre_Coy[m4$First == 1] <- 0 #m4 Check, Making all first undocumented trips value for prior
                               #trips with a coyote to 0

m4$Last_Coy <- 0#creating a binary dummy variable if the migrant has used a coyote on the most recent trip
for(i in 2:nrow(m4)) #the first m4 row is a first cross
{
  j <- i - 1 #the row above i (ie. the prior observations)
  # if: it's not the first crossing trip, and the observation above belows to the same migrant and
  # in the above row, the value is not missing and it equal to 1, then Last_Coy = 1, meaning
  # the respondent did use a coyote on the prior crossing trip
  if(m4$First[i] == 0 & m4$MMP_id[i] == m4$MMP_id[j] & m4$Coyote[j] == 1 &
     !is.na(m4$Coyote[j]))
  {
    m4$Last_Coy[i] <- 1 #Last Coy = 1
  }
  
}
m4$Last_Coy[m4$First == 1] <- 0 #m4 Check, Making all first undocumented trips value for prior
                                #trips with a coyote to 0

#creating a m4frame to hold average cost of migration in that trip
sum(is.na(m4$Cost)) #6623 missing values
cost <- matrix(0, length(unique(m4$Cross_Year)),2) #m4frame with each crossing year as a unique row
m4$Cost[m4$Cost == 9999 | m4$Cost == 8888] <- NA #Setting all missing values in Cost to NA
cost[,1]<- sort(unique(m4$Cross_Year)) #Placing Crossing Years in ascending order
m4$Avg_Cost <- 0 #Creating a new variable to hold average cost of each year in m4set
for(i in 1:nrow(cost)) #for each year in the m4set
{
  #obtaining the mean (average) of coyote cost in each year without NAs
  cost[i,2] <- mean(m4$Cost[m4$Cross_Year == cost[i,1]], na.rm = T)
  #placing the value in each observation that crossed in that year
  m4$Avg_Cost[m4$Cross_Year == cost[i,1]] <-  cost[i,2]
}
print(cost[,2]) #print average cost
### we need to set the dollars to not be nominal but rather in cost 2017 dollars using a cpi index
cost <- as.data.frame(cost)
cost$CPI <- 0
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data") #setting working directory
CPI <- read.csv("CPI_11_16_US.csv", stringsAsFactors = F) #reading in dataframe
#to calculate CPI index, we need to divide the average
for(i in 1:nrow(cost)){
  #we need to calculate the percent increase
  year <- cost$V1[i]
  percent <- CPI$Jan[CPI$Year == 2017] / CPI$Jan[CPI$Year == year]
  cost$CPI[i] <- cost$V2[i] * percent
}
for(i in 1:nrow(cost)) #for each year in the m4set
{
  #placing the value in each observation that crossed in that year
  m4$Avg_Cost[m4$Cross_Year == cost[i,1]] <-  cost$CPI
}
#divide all average cost values by 1000 (Singer and Massey 1998)
m4$Avg_Cost <- m4$Avg_Cost / 1000 #Cost in Thousands

table(m4$Cross_Mode) #tabling crossing mode (0 = alone, 1 = Friends and Family, 2 = Coyote)
m4$Cross_Mode <- 2 - m4$Cross_Mode #recoding Crossing Mode (2 = alone, 1 = Friends and Family, 0 = Coyote)
table(m4$Cross_Mode) #checking coding of variable
m4$Linewatch <- m4$Linewatch / 1000000

#Recoding Place of Crossing to represent the three largest operations
m4$Tijuana <- 0 #An indicator for if the crossing trip occured in the Tijuana Border Sector
m4$Tijuana[m4$Cross_Place  == 0] <- 1

m4$Nogales<- 0#An indicator for if the crossing trip occured in the Nogales Border Sector
m4$Nogales[m4$Cross_Place  == 5 ] <- 1

m4$Juarez <- 0#An indicator for if the crossing trip occured in the Juarez Border Sector
m4$Juarez[m4$Cross_Place  == 2 ] <- 1

m4$California <- ifelse(m4$State == 2, 1, 0) #if state = California, 1
# Coahuila, Chihuuahau, Nuevo Leon, Tamuliapas
m4$Texas <- ifelse(m4$State %in% c(5,8,19,28), 1, 0) #if state = Texas, 1
# Arizona
m4$Arizona <- ifelse(m4$State == 26, 1, 0) #if state = Arizona, 1
# Missing
m4$MISSING <- ifelse(m4$State %in% c(1111,9999), 1,0) #Missing m4
### Rio-Grande Sector
m4$RIO_GRANDE <- ifelse(m4$State == 28, 1, 0) #if crossing was in Tamuliapas == 1, if not 0
### Big-Bend Sector
m4$BIG_BEND <- ifelse(m4$State %in% c(19,8), 1, 0) #if crossing was in NuevoLeon or Chihuuahau, == 1, if not 0
### Del- Rio Sector
m4$DEL_RIO <- ifelse(m4$State == 5, 1, 0) #if crossing was in Coahuila, == 1, if not 0
# Missing
m4$MISSING <- ifelse(m4$State %in% c(1111,9999), 1,0) #Missing m4


################################################################################
 
# Saving Datafiles

################################################################################
# setting working directory
setwd("~/Dropbox/Research_Projects/Coyotaje_2024/Data")
write.csv(m4, "Cleaned_Procesed_MMP_062825.csv") #exporting csv with renamed columns
write.csv(m3, "Cleaned_Procesed_MMP_withallvaribles_062825.csv") #exporting csv with renamed columns
################################################################################
