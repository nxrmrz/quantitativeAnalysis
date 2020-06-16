# Clear all variables in the environment
rm(list = ls(all.names = TRUE))

# Install Packages
# Note any special packages & relevant functions needed here

# Functions for Model Evaluation
calc_ICC <- function(lme_umm) {
  var_between <- as.numeric(VarCorr(lme_umm)[1][1]) 
  var_within <- as.numeric(VarCorr(lme_umm)[2][1])
  ICC <- var_between/(var_between+var_within)
  print(paste("ICC: ",round(ICC,3)))
}
calc_R_sq_e <- function(lme_ugm, lme_umm) {
  ugm_params <- dim(VarCorr(lme_ugm))[1]
  var_within_ugm <- as.numeric(VarCorr(lme_ugm)[ugm_params][1]) 
  var_within_umm <- as.numeric(VarCorr(lme_umm)[2][1]) 
  R_sq_e <- 1-(var_within_ugm/var_within_umm)
  print(paste("R_sq_e: ",round(R_sq_e,3)))
  aic_ugm <- summary(lme_ugm)$AIC
  aic_umm <- summary(lme_umm)$AIC
  aic_chg <- -(aic_ugm-aic_umm)
  new_eval <- ifelse(aic_chg>=10,"Very Strong",
                     ifelse(aic_chg>=6,"Strong",
                            ifelse(aic_chg>=2,"Positive",
                                   ifelse(aic_chg>=0,"Weak","BAD"))))
  print(paste("AIC Reduction: ",round(aic_chg,3), "Evaluation: ",new_eval))
}
calc_R_sq_n <- function(lme_new, lme_ugm) {
  new_params <- dim(VarCorr(lme_new))[1]
  ugm_params <- dim(VarCorr(lme_ugm))[1]
  if(new_params==ugm_params){
    for(i in 1:(ugm_params-1)) {
      var_between_new <- as.numeric(VarCorr(lme_new)[i])
      var_between_ugm <- as.numeric(VarCorr(lme_ugm)[i])
      R_sq_n <- 1-(var_between_new/var_between_ugm)
      print(paste("R_sq_",i-1,": ",round(R_sq_n,3)))
    }
    aic_new <- summary(lme_new)$AIC
    aic_ugm <- summary(lme_ugm)$AIC
    aic_chg <- -(aic_new-aic_ugm)
    new_eval <- ifelse(aic_chg>=10,"Very Strong",
                       ifelse(aic_chg>=6,"Strong",
                              ifelse(aic_chg>=2,"Positive",
                                     ifelse(aic_chg>=0,"Weak","BAD"))))
    print(paste("AIC Reduction: ",round(aic_chg,3), "Evaluation: ",new_eval))
  } else{
    print("Models have different number of parameters")
  }
}

# Load CSV file
prj <- read.csv("prjDetPanel-Jan2011.csv")

# Display partial set of records
head(prj)
# Display dimensions of dataset
dim(prj)
# Display columns names of dataset
colnames(prj)
# 2 columns with '.' noted - rename to avoid issues later
names(prj)[names(prj)=="X."] <- "X"
names(prj)[names(prj)=="PR.Issue.Cmnt"] <- "PRIssueCmnt"
# Display summary of each column
summary(prj)
# Note: X is not continuous - does not end at 2680, prjId is not continuous
# 3 columns (PRClosedTime, IssueClosedTime, Health) show NA
# 2 columns (Licence, ContribFile) show BLANK

# Check number of instances in each wave
aggregate(X ~ Time, data = prj, FUN = length)
# Compute number of waves in each instance
prj_wave <- aggregate(Time ~ prjId, data = prj, FUN = length)
# Check possible number of instances
aggregate(prjId ~ Time, data = prj_wave, FUN = length)
# Note: we have 8 identical waves for each project, so it is a balanced dataset

# Add sequential observation code - assuming data is sequential: project-time
prj$obsCode <- 1:nrow(prj)
# Add sequential project code - considering 8 waves per project
prj$prjCode <- (prj$obsCode-1)%/%8+1
# Display summary of each column
summary(prj)
# Note: obsCode is continuous & prjCode is continuous

# Identify time-invariant columns
library(sqldf)
for (col in colnames(prj)) {
  query <- paste("SELECT prjCode, ", col, ", COUNT(1)",
                 " FROM prj",
                 " GROUP BY prjCode, ", col,
                 " HAVING COUNT(1) < 8",
                 sep="")
  if (dim(sqldf(query))[1]==0){
    print(paste(col, " is time-invariant."))
  }
}
# Note: Health, Licence, ContribFile & OwnerType are main time-invariant columns

# Find missing data
table(complete.cases(prj))
table(is.na(prj))
sum(rowSums(is.na(prj))!=0)
sum(colSums(is.na(prj))!=0)
colSums(is.na(prj))!=0
# Note: 3 columns have missing data (NA) - PRClosedTime, IssueClosedTime & Health
# Identify patterns of missing data
library(dplyr)
library(finalfit)
prj %>%
  missing_pattern("prjId", c("PRClosedTime", "IssueClosedTime", "Health"))
prj %>%
  missing_compare("PRClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
  missing_compare("IssueClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
  missing_compare("Health", c("prjId", "Time", "Licence", "OwnerType"))
# Note: All cases show significant impact of explanatory variables on missing variables
# Check MCAR assumption
library(MissMech)
prj_miss <- prj[,c(1,2,4,21,22,23)]
TestMCARNormality(prj_miss)
# Note: MCAR assumption is rejected, so data could be MAR

# Summary of main DVs and IVs

# Visual exploration of main DVs
df_5 <-prj[prj$prjId %in% c(2647, 3085, 3671, 3721, 5378), ]

library("ggplot2")

#watchers over time
w_o_t <- ggplot(df_5, aes(x=Time, y=watchers, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  geom_point() +
  geom_smooth()
w_o_t 

#forks over time (y axis is log10 scaled)
f_o_t <- ggplot(df_5, aes(x=Time, y=forks, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() +
  geom_smooth()
f_o_t 

#members over time (y axis is log10 scaled)
m_o_t <- ggplot(df_5, aes(x=Time, y=members, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() +
  geom_smooth()
m_o_t 

#commits over time (y axis is log10 scaled)
c_o_t <- ggplot(df_5, aes(x=Time, y=commits, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() + 
  geom_smooth()
c_o_t 

# Assumption Checks

# Transformations

# RQ1
# UMM
# UGM
# Additional Models
# Summary for RQ1

# RQ2
# UMM
# UGM
# Additional Models
# Summary for RQ2

# RQ3
# UMM
# UGM
# Additional Models
# Summary for RQ3

# Any visualizations

# END