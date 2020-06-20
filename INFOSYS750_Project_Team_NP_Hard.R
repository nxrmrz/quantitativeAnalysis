# Clear all variables in the environment
rm(list = ls(all.names = TRUE))

# Install Packages
# Note any special packages & relevant functions needed here
library("car")
library("ggplot2")
library("sqldf")
library("dplyr")
library("finalfit")
library("MissMech")
library("nlme")


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
summary_lme <- function(lme_model, type, ref_model) {
  sum_model <-summary(lme_model) 
  print(round(sum_model$tTable[, c(1,5)],3))
  print(VarCorr(lme_model))
  print(paste("AIC: ",round(sum_model$AIC,0)))
  print(paste("BIC: ",round(sum_model$BIC,0)))
  print(paste("LogLik: ",round(sum_model$logLik,0)))
  print(paste("Deviance: ",round(-2*sum_model$logLik,0)))
  if(type==1){calc_ICC(lme_model)}
  if(type==2){calc_R_sq_e(lme_model,ref_model)}
  if(type==3){calc_R_sq_n(lme_model,ref_model)}
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
prj_miss <- prj[,c(1,2,4,21,22,23)]
TestMCARNormality(prj_miss) # WARNING: This functions takes a long time to run
# Note: MCAR assumption is rejected, so data could be MAR

# Summary of main DVs and IVs

# Visual exploration of main DVs. Using 5 projects
df_5 <-prj[prj$prjId %in% c(2647, 3085, 3671, 3721, 5378), ]

#watchers over time (y axis is log10 scaled)
w_o_t <- ggplot(df_5, aes(x=Time, y=watchers, color=as.factor(prjId))) + 
  labs(title="Watchers Over Time for 5 Projects") + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() +
  geom_smooth()
w_o_t 

#forks over time (y axis is log10 scaled)
f_o_t <- ggplot(df_5, aes(x=Time, y=forks, color=as.factor(prjId))) + 
  labs(title="Forks Over Time for 5 Projects") + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() +
  geom_smooth()
f_o_t 

#members over time (y axis is log10 scaled)
m_o_t <- ggplot(df_5, aes(x=Time, y=members, color=as.factor(prjId))) + 
  labs(title="Members Over Time for 5 Projects") + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() +
  geom_smooth()
m_o_t 

#commits over time (y axis is log10 scaled)
c_o_t <- ggplot(df_5, aes(x=Time, y=commits, color=as.factor(prjId))) + 
  labs(title="Commits Over Time for 5 Projects") + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  scale_y_continuous(trans="log10") + 
  geom_point() + 
  geom_smooth()
c_o_t 

#have frequency distributions of measures of research question
ggplot(prj, aes(watchers)) + geom_histogram() + scale_x_continuous(trans="log10") + labs(title="Histogram of Watchers")
ggplot(prj, aes(issues)) + geom_histogram() + scale_x_continuous(trans="log10") + labs(title="Histogram of Issues")
ggplot(prj, aes(forks)) + geom_histogram() + scale_x_continuous(trans="log10") + labs(title="Histogram of Forks")
ggplot(prj, aes(commits)) + geom_histogram() + scale_x_continuous(trans="log10") + labs(title="Histogram of Commits")

# Assumption Checks
#https://ademos.people.uic.edu/Chapter18.html#6_assumptions

#define a function that: 1.) models residuals vs observed 2.) models residuals vs fitted 3.) qqnorm 
visual_assumption_checks <- function(model, dv) {
  par(mfrow=c(2,2))
  plot(resid(model), 
       prj[[dv]], 
       main="Observed Values vs Residuals",
       sub="Linearity assumption check",
       xlab="Model Residuals",
       ylab="Observations") #checks linearity
  plot(predict(model),resid(model),
       main="Residuals vs Fitted",
       sub="Homoscedasticity assumption check",
       xlab="Fitted Values",
       ylab="Model Residuals") #checks homoscedasticity
  qqnorm(resid(model)) #normality
  qqline(resid(model))
}

ggplot(prj, aes(x=Time, y=watchers, color=as.factor(prjId))) + geom_point() + geom_smooth() + scale_y_continuous(trans="log10")


#===========INCLUDE BELOW IN ASSUMPTION CHECKS==============

#standardise residuals before plotting & statistical testing
#creating the standardized residual (std epsilon.hat)
resid.std <- resid/sd(resid)
plot(alcohol1$id, resid.std, ylim=c(-3, 3), ylab="std epsilon hat")
abline(h=0)

#edit later on -
random.effects(lme_w_oT)[[1]]
random.effects(lme_w_oT)[[2]]

#further checks
statistical_assumption_checks <- function(model) {
  #classic levene's test - test for homogenous residual variances across projects
  print(leveneTest(resid(model) ~ as.factor(prjId), data=prj))
  #shapiro wilk to test normality of residuals
  print(shapiro.test(resid(model)))
}

#===========INCLUDE ABOVE IN ASSUMPTION CHECKS==============                      
                        
#A-1

lme_w_ug <- lme(watchers ~ Time, data = prj, random=~Time|prjId, method="ML")
visual_assumption_checks(lme_w_ug, "watchers")
statistical_assumption_checks(lme_w_ug)

#doesnt converge
lme_w_dL1 <-lme(watchers ~ Time*dummyLicence1, data = prj, random=~Time|prjId, method="ML")

lme_w_dfL2 <- lme(watchers~ Time*dummyLicence2, data = prj, random=~Time|prjId, method="ML")
visual_assumption_checks(lme_w_dfL2, "watchers")
statistical_assumption_checks(lme_w_dfL2)

lme_w_oT <- lme(watchers ~ Time*OwnerType, data = prj, random=~Time|prjId, method="ML")
visual_assumption_checks(lme_w_oT, "watchers")
statistical_assumption_checks(lme_w_oT)
summary(lme_w_oT)

#B-1

plot(nparLD(issues ~ Time, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(issues ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(issues ~ Time*dummyLicence2, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(issues ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

#B-2

plot(nparLD(forks ~ Time, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(forks ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(forks ~ Time*dummyLicence2, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(forks ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

#C-1

plot(nparLD(commits ~ Time, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(commits ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(commits ~ Time*dummyLicence2, data = prj, subject = "prjId", description = FALSE))

plot(nparLD(commits ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

# Transformations


# Derived Values
# License: Blank vs Non-Blank
prj$dummyLicence1 <- ifelse(prj$Licence=="",0,1)
# License: MIT vs Non-MIT
prj$dummyLicence2 <- ifelse(prj$Licence=="MIT License",1,0)
# Health: Set NA to 0
prj$dummyHealth <- ifelse(is.na(prj$Health),0,prj$Health)


# RQ1
lme_A1m <- lme(watchers~1, prj, random=~1|prjId, method="ML")
lme_A1g1 <- lme(watchers~Time, prj, random=~Time|prjId, method="ML")
lme_A1g1aI <- lme(watchers~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1aG <- lme(watchers~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1aB <- lme(watchers~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1bI <- lme(watchers~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1bG <- lme(watchers~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1bB <- lme(watchers~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1cI <- lme(watchers~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_A1g1cG <- lme(watchers~Time+Time:dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_A1g1cB <- lme(watchers~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_A1g1dI <- lme(watchers~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
lme_A1g1dG <- lme(watchers~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
lme_A1g1dB <- lme(watchers~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_A1g1eI <- lme(watchers~Time+OwnerType, prj, random=~Time|prjId, method="ML")
lme_A1g1eG <- lme(watchers~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML")
lme_A1g1eB <- lme(watchers~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_A1m, 1)
summary_lme(lme_A1g1, 2, lme_A1m)
summary_lme(lme_A1g1aI, 3, lme_A1g1)
summary_lme(lme_A1g1aG, 3, lme_A1g1)
summary_lme(lme_A1g1aB, 3, lme_A1g1)
summary_lme(lme_A1g1bI, 3, lme_A1g1)
summary_lme(lme_A1g1bG, 3, lme_A1g1)
summary_lme(lme_A1g1cI, 3, lme_A1g1)
summary_lme(lme_A1g1cG, 3, lme_A1g1)
summary_lme(lme_A1g1cB, 3, lme_A1g1)
summary_lme(lme_A1g1dI, 3, lme_A1g1)
summary_lme(lme_A1g1dG, 3, lme_A1g1)
summary_lme(lme_A1g1dB, 3, lme_A1g1)
summary_lme(lme_A1g1eI, 3, lme_A1g1)
summary_lme(lme_A1g1eG, 3, lme_A1g1)
summary_lme(lme_A1g1eB, 3, lme_A1g1)
# UMM
# UGM
# Additional Models
# Summary for RQ1

# RQ2
lme_B1m <- lme(issues~1, prj, random=~1|prjId, method="ML")
lme_B1g1 <- lme(issues~Time, prj, random=~Time|prjId, method="ML")
lme_B1g1aI <- lme(issues~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1aG <- lme(issues~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1aB <- lme(issues~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1bI <- lme(issues~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1bG <- lme(issues~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1bB <- lme(issues~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1cI <- lme(issues~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B1g1cG <- lme(issues~Time+Time:dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B1g1cB <- lme(issues~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B1g1dI <- lme(issues~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B1g1dG <- lme(issues~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B1g1dB <- lme(issues~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B1g1eI <- lme(issues~Time+OwnerType, prj, random=~Time|prjId, method="ML")
lme_B1g1eG <- lme(issues~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML")
lme_B1g1eB <- lme(issues~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B1m, 1)
summary_lme(lme_B1g1, 2, lme_B1m)
summary_lme(lme_B1g1aI, 3, lme_B1g1)
summary_lme(lme_B1g1aG, 3, lme_B1g1)
summary_lme(lme_B1g1aB, 3, lme_B1g1)
summary_lme(lme_B1g1bI, 3, lme_B1g1)
summary_lme(lme_B1g1bG, 3, lme_B1g1)
summary_lme(lme_B1g1bB, 3, lme_B1g1)
summary_lme(lme_B1g1cI, 3, lme_B1g1)
summary_lme(lme_B1g1cG, 3, lme_B1g1)
summary_lme(lme_B1g1cB, 3, lme_B1g1)
summary_lme(lme_B1g1dI, 3, lme_B1g1)
summary_lme(lme_B1g1dG, 3, lme_B1g1)
summary_lme(lme_B1g1dB, 3, lme_B1g1)
summary_lme(lme_B1g1eI, 3, lme_B1g1)
summary_lme(lme_B1g1eG, 3, lme_B1g1)
summary_lme(lme_B1g1eB, 3, lme_B1g1)
lme_B2m <- lme(forks~1, prj, random=~1|prjId, method="ML")
lme_B2g1 <- lme(forks~Time, prj, random=~Time|prjId, method="ML")
lme_B2g1aI <- lme(forks~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_B2g1aG <- lme(forks~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_B2g1aB <- lme(forks~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_B2g1bI <- lme(forks~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B2g1bG <- lme(forks~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B2g1bB <- lme(forks~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B2g1cI <- lme(forks~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B2g1cG <- lme(forks~Time+Time:dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B2g1cB <- lme(forks~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_B2g1dI <- lme(forks~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B2g1dG <- lme(forks~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B2g1dB <- lme(forks~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B2g1eI <- lme(forks~Time+OwnerType, prj, random=~Time|prjId, method="ML")
lme_B2g1eG <- lme(forks~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML")
lme_B2g1eB <- lme(forks~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B2m, 1)
summary_lme(lme_B2g1, 2, lme_B2m)
summary_lme(lme_B2g1aI, 3, lme_B2g1)
summary_lme(lme_B2g1aG, 3, lme_B2g1)
summary_lme(lme_B2g1aB, 3, lme_B2g1)
summary_lme(lme_B2g1bI, 3, lme_B2g1)
summary_lme(lme_B2g1bG, 3, lme_B2g1)
summary_lme(lme_B2g1bB, 3, lme_B2g1)
summary_lme(lme_B2g1cI, 3, lme_B2g1)
summary_lme(lme_B2g1cG, 3, lme_B2g1)
summary_lme(lme_B2g1cB, 3, lme_B2g1)
summary_lme(lme_B2g1dI, 3, lme_B2g1)
summary_lme(lme_B2g1dG, 3, lme_B2g1)
summary_lme(lme_B2g1dB, 3, lme_B2g1)
summary_lme(lme_B2g1eI, 3, lme_B2g1)
summary_lme(lme_B2g1eG, 3, lme_B2g1)
summary_lme(lme_B2g1eB, 3, lme_B2g1)
# UMM
# UGM
# Additional Models
# Summary for RQ2

# RQ3
lme_C1m <- lme(commits~1, prj, random=~1|prjId, method="ML")
lme_C1g1 <- lme(commits~Time, prj, random=~Time|prjId, method="ML")
lme_C1g1aI <- lme(commits~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1aG <- lme(commits~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1aB <- lme(commits~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1bI <- lme(commits~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1bG <- lme(commits~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1bB <- lme(commits~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1cI <- lme(commits~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_C1g1cG <- lme(commits~Time+Time:dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_C1g1cB <- lme(commits~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
lme_C1g1dI <- lme(commits~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1dG <- lme(commits~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1dB <- lme(commits~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1eI <- lme(commits~Time+OwnerType, prj, random=~Time|prjId, method="ML")
lme_C1g1eG <- lme(commits~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML")
lme_C1g1eB <- lme(commits~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1m, 1)
summary_lme(lme_C1g1, 2, lme_C1m)
summary_lme(lme_C1g1aI, 3, lme_C1g1)
summary_lme(lme_C1g1aG, 3, lme_C1g1)
summary_lme(lme_C1g1aB, 3, lme_C1g1)
summary_lme(lme_C1g1bI, 3, lme_C1g1)
summary_lme(lme_C1g1bG, 3, lme_C1g1)
summary_lme(lme_C1g1bB, 3, lme_C1g1)
summary_lme(lme_C1g1cI, 3, lme_C1g1)
summary_lme(lme_C1g1cG, 3, lme_C1g1)
summary_lme(lme_C1g1cB, 3, lme_C1g1)
summary_lme(lme_C1g1dI, 3, lme_C1g1)
summary_lme(lme_C1g1dG, 3, lme_C1g1)
summary_lme(lme_C1g1dB, 3, lme_C1g1)
summary_lme(lme_C1g1eI, 3, lme_C1g1)
summary_lme(lme_C1g1eG, 3, lme_C1g1)
summary_lme(lme_C1g1eB, 3, lme_C1g1)
# UMM
# UGM
# Additional Models
# Summary for RQ3

# Any visualizations 
#Time Invariant IVs - Health, Licence, Owner Type
#health
h_o_t <- ggplot(df_5, aes(x=Time, y=Health, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  geom_point() +
  geom_smooth()
h_o_t

#license type
l_o_t <- ggplot(df_5, aes(x=Time, y=Licence, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  geom_point() +
  geom_smooth()
l_o_t

#owner type
ot_o_t <- ggplot(df_5, aes(x=Time, y=OwnerType, color=as.factor(prjId))) + 
  scale_color_manual(labels = c("1", "2", "3", "4", "5"), values=c("blue", "red", "pink", "green", "yellow")) +
  geom_point() +
  geom_smooth()
ot_o_t

#RELATIONSHIPS BETWEEN VARIABLES
#watchers over time, influenced by TIME INVARIANT VARIABLES health, licence, owner type (SPECTATOR INTEREST)
w_withHealth_ot <-ggplot(df_5, aes(x=Time, y=watchers, color=Health)) + 
  scale_y_continuous(trans="log10") + 
  labs(title="Watchers Over Time Influenced By Health") + 
  geom_point() + 
  geom_smooth()
w_withHealth_ot

w_withLicence_ot <-ggplot(df_5, aes(x=Time, y=watchers, color=Licence)) + 
  labs(title="Watchers Over Time Influenced By Licence") + 
  geom_point() + 
  geom_smooth()
w_withLicence_ot

w_withOwnerType_ot <-ggplot(prj, aes(x=Time, y=watchers, color=OwnerType)) + 
  labs(title="Watchers Over Time Influenced By Owner Type") + 
  geom_point() + 
  geom_smooth()
w_withOwnerType_ot

#issues over time, influenced by IME INVARIANT VARIABLES, health, owner type, licence (ADOPTER INTEREST)
(ggplot(df_5, aes(x=Time, y=issues, color=Health)) + 
  labs(title="Issues Over Time Influenced By Health") + 
  geom_point() + 
  geom_smooth())

(ggplot(df_5, aes(x=Time, y=issues, color=OwnerType)) + 
    labs(title="Issues Over Time Influenced By Owner Type") + 
    geom_point() + 
    geom_smooth())

(ggplot(df_5, aes(x=Time, y=issues, color=Licence)) + 
    labs(title="Issues Over Time Influenced By Licence") + 
    geom_point() + 
    geom_smooth())

#pull requests over time, nfluenced by TIME INVARIANT VARIABLES health, owner type, licence (PROJECT ACTIVITY)
(ggplot(df_5, aes(x=Time, y=pullReq, color=Health)) + 
    labs(title="Pull Requests Over Time Influenced By Health") + 
    geom_point() + 
    geom_smooth())

(ggplot(df_5, aes(x=Time, y=pullReq, color=Licence)) + 
    labs(title="Pull Requests Over Time Influenced By Licence") + 
    geom_point() + 
    geom_smooth())

(ggplot(df_5, aes(x=Time, y=pullReq, color=OwnerType)) + 
    labs(title="Pull Requests Over Time Influenced By OwnerType") + 
    geom_point() + 
    geom_smooth())



# END