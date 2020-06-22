# Clear all variables in the environment
rm(list = ls(all.names = TRUE))

# Load relevant packages
library(car)
library(ggplot2)
library(sqldf)
library(dplyr)
library(finalfit)
library(MissMech)
library(nlme)
library(lattice)
library(corrplot)
library(nparLD)
library(nortest)

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

# Function for sample plots on random data
prj_plots <- function(data, type) {
  ymin<- min(data)-0.1*(max(data)-min(data))
  ymax<- max(data)+0.1*(max(data)-min(data))
  if (type==0){
    xyplot(data~Time|prjId, data=prj_rnd_10, layout = c(5,2))
  }
  if (type==1){
    xyplot(data~Time|prjId, data=prj_rnd_10,
           panel=function(x,y){
             panel.xyplot(x, y)
             panel.loess(x,y, family="gaussian")
           },ylim=c(ymin,ymax), layout = c(5,2))
  }
  if (type==2){
    xyplot(data~Time|prjId, data=prj_rnd_10,
           panel=function(x,y){
             panel.xyplot(x, y)
             panel.lmline(x,y)
           },ylim=c(ymin,ymax), layout = c(5,2))
  }
}

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
#further checks
statistical_assumption_checks <- function(model) {
  #classic levene's test - test for homogenous residual variances across projects
  print(leveneTest(resid(model) ~ as.factor(prjId), data=prj))
  #shapiro wilk to test normality of residuals
  print(shapiro.test(resid(model)))
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

# List of Licences
sqldf("select Licence, count(1)
       from (
         select prjId, Licence, count(1)
         from prj
         group by prjId, Licence)
       group by Licence
       order by 2 desc")
# List of Health
sqldf("select Health, count(1)
       from (
         select prjId, Health, count(1)
         from prj
         group by prjId, Health)
       group by Health
       order by 2 desc")

# Derived Values
# License: Blank vs Non-Blank
prj$dummyLicence1 <- ifelse(prj$Licence=="",0,1)
# Health: Set NA to 0
prj$dummyHealth <- ifelse(is.na(prj$Health),0,prj$Health)

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
  missing_compare("PRClosedTime", c("prjId", "Time", "dummyHealth", "dummyLicence1", "OwnerType"))
prj %>%
  missing_compare("IssueClosedTime", c("prjId", "Time", "dummyHealth", "dummyLicence1", "OwnerType"))
prj %>%
  missing_compare("Health", c("prjId", "Time", "dummyLicence1", "OwnerType"))
# Note: All cases show significant impact of explanatory variables on missing variables

#TestMCARNormality(prj_miss) # WARNING: This functions takes a long time to run
# Note: MCAR assumption is rejected, so data could be MAR

# extract 10 random cases (80 records)
prj_rnd_10<-prj[prj$prjId %in% sample(unique(prj$prjId),10), ]
# Display 4 DVs against time for 10 selected projects
prj_plots(prj_rnd_10$watchers,2)
prj_plots(prj_rnd_10$issues,2)
prj_plots(prj_rnd_10$forks,2)
prj_plots(prj_rnd_10$commits,2)

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

# the next step takes a long time to run
ggplot(prj, aes(x=Time, y=watchers, color=as.factor(prjId))) + geom_point() + geom_smooth() + scale_y_continuous(trans="log10")

# A-1
lme_w_ug <- lme(watchers ~ Time, data = prj, random=~Time|prjId, method="ML")
visual_assumption_checks(lme_w_ug, "watchers")
statistical_assumption_checks(lme_w_ug)

# doesnt converge
lme_w_dL1 <-lme(watchers ~ Time*dummyLicence1, data = prj, random=~Time|prjId, method="ML")

lme_w_oT <- lme(watchers ~ Time*OwnerType, data = prj, random=~Time|prjId, method="ML")
visual_assumption_checks(lme_w_oT, "watchers")
statistical_assumption_checks(lme_w_oT)
summary(lme_w_oT)

par(mfrow=c(1,1))

#B-1
plot(nparLD(issues ~ Time, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(issues ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(issues ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

#B-2
plot(nparLD(forks ~ Time, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(forks ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(forks ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

#C-1
plot(nparLD(commits ~ Time, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(commits ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(commits ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))

# RQ1
lme_A1m <- lme(watchers~1, prj, random=~1|prjId, method="ML")
lme_A1g1 <- lme(watchers~Time, prj, random=~Time|prjId, method="ML")
lme_A1g1aI <- lme(watchers~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1aG <- lme(watchers~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1aB <- lme(watchers~ Time*Licence, prj, random=~Time|prjId, method="ML")
lme_A1g1bI <- lme(watchers~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1bG <- lme(watchers~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1bB <- lme(watchers~Time*dummyLicence1, prj, random=~Time|prjId, method="ML") # doesn't converge
lme_A1g1dI <- lme(watchers~Time+dummyHealth, prj, random=~Time|prjId, method="ML") # doesn't converge
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
summary_lme(lme_A1g1dG, 3, lme_A1g1)
summary_lme(lme_A1g1dB, 3, lme_A1g1)
summary_lme(lme_A1g1eI, 3, lme_A1g1)
summary_lme(lme_A1g1eG, 3, lme_A1g1)
summary_lme(lme_A1g1eB, 3, lme_A1g1)

# RQ2
lme_B1m <- lme(issues~1, prj, random=~1|prjId, method="ML")
lme_B1g1 <- lme(issues~Time, prj, random=~Time|prjId, method="ML")
lme_B1g1aI <- lme(issues~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1aG <- lme(issues~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1aB <- lme(issues~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_B1g1bI <- lme(issues~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1bG <- lme(issues~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1bB <- lme(issues~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
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
summary_lme(lme_B2g1dI, 3, lme_B2g1)
summary_lme(lme_B2g1dG, 3, lme_B2g1)
summary_lme(lme_B2g1dB, 3, lme_B2g1)
summary_lme(lme_B2g1eI, 3, lme_B2g1)
summary_lme(lme_B2g1eG, 3, lme_B2g1)
summary_lme(lme_B2g1eB, 3, lme_B2g1)

# RQ3
lme_C1m <- lme(commits~1, prj, random=~1|prjId, method="ML")
lme_C1g1 <- lme(commits~Time, prj, random=~Time|prjId, method="ML")
lme_C1g1aI <- lme(commits~Time+Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1aG <- lme(commits~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1aB <- lme(commits~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1bI <- lme(commits~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1bG <- lme(commits~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1bB <- lme(commits~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1dI <- lme(commits~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1dG <- lme(commits~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1dB <- lme(commits~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1eI <- lme(commits~Time+OwnerType, prj, random=~Time|prjId, method="ML")
lme_C1g1eG <- lme(commits~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML") # doesn't converge
lme_C1g1eB <- lme(commits~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1m, 1)
summary_lme(lme_C1g1, 2, lme_C1m)
summary_lme(lme_C1g1aI, 3, lme_C1g1)
summary_lme(lme_C1g1aG, 3, lme_C1g1)
summary_lme(lme_C1g1aB, 3, lme_C1g1)
summary_lme(lme_C1g1bI, 3, lme_C1g1)
summary_lme(lme_C1g1bG, 3, lme_C1g1)
summary_lme(lme_C1g1bB, 3, lme_C1g1)
summary_lme(lme_C1g1dI, 3, lme_C1g1)
summary_lme(lme_C1g1dG, 3, lme_C1g1)
summary_lme(lme_C1g1dB, 3, lme_C1g1)
summary_lme(lme_C1g1eI, 3, lme_C1g1)
summary_lme(lme_C1g1eB, 3, lme_C1g1)

# Transformations
prj$watchers <- log(prj$watchers+1)
prj$issues <- sqrt(sqrt(prj$issues))
prj$forks <- log(prj$forks+1)
prj$commits <- log(prj$commits+1)

# Spectator Interest (A): Watchers (1)
lme_A1m <- lme(watchers~1, prj, random=~1|prjId, method="ML")
lme_A1g1 <- lme(watchers~Time, prj, random=~Time|prjId, method="ML")
lme_A1g1aB <- lme(watchers~Time*Licence, prj, random=~Time|prjId, method="ML") # doesn't converge
lme_A1g1bB <- lme(watchers~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_A1g1dB <- lme(watchers~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_A1g1eB <- lme(watchers~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_A1m, 1)
summary_lme(lme_A1g1, 2, lme_A1m)
summary_lme(lme_A1g1bB, 3, lme_A1g1)
summary_lme(lme_A1g1dB, 3, lme_A1g1)
summary_lme(lme_A1g1eB, 3, lme_A1g1)
lme_A1g1bGdI <- lme(watchers~Time+Time:dummyLicence1+dummyHealth, prj,
                    random=~Time|prjId, method="ML")
summary_lme(lme_A1g1bGdI, 3, lme_A1g1)
# Adopter Interest (B): Issues (1)
lme_B1m <- lme(issues~1, prj, random=~1|prjId, method="ML")
lme_B1g1 <- lme(issues~Time, prj, random=~Time|prjId, method="ML")
lme_B1g1aB <- lme(issues~Time*Licence, prj, random=~Time|prjId, method="ML") # doesn't converge
lme_B1g1bB <- lme(issues~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B1g1dB <- lme(issues~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B1g1eB <- lme(issues~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B1m, 1)
summary_lme(lme_B1g1, 2, lme_B1m)
summary_lme(lme_B1g1bB, 3, lme_B1g1)
summary_lme(lme_B1g1dB, 3, lme_B1g1)
summary_lme(lme_B1g1eB, 3, lme_B1g1)
lme_B1g1bGdI <- lme(issues~Time+Time:dummyLicence1+dummyHealth, prj,
                    random=~Time|prjId, method="ML")
summary_lme(lme_B1g1bGdI, 3, lme_B1g1)
# Adopter Interest (B): Forks (2)
lme_B2m <- lme(forks~1, prj, random=~1|prjId, method="ML")
lme_B2g1 <- lme(forks~Time, prj, random=~Time|prjId, method="ML")
lme_B2g1aB <- lme(forks~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_B2g1bB <- lme(forks~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_B2g1dB <- lme(forks~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_B2g1eB <- lme(forks~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B2m, 1)
summary_lme(lme_B2g1, 2, lme_B2m)
summary_lme(lme_B2g1aB, 3, lme_B2g1)
summary_lme(lme_B2g1bB, 3, lme_B2g1)
summary_lme(lme_B2g1dB, 3, lme_B2g1)
summary_lme(lme_B2g1eB, 3, lme_B2g1)
lme_B2g1bGdI <- lme(forks~Time+Time:dummyLicence1+dummyHealth, prj,
                    random=~Time|prjId, method="ML")
summary_lme(lme_B2g1bGdI, 3, lme_B2g1)
# Project Activity (C): Commits (1)
lme_C1m <- lme(commits~1, prj, random=~1|prjId, method="ML")
lme_C1g1 <- lme(commits~Time, prj, random=~Time|prjId, method="ML")
lme_C1g1aB <- lme(commits~Time*Licence, prj, random=~Time|prjId, method="ML")
lme_C1g1bB <- lme(commits~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
lme_C1g1dB <- lme(commits~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
lme_C1g1eB <- lme(commits~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1m, 1)
summary_lme(lme_C1g1, 2, lme_C1m)
summary_lme(lme_C1g1aB, 3, lme_C1g1)
summary_lme(lme_C1g1bB, 3, lme_C1g1)
summary_lme(lme_C1g1dB, 3, lme_C1g1)
summary_lme(lme_C1g1eB, 3, lme_C1g1)
lme_C1g1dBeI <- lme(commits~Time+dummyHealth+Time:dummyHealth+OwnerType, prj,
                    random=~Time|prjId, method="ML")
summary_lme(lme_C1g1dBeI, 3, lme_C1g1)# No Improvement

# Visualization Plots - Transformed

fixef <- fixef(lme_A1g1bGdI)
fit_L0_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
fit_L0_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 5), type="b", pch=0,
     ylab="Predicted Values: ln(watchers+1)", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="left", c("No Licence, Health=25", "Licence, Health=25", 
                   "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_B1g1bGdI)
fit_L0_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
fit_L0_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 2.5), type="b", pch=0,
     ylab="Predicted Values: sqrt(sqrt(issues))", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="topleft", c("No Licence, Health=25", "Licence, Health=25", 
                      "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_B2g1bGdI)
fit_L0_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H25 <- fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
fit_L0_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]]
fit_L1_H75 <- fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]])
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 2.5), type="b", pch=0,
     ylab="Predicted Values: ln(forks+1)", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="topleft", c("No Licence, Health=25", "Licence, Health=25", 
                      "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_C1g1dBeI)
fit_OwnO_H25 <- fixef[[1]]+25*fixef[[3]]+prj$Time[1:8]*(fixef[[2]]+25*fixef[[5]])
fit_OwnU_H25 <- fixef[[1]]+25*fixef[[3]]+fixef[[4]]+prj$Time[1:8]*(fixef[[2]]+25*fixef[[5]])
fit_OwnO_H75 <- fixef[[1]]+75*fixef[[3]]+prj$Time[1:8]*(fixef[[2]]+75*fixef[[5]])
fit_OwnU_H75 <- fixef[[1]]+75*fixef[[3]]+fixef[[4]]+prj$Time[1:8]*(fixef[[2]]+75*fixef[[5]])
plot(prj$Time[1:8], fit_OwnO_H25, ylim=c(0, 7), type="b", pch=0,
     ylab="Predicted Values: ln(commits+1)", xlab="Time")  
lines(prj$Time[1:8], fit_OwnU_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_OwnO_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_OwnU_H75, type="b", pch=16) 
title("Final Model: controlled effects of Owner") 
legend(x="topleft", c("Owner=Org, Health=25", "Owner=User, Health=25", 
                      "Owner=Org, Health=75", "Owner=User, Health=75"),
       pch=c(0,15,1,16))

# Visualization Plots - Converted

fixef <- fixef(lme_A1g1bGdI)
fit_L0_H25 <- exp(fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]])-1
fit_L1_H25 <- exp(fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))-1
fit_L0_H75 <- exp(fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]])-1
fit_L1_H75 <- exp(fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))-1
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 80), type="b", pch=0,
     ylab="Predicted Values: watchers", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="topleft", c("No Licence, Health=25", "Licence, Health=25", 
                      "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_B1g1bGdI)
fit_L0_H25 <- (fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]])^4
fit_L1_H25 <- (fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))^4
fit_L0_H75 <- (fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]])^4
fit_L1_H75 <- (fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))^4
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 40), type="b", pch=0,
     ylab="Predicted Values: issues", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="topleft", c("No Licence, Health=25", "Licence, Health=25", 
                      "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_B2g1bGdI)
fit_L0_H25 <- exp(fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*fixef[[2]])-1
fit_L1_H25 <- exp(fixef[[1]] + 25*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))-1
fit_L0_H75 <- exp(fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*fixef[[2]])-1
fit_L1_H75 <- exp(fixef[[1]] + 75*fixef[[3]] + prj$Time[1:8]*(fixef[[2]]+fixef[[4]]))-1
plot(prj$Time[1:8], fit_L0_H25, ylim=c(0, 10), type="b", pch=0,
     ylab="Predicted Values: forks", xlab="Time")  
lines(prj$Time[1:8], fit_L1_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_L0_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_L1_H75, type="b", pch=16) 
title("Final Model: controlled effects of Licence") 
legend(x="topleft", c("No Licence, Health=25", "Licence, Health=25", 
                      "No Licence, Health=75", "Licence, Health=75"),
       pch=c(0,15,1,16))

fixef <- fixef(lme_C1g1dBeI)
fit_OwnO_H25 <- exp(fixef[[1]]+25*fixef[[3]]+prj$Time[1:8]*(fixef[[2]]+25*fixef[[5]]))-1
fit_OwnU_H25 <- exp(fixef[[1]]+25*fixef[[3]]+fixef[[4]]+prj$Time[1:8]*(fixef[[2]]+25*fixef[[5]]))-1
fit_OwnO_H75 <- exp(fixef[[1]]+75*fixef[[3]]+prj$Time[1:8]*(fixef[[2]]+75*fixef[[5]]))-1
fit_OwnU_H75 <- exp(fixef[[1]]+75*fixef[[3]]+fixef[[4]]+prj$Time[1:8]*(fixef[[2]]+75*fixef[[5]]))-1
plot(prj$Time[1:8], fit_OwnO_H25, ylim=c(0, 1100), type="b", pch=0,
     ylab="Predicted Values: commits", xlab="Time")  
lines(prj$Time[1:8], fit_OwnU_H25, type="b", pch=15)   
lines(prj$Time[1:8], fit_OwnO_H75, type="b", pch=1)   
lines(prj$Time[1:8], fit_OwnU_H75, type="b", pch=16) 
title("Final Model: controlled effects of Owner") 
legend(x="topleft", c("Owner=Org, Health=25", "Owner=User, Health=25", 
                      "Owner=Org, Health=75", "Owner=User, Health=75"),
       pch=c(0,15,1,16))


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
w_withOwnerType_ot # fails computation

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

# forks over time, nfluenced by TIME INVARIANT VARIABLES health, owner type, licence (PROJECT ACTIVITY)
(ggplot(df_5, aes(x=Time, y=forks, color=Health)) + 
    labs(title="Pull Requests Over Time Influenced By Health") + 
    geom_point() + 
    geom_smooth())

(ggplot(df_5, aes(x=Time, y=forks, color=Licence)) + 
    labs(title="Pull Requests Over Time Influenced By Licence") + 
    geom_point() + 
    geom_smooth())

(ggplot(df_5, aes(x=Time, y=forks, color=OwnerType)) + 
    labs(title="Pull Requests Over Time Influenced By OwnerType") + 
    geom_point() + 
    geom_smooth())

# END
