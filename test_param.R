rm(list = ls(all.names = TRUE))

# Packages

# Loading CSV file
prj <- read.csv("prjDetPanel-Jan2011.csv")
# Display summary of all columns
# Numeric will have 5-point summary + Mean
# Category will have counts per case
# Date/Time are being treated as category as it is plain text
summary(prj)

# License summary
summary(prj$Licence)

# display sample records
head(prj)
summary(prj)

# Add sequential observation code
total_rows <- nrow(prj)
prj$obsCode <- 1:total_rows
# Add sequential project code
prj$prjCode <- (prj$obsCode-1)%/%8+1
summary(prj)

# Run PCA on dataset
head(prj[,c(4,7:23,26,27)])
prcomp(prj[,c(4,7:20,26:27)])
pca <- prcomp(prj[,c(4,7:20,26:27)])
pca
summary(pca)
prcomp(prj[,c(4,7:20,26,27)], scale.=TRUE)
pca_scale <- prcomp(prj[,c(4,7:20)], scale.=TRUE)
pca_scale
summary(pca_scale)

# extract 10 cases (80 records)
prj_top_10 <- prj[1:80,]
summary(prj_top_10)

# plot
plot(members~Time, data=prj_top_10)
library(lattice) # xyplot 
xyplot(Health~Time|prjId, data=prj_top_10, layout = c(5,2))

# extract 10 random cases (80 records)
prj_rnd_10<-prj[prj$prjCode %in% sample(1:(total_rows/8),10), ]
prj_rnd_10<-prj[prj$prjId %in% sample(unique(prj$prjId),10), ]
summary(prj_rnd_10)

sample(unique(prj$prjId),10)

# plot
plot(members~Time, data=prj_rnd_10)
library(lattice) # xyplot 
xyplot(Health~Time|prjId, data=prj_rnd_10, layout = c(5,2))

xyplot(X.~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(prjId~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(Period~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(Time~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(StartDate~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(EndDate~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(forks~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(members~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(commits~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(issues~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(watchers~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(issues~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(pullReq~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(CmtCmnt~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(pullReqCmnt~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
#xyplot(PR.Issue.Cmnt|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(issueCmnt~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(committers~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(MemCommitters~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(PRClosedCnt~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(IssueClosedCnt~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(Health~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(Licence~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(ContribFile~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(OwnerFollower~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(AvgFollower~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(OwnerType~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(obsCode~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))
xyplot(prjCode~Time|prjId, data=prj[prj$prjCode %in% sample(1:(total_rows/8),10), ], layout = c(5,2))

# Check all columns for within-person variance
library(nlme) # lme
lme(X.~1, prj_top_10, random=~1 |X.)
summary(lme(X.~1, prj_top_10, random=~1 |X., method="ML"))
VarCorr(lme(X.~1, prj_top_10, random=~1 |X., method="ML"))
summary(lme(Health~1, prj, random=~1 |prjId, method="ML"))
VarCorr(lme(forks~Time, prj, random=~Time |X., method="ML"))

colnames(prj)

summary(prj$prjId)
col <- "prjId"
summary(prj$col)
summary(prj[col])
summary(prj[[col]])

for (col in colnames(prj)){
        print(col)
        print(summary(prj[[col]]))
        print(lme(X.~col, prj, random=~1 |X.))
}

for (col in colnames(prj)){
        print(col)
        summary(prj$col)
        if (col!="X."){
                #summary(lme(X.~col, prj, random=~1 |X.))
                summary(prj$col)
        }
}

summary(lme(X.~prjId, prj, random=~1 |X.))

hist(prj$Health)
table(prj$Health)
summary(prj$Health)

ymin<- min(prj_8$members)-0.1*(max(prj_8$members)-min(prj_8$members))
ymax<- max(prj_8$members)+0.1*(max(prj_8$members)-min(prj_8$members))

xyplot(members~Time|prjId, data=prj_8,
       panel=function(x,y){
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian")
       },ylim=c(ymin,ymax))

xyplot(members~Time|prjId, data=prj_8,
       panel=function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x,y)
       },ylim=c(ymin,ymax))

interaction.plot(prj_8$Time, prj_8$prjId, prj_8$members) 

# box plot
boxplot(members ~ OwnerType, data = prj_8)
boxplot(members ~ Time, data = prj_8)
boxplot(members ~ OwnerType*Time, data = prj_8)
boxplot(members ~ OwnerType*Time, data = prj_8,
        names = FALSE, col = c("red", "blue"))

boxplot(members ~ Time, data = prj)
boxplot(log(members+1) ~ Time, data = prj)
boxplot(log(log(members+1)+1) ~ Time, data = prj)
boxplot(log(log(log(members+1)+1)+1) ~ Time, data = prj)
boxplot(log(log(log(log(members+1)+1)+1)+1) ~ Time, data = prj)






# mmm package - exploration
#install.packages("mmm", quiet=TRUE)
library(mmm)
data("motherStress")
head(motherStress,15)
ms <- motherStress[,c(1,14,2,3,4,5)]
head(ms,25)

# extract 10 cases (120 records)
ms_top_10 <- ms[1:120,]
summary(ms_top_10)

# plot
plot(stress~week, data=ms_top_10)
plot(illness~week, data=ms_top_10)
library(lattice) # xyplot 
xyplot(stress~week|id, data=ms_top_10, layout = c(5,2))
xyplot(illness~week|id, data=ms_top_10, layout = c(5,2))

# Evaluate growth over time
aggregate(ms_top_10$stress, list(ms_top_10$week), mean)
aggregate(ms_top_10$illness, list(ms_top_10$week), mean)

# Unconditional Means Model
library(nlme) # lme
lme(stress~1, ms, random=~1 |id)
lme(stress~1, ms, random=~1 |id, method="ML")
lme_a <- lme(stress~1, ms, random=~1 |id, method="ML")
summary(lme_a)
VarCorr(lme_a)
lme(illness~1, ms, random=~1 |id)
lme(illness~1, ms, random=~1 |id, method="ML")
lme_b <- lme(illness~1, ms, random=~1 |id, method="ML")
summary(lme_b)
VarCorr(lme_b)

# Unconditional Growth Model
lme(stress~week, ms, random=~week |id)
lme(stress~week, ms, random=~week |id, method="ML")
lme_c <- lme(stress~week, ms, random=~week |id, method="ML")
summary(lme_c)
VarCorr(lme_c)
lme(illness~week, ms, random=~week |id)
lme(illness~week, ms, random=~week |id, method="ML")
lme_d <- lme(illness~week, ms, random=~week |id, method="ML")
summary(lme_d)
VarCorr(lme_d)

# Combined
lme(cbind(stress,illness)~1, ms, random=~1 |id)
lme(cbind(stress,illness)~1, ms, random=~1 |id, method="ML")
lme_ab <- lme(cbind(stress,illness)~1, ms, random=~1 |id, method="ML")
summary(lme_ab)
VarCorr(lme_ab)
# FAIL - same as a

lme(stress~week, ms, random=~week |id)
lme(illness~week, ms, random=~week |id)
mmm(formula=cbind(stress,illness)~week, id=ms$id, data=ms)
fit_cd <- mmm(formula=cbind(stress,illness)~week, id=ms$id, data=ms)
summary(fit_cd)
fit_cd$coefficients

# NOT SUITABLE

#install.packages("plm")
library(plm)
?plm
summary(plm(members ~ Time, data = prj, index = "prjId"))
summary(plm(members~1, data = prj, index = "prjId", model="pooling"))

summary(plm(members ~ Time, data = prj, index = "prjId", model="within"))
summary(plm(members ~ Time, data = prj, index = "prjId", model="random"))

summary(plm(commits ~ Time, data = prj, index = "prjId"))
summary(plm(commits ~ members, data = prj, index = "prjId"))
summary(plm(commits ~ Time*members, data = prj, index = "prjId"))

round(cor(prj[, c(1,4,7:20)]),2)

# Check time variant vs invariant columns
install.packages("sqldf", quiet=TRUE)
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
col <- 'X.'
query <- paste("SELECT prjCode, ", col, ", COUNT(1)",
               " FROM prj",
               " GROUP BY prjCode, ", col,
               " HAVING COUNT(1) < 8",
               sep="")
dim(sqldf(query))

# Missing Data Tests
library(dplyr)
install.packages("MissMech", quiet=TRUE)
library(MissMech)
TestMCARNormality(prj[,c(1,2,4,21,22,23)])

explanatory=c("Time","prjId")
dependent="PRClosedTime"
prj %>%
        select(explanatory) %>%
        TestMCARNormality()

# missing data
dim(prj)
dim(prj[!complete.cases(prj),])
dim(prj[complete.cases(prj),])
sum(is.na(prj))
table(complete.cases(prj))
table(is.na(prj))

sapply(prj, function(x) sum(is.na(x)))

sum(rowSums(is.na(prj))!=0)
sum(colSums(is.na(prj))!=0)

colSums(is.na(prj))!=0

library(dplyr)
install.packages("finalfit", quiet=TRUE)
library(finalfit)
prj %>%
        missing_compare("PRClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
        missing_compare("IssueClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
        missing_compare("Health", c("prjId", "Time", "Licence", "OwnerType"))

install.packages("naniar", quiet=TRUE)
# Scatter Plot
library(ggplot2)
ggplot(data = prj,
       aes(x = obsCode,
           y = Health)) +
        geom_point()
ggplot(data = prj,
       aes(x = obsCode,
           y = PRClosedTime)) +
        geom_point()
ggplot(data = prj,
       aes(x = obsCode,
           y = IssueClosedTime)) +
        geom_point()
# Missing
library(naniar)
ggplot(data = prj,
       aes(x = obsCode,
           y = Health)) +
        geom_miss_point()
ggplot(data = prj,
       aes(x = obsCode,
           y = PRClosedTime)) +
        geom_miss_point()
ggplot(data = prj,
       aes(x = obsCode,
           y = IssueClosedTime)) +
        geom_miss_point()
# By Owner
ggplot(data = prj,
       aes(x = obsCode,
           y = Health)) +
        geom_miss_point() +
        facet_wrap(~OwnerType)
ggplot(data = prj,
       aes(x = obsCode,
           y = PRClosedTime)) +
        geom_miss_point() +
        facet_wrap(~OwnerType)
ggplot(data = prj,
       aes(x = obsCode,
           y = IssueClosedTime)) +
        geom_miss_point() +
        facet_wrap(~OwnerType)

prj_miss <- prj[,c(1,2,4,21,22,23,29)]
nabular(prj_miss)
prj_miss %>%
        nabular() %>%
        ggplot(aes(x = obsCode,
                   fill = Health_NA)) + 
        geom_density(alpha = 0.5)
prj_miss %>%
        nabular() %>%
        ggplot(aes(x = obsCode,
                   fill = PRClosedTime_NA)) + 
        geom_density(alpha = 0.5)
prj_miss %>%
        nabular() %>%
        ggplot(aes(x = obsCode,
                   fill = IssueClosedTime_NA)) + 
        geom_density(alpha = 0.5)

# License: Blank vs Non-Blank
prj$dummyLicence1 <- ifelse(prj$Licence=="",0,1)

summary(lme(members~Time, prj, random=~Time|prjId))
summary(lme(commits~Time, prj, random=~Time|prjId))

install.packages("joineRML", quiet=TRUE)
library(joineRML)

data(heart.valve)
hvd <- heart.valve[!is.na(heart.valve$log.grad) & !is.na(heart.valve$log.lvmi), ]
fit2 <- mjoint(
        formLongFixed = list("grad" = log.grad ~ time + sex + hs,
                             "lvmi" = log.lvmi ~ time + sex),
        formLongRandom = list("grad" = ~ 1 | num,
                              "lvmi" = ~ time | num),
        formSurv = Surv(fuyrs, status) ~ age,
        data = list(hvd, hvd),
        inits = list("gamma" = c(0.11, 1.51, 0.80)),
        timeVar = "time",
        verbose = TRUE)
logLik(fit2)
summary(fit2)

prj_joint <- prj[!is.na(prj$members) & !is.na(prj$commits), ]
mjoint(
        formLongFixed = list("01"=members~Time,
                             "02"=commits~Time),
        formLongRandom = list("01"=~Time|prjId,
                              "02"=~Time|prjId),
        data=list(prj_joint, prj_joint),
        timeVar="Time"
)
# NOT SUITABLE

summary(lme(members~Time, prj, random=~Time|prjId))
summary(lme(commits~Time, prj, random=~Time|prjId))
mmm(formula=cbind(members,commits)~Time, id=prj$prjId, data=prj)
summary(mmm(formula=cbind(members,commits)~Time, id=prj$prjId, data=prj))
summary(mmm(formula=cbind(members,commits,issues)~Time+OwnerType, id=prj$prjId, data=prj))
?mmm

summary(prj$committers)
summary(prj$MemCommitters)
summary(prj$committers-prj$MemCommitters)

# Commiters who are not Members
prj$nonMemCommitters <- prj$committers-prj$MemCommitters
prj$nonMemCommittersRatio <- ifelse(prj$committers==0,0,(prj$nonMemCommitters/prj$committers))

head(prj[,-c(3,5,6,24,25,28)])
round(cor(prj[,-c(3,5,6,24,25,28)]),2)

# Functions for Model Evaluation
calc_ICC <- function(lme_umm) {
        var_between <- as.numeric(VarCorr(lme_umm)[1][1]) 
        var_within <- as.numeric(VarCorr(lme_a)[2][1])
        ICC <- var_between/(var_between+var_within)
        print(paste("ICC: ",ICC))
        return(TRUE)
}
calc_R_sq_e <- function(lme_ugm, lme_umm) {
        ugm_params <- dim(VarCorr(lme_ugm))[1]
        var_within_ugm <- as.numeric(VarCorr(lme_ugm)[ugm_params][1]) 
        var_within_umm <- as.numeric(VarCorr(lme_umm)[2][1]) 
        R_sq_e <- 1-(var_within_ugm/var_within_umm)
        print(paste("R_sq_e: ",R_sq_e))
        aic_ugm <- summary(lme_ugm)$AIC
        aic_umm <- summary(lme_umm)$AIC
        aic_chg <- aic_ugm-aic_umm
        print(paste("AIC Change: ",aic_chg))
        return(TRUE)
}
calc_R_sq_n <- function(lme_new, lme_ugm) {
        new_params <- dim(VarCorr(lme_new))[1]
        ugm_params <- dim(VarCorr(lme_ugm))[1]
        if(new_params==ugm_params){
                for(i in 1:(ugm_params-1)) {
                        var_between_new <- as.numeric(VarCorr(lme_new)[i])
                        var_between_ugm <- as.numeric(VarCorr(lme_ugm)[i])
                        R_sq_n <- 1-(var_between_new/var_between_ugm)
                        print(paste("R_sq_",i-1,": ",R_sq_n))
                }
                aic_new <- summary(lme_new)$AIC
                aic_ugm <- summary(lme_ugm)$AIC
                aic_chg <- aic_new-aic_ugm
                print(paste("AIC Change: ",aic_chg))
                return(TRUE)
        } else{
                print("Models have different number of parameters")
                return(FALSE)
        }
}

hist(prj$watchers)
hist(log(prj$watchers))
hist(log(log(prj$watchers)))
hist(sqrt(prj$watchers))
hist(sqrt(sqrt(prj$watchers)))


# Spectator Interest: Watchers, OwnerFollower, AvgFollower
lme_A1m <- lme(watchers~1, prj, random=~1|prjId, method="ML")
summary(lme_A1m)
calc_ICC(lme_A1m)
dim(lme_A1m$residuals)
dim(prj$obsCode)
plot(lme_A1m$residuals, prj$obsCode)
head(lme_A1m$residuals)

lme_A1g1 <- lme(watchers~Time, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1)
calc_R_sq_e(lme_A1g1, lme_A1m)
lme_A1g1a <- lme(watchers~Time+Licence, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1a)
calc_R_sq_n(lme_A1g1a, lme_A1g1)
lme_A1g1b <- lme(watchers~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1b)
calc_R_sq_n(lme_A1g1b, lme_A1g1)
lme_A1g1c <- lme(watchers~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1c)
calc_R_sq_n(lme_A1g1c, lme_A1g1)
lme_A1g1d <- lme(watchers~Time+OwnerType, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1d)
calc_R_sq_n(lme_A1g1d, lme_A1g1)
lme_A1g1e <- lme(watchers~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
# Does not converge
lme_A1g1f <- lme(watchers~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1f)
calc_R_sq_n(lme_A1g1f, lme_A1g1)
lme_A1g1g <- lme(watchers~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1g)
calc_R_sq_n(lme_A1g1g, lme_A1g1)
lme_A1g1h <- lme(watchers~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
summary(lme_A1g1h)
calc_R_sq_n(lme_A1g1h, lme_A1g1)
lme_A1g2 <- lme(watchers~issues, prj, random=~issues|prjId, method="ML")
summary(lme_A1g2)
calc_R_sq_e(lme_A1g2, lme_A1m)
calc_R_sq_n(lme_A1g2, lme_A1g1)
lme_A1g3 <- lme(watchers~Time+issues, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3)
calc_R_sq_e(lme_A1g3, lme_A1m)
lme_A1g3a <- lme(watchers~Time+issues+Licence, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3a)
calc_R_sq_n(lme_A1g3a, lme_A1g3)
lme_A1g3b <- lme(watchers~Time+issues+dummyLicence1, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3b)
calc_R_sq_n(lme_A1g3b, lme_A1g3)
lme_A1g3c <- lme(watchers~Time+issues+dummyLicence2, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3c)
calc_R_sq_n(lme_A1g3c, lme_A1g3)
lme_A1g3d <- lme(watchers~Time+issues+OwnerType, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3d)
calc_R_sq_n(lme_A1g3d, lme_A1g3)
lme_A1g3e <- lme(watchers~Time+issues+dummyHealth, prj, random=~Time+issues|prjId, method="ML")
summary(lme_A1g3e)
calc_R_sq_n(lme_A1g3e, lme_A1g3)
# Does not converge


summary()
summary()
summary(lme(watchers~Time+issues, prj, random=~Time|prjId, method="ML"))
summary(lme(watchers~Time, prj, random=~Time+issues|prjId, method="ML"))
summary(lme(watchers~Time+issues, prj, random=~Time+issues|prjId, method="ML"))
summary(lme(watchers~Time+Licence, prj, random=~Time|prjId, method="ML"))
summary()

prj$dummyLicence2 <- ifelse(prj$Licence %in% c("Apache License 2.0", "MIT License"),1,0)
summary(lme(watchers~Time+dummyLicence2, prj, random=~Time|prjId, method="ML"))

prj$dummyHealth <- ifelse(is.na(prj$Health),0,prj$Health)

