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


hist(prj$watchers)
hist(log(prj$watchers))
hist(log(log(prj$watchers)))
hist(sqrt(prj$watchers))
hist(sqrt(sqrt(prj$watchers)))

# License: Blank vs Non-Blank
prj$dummyLicence1 <- ifelse(prj$Licence=="",0,1)
# License: MIT vs Non-MIT
prj$dummyLicence2 <- ifelse(prj$Licence=="MIT License",1,0)
# Health: Set NA to 0
prj$dummyHealth <- ifelse(is.na(prj$Health),0,prj$Health)

# Correlations
str(prj)
prj_noF <- prj[,-which(sapply(prj, class) == "factor")]
round(cor(prj_noF),2)
round(cor(prj_noF[,-c(18,19,20)]),2)
library(corrplot)
corrplot(cor(prj_noF[,-c(18,19,20)]))

install.packages("corrr", quiet=TRUE)
library(corrr)
prj_noF %>% correlate() %>% focus(watchers)

library(nlme) # lme
# Check ICC for DVs of interest
summary_lme(lme(watchers~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(OwnerFollower~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(AvgFollower~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(totalFollowers~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(issues~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(forks~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(nonMemCommitters~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(nonMemCommittersRatio~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(pullReq~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(commits~1, prj, random=~1|prjId, method="ML"), 1)
summary_lme(lme(IssueClosedCnt~1, prj, random=~1|prjId, method="ML"), 1)

hist(prj$watchers)
hist(log(prj$watchers+1))
summary(prj$watchers)
summary(prj$members)
summary(log(prj$watchers+1))
# Total Followers
prj$logwatchers <- log(prj$watchers+1)

summary(prj)

summary_lme(lme(log(watchers+1)~1, prj, random=~1|prjId, method="ML"), 1)

head(prj[prj$commits==0,],20)


# Spectator Interest (A): Watchers (1), OwnerFollower (2), AvgFollower (3)
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
lme_A1g1cIdB <- lme(watchers~Time+dummyLicence2+Time*dummyHealth, prj,
                    random=~Time|prjId, method="ML")
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
summary_lme(lme_A1g1cIdB, 3, lme_A1g1)
# Corr: Watchers
sort(round(cor(prj_noF[,-c(18,19,20)]),2)[,8])
# Corr for watchers: forks, issues, commits
lme_A1g2 <- lme(watchers~forks, prj, random=~forks|prjId, method="ML")
lme_A1g3 <- lme(watchers~issues, prj, random=~issues|prjId, method="ML")
lme_A1g4 <- lme(watchers~commits, prj, random=~commits|prjId, method="ML")
summary_lme(lme_A1g2, 2, lme_A1m)
summary_lme(lme_A1g3, 2, lme_A1m)
summary_lme(lme_A1g4, 2, lme_A1m)

lme_A2m <- lme(OwnerFollower~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_A2m, 1)
lme_A2g1 <- lme(OwnerFollower~Time, prj, random=~Time|prjId, method="ML")
# Corr: OwnerFollower
sort(round(cor(prj_noF[,-c(18,19,20)]),2)[,19])
lme_A2g1 <- lme(OwnerFollower~dummyLicence2, prj, random=~1|prjId, method="ML")
summary_lme(lme_A2g1, 2, lme_A2m)


lme_A3m <- lme(AvgFollower~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_A3m, 1)
lme_A3g1 <- lme(AvgFollower~OwnerFollower, prj, random=~1|prjId, method="ML")
summary_lme(lme_A3g1, 2, lme_A2m)
# Total Followers
prj$totalFollowers <- prj$members*prj$AvgFollower
lme_A4m <- lme(totalFollowers~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_A4m, 1)
lme_A4g1 <- lme(totalFollowers~Time, prj, random=~1|prjId, method="ML")
summary_lme(lme_A4g1, 2, lme_A4m)
lme_A4g2 <- lme(totalFollowers~commits, prj, random=~1|prjId, method="ML")
summary_lme(lme_A4g2, 2, lme_A4m)



# Commiters who are not Members
prj$nonMemCommitters <- prj$committers-prj$MemCommitters
prj$nonMemCommittersRatio <- ifelse(prj$committers==0,0,(prj$nonMemCommitters/prj$committers))
# Adopter Interest (B): Issues (1), Forks (2), nonMemberCommitterRatio (3)
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
lme_B1g1bGdB <- lme(issues~Time+Time:dummyLicence1+Time*dummyHealth, prj,
                    random=~Time|prjId, method="ML")
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
summary_lme(lme_B1g1bGdB, 3, lme_B1g1) # No improvement

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
lme_B2g1bBdB <- lme(forks~Time*dummyLicence1+Time*dummyHealth, prj,
                    random=~Time|prjId, method="ML")
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
summary_lme(lme_B2g1bBdB, 3, lme_B2g1) # No improvement


lme_B3m <- lme(nonMemCommitters~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_B3m, 1)
lme_B3g1 <- lme(nonMemCommitters~Time, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B3g1, 2, lme_B3m)
lme_B4m <- lme(nonMemCommittersRatio~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_B4m, 1)
lme_B4g1 <- lme(nonMemCommittersRatio~Time, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1, 2, lme_B4m)
lme_B4g1aB <- lme(nonMemCommittersRatio~Time*Licence, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1aB, 3, lme_B4g1)
lme_B4g1bB <- lme(nonMemCommittersRatio~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1bB, 3, lme_B4g1)
lme_B4g1cB <- lme(nonMemCommittersRatio~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1cB, 3, lme_B4g1)
lme_B4g1dB <- lme(nonMemCommittersRatio~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1dB, 3, lme_B4g1)
lme_B4g1eB <- lme(nonMemCommittersRatio~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_B4g1eB, 3, lme_B4g1)

# Project Activity (C): Commits (1), IssueClosedCount (2), PullReq (3)
lme_C1m <- lme(commits~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_C1m, 1)

plot(lme_C1m)
plot(lme_C1g1)
plot(lme_C1g1dB)

boxplot(watchers ~ dummyLicence2, data = prj)
boxplot(watchers ~ Time, data = prj)
boxplot(watchers ~ dummyLicence2*Time, data = prj)

library(nparLD)
#A-1
plot(nparLD(watchers ~ Time, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(watchers ~ Time*dummyLicence1, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(watchers ~ Time*dummyLicence2, data = prj, subject = "prjId", description = FALSE))
plot(nparLD(watchers ~ Time*OwnerType, data = prj, subject = "prjId", description = FALSE))
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

prj$dummyCommits <- log(prj$commits+1)
plot(nparLD(dummyCommits ~ dummyLicence1*Time, data = prj,
            subject = "prjId", description = FALSE))

head(residuals(lme_C1m))
head(random.effects(lme_C1m)[[1]])
head(residuals(lme_C1g1))
head(random.effects(lme_C1g1))
head(random.effects(lme_C1g1)[[1]])
head(random.effects(lme_C1g1)[[2]])
head(residuals(lme_C1g1bB))
head(random.effects(lme_C1g1bB))
head(random.effects(lme_C1g1bB)[[1]])
head(random.effects(lme_C1g1bB)[[2]])

lme_C1g1 <- lme(commits~Time, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1, 2, lme_C1m)
lme_C1g1aB <- lme(commits~Time*Licence, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1aB, 3, lme_C1g1)
lme_C1g1aI <- lme(commits~Time+Licence, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1aI, 3, lme_C1g1)
lme_C1g1aG <- lme(commits~Time+Time:Licence, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1aG, 3, lme_C1g1)
lme_C1g1bB <- lme(commits~Time*dummyLicence1, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1bB, 3, lme_C1g1)
lme_C1g1bI <- lme(commits~Time+dummyLicence1, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1bI, 3, lme_C1g1)
lme_C1g1bG <- lme(commits~Time+Time:dummyLicence1, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1bG, 3, lme_C1g1)
lme_C1g1cB <- lme(commits~Time*dummyLicence2, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1cB, 3, lme_C1g1)
lme_C1g1cI <- lme(commits~Time+dummyLicence2, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1cI, 3, lme_C1g1)
lme_C1g1cG <- lme(commits~Time+Time:dummyLicence2, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1cG, 3, lme_C1g1)
lme_C1g1dB <- lme(commits~Time*dummyHealth, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1dB, 3, lme_C1g1)
lme_C1g1dI <- lme(commits~Time+dummyHealth, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1dI, 3, lme_C1g1)
lme_C1g1dG <- lme(commits~Time+Time:dummyHealth, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1dG, 3, lme_C1g1)
lme_C1g1eB <- lme(commits~Time*OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1eB, 3, lme_C1g1)
lme_C1g1eI <- lme(commits~Time+OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1eI, 3, lme_C1g1)
lme_C1g1eG <- lme(commits~Time+Time:OwnerType, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C1g1eG, 3, lme_C1g1)
lme_C1g1bBdB <- lme(commits~Time*dummyLicence1+Time*dummyHealth, prj,
                    random=~Time|prjId, method="ML")
summary_lme(lme_C1g1bBdB, 3, lme_C1g1)

lme_C2m <- lme(IssueClosedCnt~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_C2m, 1)
lme_C2g1 <- lme(IssueClosedCnt~Time, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C2g1, 2, lme_C2m)
lme_C2g2 <- lme(IssueClosedCnt~issues, prj, random=~issues|prjId, method="ML")
summary_lme(lme_C2g2, 2, lme_C2m)
lme_C2g3 <- lme(IssueClosedCnt~commits, prj, random=~commits|prjId, method="ML")
summary_lme(lme_C2g3, 2, lme_C2m)
lme_C2g4 <- lme(IssueClosedCnt~members, prj, random=~members|prjId, method="ML")
summary_lme(lme_C2g4, 2, lme_C2m)
lme_C2g4 <- lme(IssueClosedCnt~MemCommitters, prj, random=~MemCommitters|prjId, method="ML")
summary_lme(lme_C2g4, 2, lme_C2m)



Plot.Model.F.Linearity<-plot(resid(lme_C1g1),prj$commits)

Plot.Model.F.Linearity<-plot(resid(lme_A1m),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_B1m),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_B2m),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_C1m),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_A1g1),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_B1g1),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_B2g1),prj$commits)
Plot.Model.F.Linearity<-plot(resid(lme_C1g1),prj$commits)


head(resid(lme_C1g1))


lme_C3m <- lme(pullReq~1, prj, random=~1|prjId, method="ML")
summary_lme(lme_C3m, 1)
lme_C3g1 <- lme(pullReq~Time, prj, random=~Time|prjId, method="ML")
summary_lme(lme_C3g1, 2, lme_C3m)
lme_C3g2 <- lme(pullReq~issues, prj, random=~issues|prjId, method="ML")
summary_lme(lme_C3g2, 2, lme_C3m)


dim(lme_A1m$residuals)
dim(prj$obsCode)
plot(lme_A1m$residuals, prj$obsCode)
head(lme_A1m$residuals)
