# Clear all variables in the environment
rm(list = ls(all.names = TRUE))

# Install Packages
# Note any special packages & relevant functions needed here

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
# Note: X. is not continuous - does not end at 2680, prjId is not continuous

# Check number of instances in each wave
aggregate(X ~ Time, data = prj, FUN = length)
# Compute number of waves in each instance
prj_wave <- aggregate(Time ~ prjId, data = prj, FUN = length)
# Check possible number of instances
aggregate(prjId ~ Time, data = prj_wave, FUN = length)
# Note: we have 8 waves for each project, so it is a balanced dataset

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

library(dplyr)
library(finalfit)
prj %>%
  missing_compare("PRClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
  missing_compare("IssueClosedTime", c("prjId", "Time", "Health", "Licence", "OwnerType"))
prj %>%
  missing_compare("Health", c("prjId", "Time", "Licence", "OwnerType"))
# Note: All cases show significant impact of explanatory variables on missing variables

library(MissMech)
prj_miss <- prj[,c(1,2,4,21,22,23)]
TestMCARNormality(prj_miss)
# Note: MCAR assumption is rejected, so data could be MAR



# Summary of main DVs and IVs

# Visual exploration of main DVs

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