rm(list = ls(all.names = TRUE))

# Install Packages
# Note any special packages & relevant functions needed here

# Loading CSV file
prj <- read.csv("prjDetPanel-Jan2011.csv")

# Add sequential observation code
total_rows <- nrow(prj)
prj$obsCode <- 1:total_rows
# Add sequential project code
prj$prjCode <- (prj$obsCode-1)%/%8+1
summary(prj)

# Find missing data


