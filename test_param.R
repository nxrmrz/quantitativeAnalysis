# Loading CSV file
prjDetPanel_Jan2011 <- read.csv("prjDetPanel-Jan2011.csv")
# Display summary of all columns
# Numeric will have 5-point summary + Mean
# Category will have counts per case
# Date/Time are being treated as category as it is plain text
summary(prjDetPanel_Jan2011)
# End of Job