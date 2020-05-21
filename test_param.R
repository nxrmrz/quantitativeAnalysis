rm(list = ls(all.names = TRUE))

# Packages
library(lattice) # xyplot 

# Loading CSV file
prj <- read.csv("prjDetPanel-Jan2011.csv")
# Display summary of all columns
# Numeric will have 5-point summary + Mean
# Category will have counts per case
# Date/Time are being treated as category as it is plain text
summary(prj)
# End of Job

# display sample records
head(prj)

# extract 8 cases (64 records)
prj_8 <- prj[17:80,]
summary(prj_8$members)
summary(prj_8)

# plot
plot(members~Time, data=prj_8)
xyplot(members~Time|prjId, data=prj_8)

ymin<- min(prj_8$members)
ymax<- max(prj_8$members)

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

