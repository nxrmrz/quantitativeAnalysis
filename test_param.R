rm(list = ls(all.names = TRUE))

# Packages

# Loading CSV file
prj <- read.csv("prjDetPanel-Jan2011.csv")
# Display summary of all columns
# Numeric will have 5-point summary + Mean
# Category will have counts per case
# Date/Time are being treated as category as it is plain text
summary(prj)

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
summary(prj_rnd_10)

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

