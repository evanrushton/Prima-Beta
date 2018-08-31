# Rasch Estimates for Link file (stacked pr and po dataframes)
# Putting pre and post scores on the same scale with linking items
# Itemfit looks good (can be improved in Conquest to account for error)

library("eRm")
library("dplyr")

pr <- read.table("./Data/prsurvey.csv", header=TRUE, sep=",", as.is = TRUE)
po <- read.table("./Data/posurvey.csv", header=TRUE, sep=",", as.is = TRUE)

# Remove non-item and non-id cols
pr <- pr[c(-1, -2, -4, -5)]
po <- po[c(-2, -3)]

# Reorder cols so linking items are first with same names (3, 12, 16, 21, 34, 40)
pr <- pr[,c(1,4,10,13,16,22,25,2,3,5:9,11,12,14,15,17:21,23,24)]
po <- po[,c(1,2,5,7,10,18,22,3,4,6,8,9,11:17,19:21,23:25)]
names(pr)[names(pr) == "X.3.MD.UR.pr..r"] <- "X.3.MD.UR_L.r"
names(pr)[names(pr) == "X.1.MD.UR_L.r"] <- "X.1.MD.UR.pr.r"

# Fill in non-overlapping columns with NAs
pr[setdiff(names(po), names(pr))] <- NA
po[setdiff(names(pr), names(po))] <- NA

# Select kids with both pr/po
pr1 <- filter(pr, uid %in% po$uid)
po1 <- filter(po, uid %in% pr$uid)
link2 <- rbind(pr1, po1) #stack dataframes vertically
uids <- link2[c(1)]
link <- link2[c(-1)]

#Rasch
res.rasch <- RM(link)
#Person Parameters
pres.rasch <- person.parameter(res.rasch)

#Andersen’s LR-test for goodness-of-fit with mean split criterion
lrres.rasch <- LRtest(res.rasch, splitcr = "mean")
lrres.rasch
plotGOF(lrres.rasch, beta.subset = c(14, 5, 15, 3, 1), tlab = "item", # 14, 5, 18, 7, 1
        conf = list(ia = FALSE, col = "green", lty = "dotted"))
#Wright Map
plotPImap(res.rasch, sorted = TRUE)

#Item Fit
#For easy description of the fit statistics see: http://www.rasch.org/rmt/rmt82a.htm
itemfit(pres.rasch)



# (NOT RUN) Alternative ability estimates difference 
personparams <- as.data.frame(pres.rasch[["theta.table"]][["Person Parameter"]])
personparams <- cbind(uids, personparams) # has order been maintained?
names(personparams)[c(2)] <- "theta"
prperson <- personparams[1:343,]
names(prperson)[c(2)] <- "thetapr"
poperson <- personparams[344:686,]
names(poperson)[c(2)] <- "thetapo"

persons <- merge(prperson, poperson, by="uid")
persons <- cbind(persons, diff=persons$thetapo - persons$thetapr)

hist(persons$thetapr, col=5, breaks=20)
hist(persons$thetapo, col=5, breaks=20)
hist(persons$diff, col=5, breaks=20)

plot(persons$thetapr, persons$thetapo)

