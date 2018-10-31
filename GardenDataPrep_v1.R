##################################################################
##             Economic Valution of Urban Gardens               ##                                
##                        DATA PREPARATION                      ##
##                                                              ##
##################################################################


rm(list=ls())


setwd("~/Desktop/R Working ")
.libPaths()
pack<-c("evd", "mlogit")
lapply(pack, require, character.only=T)


# read in SurveyEngine English data
expdata.eg <- read.csv("englishexp_final.csv")

# read in SurveyEngine German data
expdata.de <- read.csv("germanexp_final.csv")

covdata.all <- read.csv("final_german_and_english_combined_survey_covariates - data.csv")

    
# filter the covariate data to the relevant columns of information 
##ALL COV DATA - Updated

covdata.all <- covdata.all[,c("RID","GREENSAT","URBGAR","PRINZ","VISIT","AGE","GENDER","EMPLOY","INCOME","GERMAN","INTEREST")]


#merging the two datasets together
merged.de <- merge(expdata.de, covdata.all, by = "RID", all=TRUE)
merged.eg <- merge(expdata.eg, covdata.all, by = "RID", all=TRUE)

# combine German and English datasets as one 
# order by design row
fulldata <- rbind.data.frame(merged.de, merged.eg)

fulldata <- fulldata[order(fulldata$RID, fulldata$DESIGN_ROW),]

# remove missing pref1 respondents and all RIDs that did not finish all 9 choice sets
fulldata <- fulldata[which(fulldata$pref1!='is.NA'),]
fulldata <- fulldata[fulldata$RID %in% names(which(table(fulldata$RID) == 9 )),]

# write csv file for export to Excel
write.csv(fulldata, file="fulldata.csv", quote=FALSE, na="NA", row.names=TRUE)


#rename alternatives
alt <-1:3

for (n in alt) {
  print(n)
  print(alt[[n]])
  print(paste0("test", n,"test"))
  print(paste0("a",n,"_x1"))
  colnames(fulldata)[which(colnames(fulldata[1,]) == paste0("a",n,"_x1"))] <- paste0("FACILITIES.",n)
  colnames(fulldata)[which(colnames(fulldata[1,]) == paste0("a",n,"_x2"))] <- paste0("ENGAGEMENT.",n)
  colnames(fulldata)[which(colnames(fulldata[1,]) == paste0("a",n,"_x3"))] <- paste0("STYLE.",n)
  colnames(fulldata)[which(colnames(fulldata[1,]) == paste0("a",n,"_x4"))] <- paste0("COST.",n)}

fd <- mlogit.data(fulldata,  shape = "wide", id ="RID",  varying = 6:17, choice = "pref1")

mlogit.data(fulldata,  shape = "wide", id ="RID",  varying = 6:17, choice = "pref1")


#Create dummy variables and ASC

fd$ASC<- as.numeric(fd$alt == 3)

fd$D.FACILITIES.2<- as.numeric(fd$FACILITIES == 2)
fd$D.FACILITIES.3<- as.numeric(fd$FACILITIES == 3)

fd$D.ENGAGEMENT.2<- as.numeric(fd$ENGAGEMENT == 2)
fd$D.ENGAGEMENT.3<- as.numeric(fd$ENGAGEMENT == 3)

fd$D.STYLE.2<- as.numeric(fd$STYLE == 2)
fd$D.STYLE.1<- as.numeric(fd$STYLE == 1)


# recoding the price attribute (which has been divided by 10)
{
  fd$COST[fd$COST==1] <- 0.5
  fd$COST[fd$COST==2] <- 1
  fd$COST[fd$COST==3] <- 2
  fd$COST[fd$COST==4] <- 5
  fd$COST[fd$COST==5] <- 10
  fd$COST[fd$COST==6] <- 15
  fd$COST[fd$alt==3] <- 0
}


# kicking out not-chosen alternatives
logitprep.trues <- fd[which(fd$pref1==TRUE),]

# standardizing age, income, to 0 is mean
{
  agemean <- mean(fd$AGE, na.rm=TRUE)
  agemean
  incomemean <- mean(fd$INCOME, na.rm=TRUE)
  incomemean
}



save.image(file = "Preppeddata.ND.RData")




