
#######################################################
##        Economic valuation of urban gardens        ##
##                                                   ##
##                                                   ##
##            DATA PREP (INTERACTIONS)               ##
##                   27.10.2018                      ##
#######################################################


rm(list=ls())


setwd("~/Desktop/R Working ")
.libPaths()
pack <- c("gmnl","mlogit")
lapply(pack, require, character.only=T)


load(file="Preppeddata.ND.RData")


###/////////////////////////////////###
###/   FORMING INTERACTION TERMS   /###
###/////////////////////////////////###


# standardizing age, income, to 0 is mean
{
  agemean <- mean(fd$AGE, na.rm=TRUE)
  agemean
  incomemean <- mean(fd$INCOME, na.rm=TRUE)
  incomemean
}


##### FACILITIES and AGE #####
{
  fd$D.FACILITIES.2xAGE <- fd$D.FACILITIES.2 * fd$AGE
  fd$D.FACILITIES.3xAGE <- fd$D.FACILITIES.3 * fd$AGE
  # and for linear model...
  fd$FACILITIESxAGE <- fd$FACILITIES * fd$AGE
}

##### ENGAGEMENT and AGE #####
{
  fd$D.ENGAGEMENT.2xAGE <- fd$D.ENGAGEMENT.2 * fd$AGE
  fd$D.ENGAGEMENT.3xAGE <- fd$D.ENGAGEMENT.3 * fd$AGE
  # and for linear model...
  fd$ENGAGEMENTxAGE <- fd$ENGAGEMENT * fd$AGE
}

###### STYLE and INCOME #####
{
  fd$D.STYLE.2xINCOME <- fd$D.STYLE.2 * fd$INCOME
  fd$D.STYLE.1xINCOME <- fd$D.STYLE.1 * fd$INCOME
  # and for linear model...
  fd$D.STYLExINCOME <- fd$STYLE * fd$INCOME
}




# saving environment as RData
save.image(file = "DataPrep2.RData")




