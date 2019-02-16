##################################################################
##              Economic valuation of urban gardens             ##
##                                                              ##
##            LOGISTIC REGRESSION  With Interactions            ##
##                           27.10.2018                         ##
##################################################################

rm(list=ls())


setwd("~/Desktop/R Working ")
.libPaths()
pack<-c("evd", "mlogit", "gmnl")
lapply(pack, require, character.only=T)


load(file="DataPrep2.RData")


###/////////////////////////////////////////////###
###/CONDITIONAL LOGIT MODEL WITH INTERACTIONS  /###
###/////////////////////////////////////////////###

##### UTILITY FUNCTION #####

attach(fd)

V.mnl.i <- (pref1~ASC
       + D.FACILITIES.2 + D.FACILITIES.3
       + D.FACILITIES.2xAGE + D.FACILITIES.3xAGE
       + D.ENGAGEMENT.2 + D.ENGAGEMENT.3
       + D.ENGAGEMENT.2xAGE + D.ENGAGEMENT.3xAGE
       + D.STYLE.2 + D.STYLE.1
       + D.STYLE.2xINCOME + D.STYLE.1xINCOME
       + COST
       | 0)
V.mnl.i

detach()


##### REGRESSION #####


mnl.i<- gmnl(V.mnl.i, data=fd, model="mnl")

summary(mnl.i)
getSummary.gmnl(mnl.i)




##### WTP CALCULATION - COND LOGIT. WITH INTERACTIONS #####

WTP.MNL.I <- wtp.gmnl(mnl.i, wrt="COST")

print(-WTP.MNL.I[,1])

print(-WTP.MNL.I[,1]*10)



