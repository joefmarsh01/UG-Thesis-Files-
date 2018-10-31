##################################################################
##              Economic valuation of urban gardens             ##
##                                                              ##
##              LOGISTIC REGRESSION  No Interactions            ##
##                          29.08.2018                          ##
##################################################################


rm(list=ls())


setwd("~/Desktop/R Working ")
.libPaths()
pack<-c("evd", "mlogit", "gmnl")
lapply(pack, require, character.only=T)

load(file="DataPrep2.RData")

pack<-c("gmnl", "mlogit", "support.CEs")
lapply(pack, require, character.only=T)


###\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\###
###\       NUll MODEL           \###
###\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\###

V.null <- (pref1~ASC | 0)

mnl.null <- gmnl(V.null, data=fd, model="mnl")
getSummary.gmnl(mnl.null)
LogLik.null <- logLik(mnl.null)


###\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\###
###\  CONDITIONAL LOGIT MODEL   \###
###\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\###

##### UTILITY FUNCTION #####

V.mnl<-pref1~ASC+ D.FACILITIES.2+D.FACILITIES.3+D.ENGAGEMENT.2+D.ENGAGEMENT.3+D.STYLE.2+D.STYLE.1+COST | 0 


##### REGRESSIOM #####

attach(fd)

mnl1 <- gmnl(V.mnl, data=fd, model="mnl")
summary(mnl1)

getSummary.gmnl(mnl1)



### Coefficient of Determination for conditional logit model ###
LogLik.mnl <- logLik(mnl1)
mnl1.R2 <- 1 - (-LogLik.mnl/-LogLik.null)
mnl1.R2



###\\\\\\\\\\\\\\\\\\\\\\\\\\###
###\   LATENT CLASS MODEL   \###
###\\\\\\\\\\\\\\\\\\\\\\\\\\###


##### UTILITY FUNCTION #####

attach(fd)

V.lc2 <- (pref1~ASC
          + D.FACILITIES.2 + D.FACILITIES.3
          + D.ENGAGEMENT.2 + D.ENGAGEMENT.3
          + D.STYLE.2 + D.STYLE.1
          + COST
          | 0 | 0 | 0 | AGE + PRINZ + GERMAN + INCOME + VISIT + EMPLOY)


detach()


##### REGRESSION #####

lcmnl2 <- gmnl(V.lc2, data=fd, model="lc", panel=TRUE, Q=2)
summary(lcmnl2)
getSummary.gmnl(lcmnl2)


shares <- function(obj){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  Q <- length(cons_class)
  shares <- exp(cons_class) / sum(exp(cons_class))
  names(shares) <- paste("share q", 1:Q, sep = "=")  
  return(shares)
}

##shares of class split ##
shares(lcmnl2)

detach()

##NULL LATENT CLASS ####
V.lc.null <- (pref1~ASC
              | 0 | 0 | 0 | AGE + PRINZ + GERMAN + INCOME + VISIT + EMPLOY))



##### WTP CALCULATION - COND LOGIT #####

WTP.mnl.noi <- wtp.gmnl(mnl1, wrt="COST")

##this multiplies it by -1 and then *10  

print(-WTP.mnl.noi[,1])
print(-WTP.mnl.noi[,1]*10)

##### WTP CALCULATION - LATENT CLASS #####

WTP.LC.class1 <- wtp.gmnl(lcmnl2, wrt=c("class.1.COST"))
print(-WTP.LC.class1[1:7,1]*10)
WTP.LC.class2 <- wtp.gmnl(lcmnl2, wrt=c("class.2.COST"))
print(-WTP.LC.class2[9:15,1]*10)



