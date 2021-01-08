### ##########################################
###
### Namibia demersal longline bycatch analysis
### Tim Reid
### March 2018
###
### ##########################################

### cleaned up by Steffen Oppel 31 July 2019
## removed all unnecessary models
## only retained actual analysis and data cleaning/preparation

### MAIN OUTSTANDING ISSUES:
# (1) dataset for pre-regulation data missing
# (2) not clear which is the final model reported in the manuscript
# (3) not clear which is the NULL model compared against the final model (2)


##############################################################
#### load ALL NECESSARY libraries
##############################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(jagsUI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(loo)
library(denstrip)


##############################################################
#### LOAD ALL NECESSARY DATASETS
##############################################################

#### Fisheries observer data

setwd("C:/Users/s435777/Local Files/Documents/Tim/Documents/trawler work BLSA/namibia stephanie")
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia\\Data")
dd <- read_excel("OBSERVER_Data_Final_OBSERVERS AND TRIPS_REVISED OCTOBER 2019.xlsx",   ## was "Copy of Copy of Namibia_OBSERVER_Data_Final_July2018.xlsx", 
                 sheet="LONGLINE_DATA")[,1:19]

# remove blank rows
dd <- subset(dd, OBSET_ID != "_")
dd <- subset(dd, !is.na(VESSEL_ID))
dd <- subset(dd, !is.na(Hooks_Observed))
dd <- subset(dd,Hooks_Observed>0)
dd <- subset(dd, !is.na(Birds_Obs_Caught))
dd <- subset(dd, !is.na(Total_Hooks_Set))

#Create seasons
dd$Month <- months(dd$Date_Start_Set)

dd$Season <- "Summer"
dd$Season[dd$Month=="April"]<- "Winter"
dd$Season[dd$Month=="May"]<- "Winter"
dd$Season[dd$Month=="June"]<- "Winter"
dd$Season[dd$Month=="July"]<- "Winter"
dd$Season[dd$Month=="August"]<- "Winter"
dd$Season[dd$Month=="September"]<- "Winter"

#Catch rate on hauls
dd$rate <- dd$Birds_Obs_Caught/dd$Hooks_Observed*1000

dd <- dd[dd$Hooks_Observed<=dd$Total_Hooks_Set,] #There are five sets with more observed hooks than set hooks
#In the new data set, no sets have greater observed hooks, and only two have equal

for(i in 1:nrow(dd)){
  if(dd$Hooks_Observed[i]==dd$Total_Hooks_Set[i]){
    dd$Hooks_Observed[i] <- dd$Hooks_Observed[i]-1
  }
}

dim(dd)
head(dd)




### ATF Birdlife observers data ################################
# read in data
# first 19 columns are data, others are reference/lookup
dat <- read_excel("Namibia_Demersal_Longline_Data_October 18.xlsx", ### changed from "Copy of Namibia_Demersal_Longline_Data_revised 240818.xlsx"
                  sheet="5_Haul")[,1:15]

corpse <- read_excel("Namibia_Demersal_Longline_Data_October 18.xlsx", ### changed from "Copy of Namibia_Demersal_Longline_Data_revised 240818.xlsx"
                     sheet="8_Bycatch")[,1:14]

# remove blank rows
dat <- subset(dat, OBSET_ID != "_")
corpse <- subset(corpse, OBSET_ID != "_")

dat$mortality <- 0
# grab the bits of data & link by OBSPERIOD_ID

corpse.shot <- corpse %>%   
  group_by(OBSET_ID) %>%
  summarize(tot_mort = max(Bycatch_ID)) 

for (i in 1:nrow(dat)){
  for(j in 1:nrow(corpse.shot)){
    if(dat$OBSET_ID[i]==corpse.shot$OBSET_ID[j]) dat$mortality[i] <- corpse.shot$tot_mort[j]
  }
}

dat$Month <- months(dat$Date_Start_Haul)

dat$Season <- "Summer"
dat$Season[dat$Month=="April"]<- "Winter"
dat$Season[dat$Month=="May"]<- "Winter"
dat$Season[dat$Month=="June"]<- "Winter"
dat$Season[dat$Month=="July"]<- "Winter"
dat$Season[dat$Month=="August"]<- "Winter"
dat$Season[dat$Month=="September"]<- "Winter"

dat$Year <- 2017
dat$Year[(dat$Date_Start_Haul>(as.POSIXct("2017-06-30",format="%Y-%m-%d")))]<- 2018
dat$Year[(dat$Date_Start_Haul>(as.POSIXct("2018-06-30",format="%Y-%m-%d")))]<- 2019


dat$rate <- dat$mortality/dat$Hooks_Observed*1000

dat <- dat[dat$Hooks_Observed<=dat$Hooks_Recovered,] #There are one sets with more observed hooks than set hooks

dim(dat)
head(dat)



### READ IN PRE-REGULATION DATA
#Combining data from 2010

d.BSL <- read.csv("namibian longline data with no BSL new seasons.csv",header=T)





##############################################################
#### MANIPULATE AND COMBINE ALL NECESSARY DATASETS
##############################################################

#############################################################333333333
#First, for fisheries observer data, corrected observed hooks

dd$Hooks_Observed_Corr_Fac <- dd$Hooks_Observed*rnorm(373,0.627,0.021)
dd$Hooks_Observed_BL <- dd$Total_Hooks_Set*rnorm(373,0.563,0.019)

dd$rate <- dd$Birds_Obs_Caught/dd$Hooks_Observed_Corr_Fac*1000

#Some data tidying
dd$BSL[dd$BSL=="no"] <- "No"
dd$BSL[dd$BSL=="Single"] <- "Yes"
dd$Offal_discard[dd$Offal_discard=="no"]<-"No"


#################################################################
#Get birdlife data
#Should be able to do this from start of this file

gear <- read_excel("Namibia_Demersal_Longline_Data_October 18.xlsx", 
                   sheet="4_Set")[,1:35]

gear <- subset(gear, OBSET_ID != "_")
head(gear)
head(dat)
# grab the bits from gear; I am assuminig they are in teh same order (they seem to be)
# THIS DOES NOT WORK BECAUSE 1 TRIP WAS REMOVED
#dat$BSL <- gear$BSL
#dat$Offal_discard <- gear$Offal_discard

gear<- gear %>% select(OBSET_ID,BSL,Offal_discard)
dat <- dat %>% left_join(gear, by="OBSET_ID")
dd$Year <- 2018


#############################################################
#Join files into file called dd2

dd2 <- dd[c(3,7,11,12,15:25)]
dat2 <- data.frame("VESSEL_ID"=dat$OBSTRIP_ID,"Date_Start_Set"=dat$Date_Start_Haul, 
                   "Latitude_Start_Set"=dat$Latitude_Start_Haul,"Longitude_Start_Set"=dat$Longitude_Start_Haul,
                   "Total_Hooks_Set"=dat$Hooks_Recovered,"Hooks_Observed"=dat$Hooks_Observed,
                   "BSL"=dat$BSL,"Birds_Obs_Caught"=dat$mortality,"Offal_discard"=dat$Offal_discard,
                   "Month"=dat$Month,"Season"=dat$Season,"Hooks_Observed_Corr_Fac"=dat$Hooks_Observed,
                   "Hooks_Observed_BL"=dat$Hooks_Observed,"rate"=dat$rate,"Year"=dat$Year)
dd3 <- rbind(dd2,dat2)





#######################################################################################
#Combining data from 2010

d.BSL <- read.csv("namibian longline data with no BSL new seasons.csv",header=T)

d.BSL <- d.BSL[!is.na(d.BSL$TripNo),]
dd4 <- dd3[,c(1,8,11:13,15)]
dd4$Reg <- 1
d.BSL1 <- d.BSL[,c(2,17,21,18,4,3)]
d.BSL1$Reg <- 0
names(d.BSL1) <- c("VESSEL_ID","Birds_Obs_Caught","Season","rate","Hooks_Observed_Corr_Fac","Year","Reg")
dd5 <- rbind(dd4,d.BSL1)
head(dd5)





##############################################################
#### PREPARE DATA FOR JAGS
##############################################################

mortality <- dd5$Birds_Obs_Caught
N <- length(mortality)
Trip <- as.numeric(as.factor(dd5$VESSEL_ID))
n.trip <- length(unique(Trip))
HooksObserved <- dd5$Hooks_Observed_Corr_Fac
Season <- as.numeric(as.factor(dd5$Season))-1
Year <- dd5$Year-2008
n.year <- max(unique(Year))
Reg <- dd5$Reg



##############################################################
#### SPECIFY AND RUN JAGS MODELS 
##############################################################



sink ("model_zip.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    w[i] ~ dbern(psi)
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- w[i]*lambda[i]
    log(lambda[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]+b3*Season[i]*Reg[i]
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    psi ~ dunif(0,1)
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    b3 ~ dnorm(0,.001)

    
    R.lpsi <- logit(1-psi)
    summerprereg <- exp(b0)*1000*psi
    summerpostreg <- exp(b0+b2+b3)*1000*psi
    winterprereg <- exp(b0+b1)*1000*psi
    winterpostreg <- exp(b0+b1+b2+b3)*1000*psi
    }
    ")
sink()

mod_zip <- jags(model = "model_zip.txt", 
                data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                            Year=Year,n.year=n.year,Reg=Reg),
                inits = function(){list(b0=rnorm(1),b1=rlnorm(1),b2=rnorm(1),b3=rnorm(1),w=rep(1,N))},
                param = c("b0","b1","b2","b3","psi","R.lpsi",
                          "summerprereg","summerpostreg",
                          "winterprereg","winterpostreg","log_lik"),
                n.chains = 3, 
                n.burnin = 10000, 
                n.iter = 50000, 
                n.thin = 5,
                parallel = T)

print(mod_zip, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_zip,c("b0")) #wonderful!
#densityplot(mod_zip)
log_lik1 <- mod_zip$sims.list$log_lik
waic1 <- waic(as.matrix(log_lik1))
loo1 <-loo(as.matrix(log_lik1),cores=8)

sink ("model_negbin.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]+b3*Season[i]*Reg[i]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    b3 ~ dnorm(0,.001)
    
    
    summerprereg <- exp(b0)*1000*mean.rho
    summerpostreg <- exp(b0+b2+b3)*1000*mean.rho
    winterprereg <- exp(b0+b1)*1000*mean.rho
    winterpostreg <- exp(b0+b1+b2+b3)*1000*mean.rho
    }
    ")
sink()

mod_negbin <- jags(model = "model_negbin.txt", 
                data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                            Year=Year,n.year=n.year,Reg=Reg),
                inits = function(){list(b0=rnorm(1),b1=rlnorm(1),b2=rnorm(1),b3=rnorm(1),w=rep(1,N),
                                        alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1))},
                param = c("b0","b1","b2","b3","alpha1","alpha2",
                          "summerprereg","summerpostreg",
                          "winterprereg","winterpostreg","log_lik"),
                n.chains = 3, 
                n.burnin = 10000, 
                n.iter = 50000, 
                n.thin = 5,
                parallel = T)

print(mod_negbin, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_zip,c("b0")) #wonderful!
#densityplot(mod_zip)
log_lik2 <- mod_negbin$sims.list$log_lik
waic2 <- waic(as.matrix(log_lik2))
loo2 <- loo(as.matrix(log_lik2))

sink ("model_negbin2.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)

    
    summerprereg <- exp(b0)*1000*mean.rho
    summerpostreg <- exp(b0+b2)*1000*mean.rho
    winterprereg <- exp(b0+b1)*1000*mean.rho
    winterpostreg <- exp(b0+b1+b2)*1000*mean.rho
    }
    ")
sink()

mod_negbin2 <- jags(model = "model_negbin2.txt", 
                   data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                               Year=Year,n.year=n.year,Reg=Reg),
                   inits = function(){list(b0=rnorm(1),b1=rlnorm(1),b2=rnorm(1),w=rep(1,N),
                                           alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1))},
                   param = c("b0","b1","b2","alpha1","alpha2",
                             "summerprereg","summerpostreg",
                             "winterprereg","winterpostreg","log_lik"),
                   n.chains = 3, 
                   n.burnin = 10000, 
                   n.iter = 50000, 
                   n.thin = 5,
                   parallel = T)

print(mod_negbin2, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_zip,c("b0")) #wonderful!
#densityplot(mod_zip)
log_lik3 <- mod_negbin2$sims.list$log_lik
waic3 <- waic(as.matrix(log_lik3))
loo3 <- loo(as.matrix(log_lik3))




##############################################################
#### COMPARE JAGS MODELS 
##############################################################


compare(loo1,loo2,loo3)
# looic  se_looic elpd_loo se_elpd_loo p_loo  se_p_loo
# loo3  679.1   54.3   -339.5     27.2       133.7   10.9  
# loo2  680.9   54.6   -340.4     27.3       134.7   11.1  
# loo1 1336.9  214.2   -668.5    107.1       114.4   20.6  
#Comparison of models: NB is best, without the interaction term.





##############################################################
#### SPECIFY AND RUN A BUNCH MORE MODELS (???)
##############################################################

sink ("model_negbin_year.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]+delta[Year[i]]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    mu_int~dnorm(0, 1) # Mean hyperparameter for random intercepts (quite tight prior from Gelman for keeping answer from wandering)
    sigma_int~dexp(.5)#dunif(0, 10) # SD hyperparameter for random intercepts
    tau_int <- 1/(sigma_int*sigma_int)
    for (i in 1:n.year) {
    delta[i]~dnorm(mu_int, tau_int) # Random intercepts
    }
    
    
    summerprereg <- exp(b0+mu_int)*1000*mean.rho
    summerpostreg <- exp(b0+b2+mu_int)*1000*mean.rho
    winterprereg <- exp(b0+b1+mu_int)*1000*mean.rho
    winterpostreg <- exp(b0+b1+b2+mu_int)*1000*mean.rho
    summerchange <- (summerprereg-summerpostreg)/summerprereg
    winterchange <- (winterprereg-winterpostreg)/winterprereg
    }
    ")
sink()

mod_negbin_year <- jags(model = "model_negbin_year.txt", 
                    data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                                Year=Year,n.year=n.year,Reg=Reg),
                    inits = function(){list(b0=rnorm(1),b1=rlnorm(1),b2=rnorm(1),w=rep(1,N),
                                            alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1),
                                            mu_int=rnorm(1),sigma_int=rlnorm(1))},
                    param = c("b0","b1","b2","alpha1","alpha2","delta","mu_int","sigma_int",
                              "summerprereg","summerpostreg",
                              "winterprereg","winterpostreg",
                              "summerchange","winterchange","log_lik"),
                    n.chains = 3, 
                    n.burnin = 10000, 
                    n.iter = 50000, 
                    n.thin = 5,
                    parallel = T)

print(mod_negbin_year, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_negbin_year,c("b0")) #wonderful!
whiskerplot(mod_negbin_year,c("b0"))
log_lik4 <- mod_negbin_year$sims.list$log_lik
waic4 <- waic(as.matrix(log_lik4))
loo4 <- loo(as.matrix(log_lik4))

#Model with interaction term - doesn't work
#  one node produced an error: Error in node alpha1
#Slicer stuck at value with infinite density





sink ("model_negbin_year_int.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]+b3*Season[i]*Reg[i]+delta[Year[i]]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    b3 ~ dnorm(0,.001)
    mu_int~dnorm(0, 1) # Mean hyperparameter for random intercepts (quite tight prior from Gelman for keeping answer from wandering)
    sigma_int~dexp(.5)#dunif(0, 10) # SD hyperparameter for random intercepts
    tau_int <- 1/(sigma_int*sigma_int)
    for (i in 1:n.year) {
    delta[i]~dnorm(mu_int, tau_int) # Random intercepts
    }
    
    
    summerprereg <- exp(b0+mu_int)*1000*mean.rho
    summerpostreg <- exp(b0+b2+b3+mu_int)*1000*mean.rho
    winterprereg <- exp(b0+b1+mu_int)*1000*mean.rho
    winterpostreg <- exp(b0+b1+b2+b3+mu_int)*1000*mean.rho
    summerchange <- (summerprereg-summerpostreg)/summerprereg
    winterchange <- (winterprereg-winterpostreg)/winterprereg
    }
    ")
sink()

model_negbin_year_int <- jags(model = "model_negbin_year_int.txt", 
                        data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                                    Year=Year,n.year=n.year,Reg=Reg),
                        inits = function(){list(b0=rnorm(1),b1=rlnorm(1),b2=rnorm(1),b3=rnorm(1),w=rep(1,N),
                                                alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1),
                                                mu_int=rnorm(1),sigma_int=rlnorm(1))},
                        param = c("b0","b1","b2","b3","alpha1","alpha2","delta","mu_int","sigma_int",
                                  "summerprereg","summerpostreg",
                                  "winterprereg","winterpostreg",
                                  "summerchange","winterchange","log_lik"),
                        n.chains = 3, 
                        n.burnin = 100000, 
                        n.iter = 500000, 
                        n.thin = 10,
                        parallel = T)

print(model_negbin_year_int, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_negbin_year,c("b0")) #wonderful!
whiskerplot(mod_negbin_year,c("b0"))
log_lik5 <- model_negbin_year_int$sims.list$log_lik
waic5 <- waic(as.matrix(log_lik5))
loo5 <- loo(as.matrix(log_lik5))

sink ("model_negbin_year_no_season.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b2*Reg[i]+delta[Year[i]]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    mu_int~dnorm(0, 1) # Mean hyperparameter for random intercepts (quite tight prior from Gelman for keeping answer from wandering)
    sigma_int~dexp(.5)#dunif(0, 10) # SD hyperparameter for random intercepts
    tau_int <- 1/(sigma_int*sigma_int)
    for (i in 1:n.year) {
    delta[i]~dnorm(mu_int, tau_int) # Random intercepts
    }
    
    
    prereg <- exp(b0+mu_int)*1000*mean.rho
    postreg <- exp(b0+b2+mu_int)*1000*mean.rho
    change <- (prereg-postreg)/prereg
    }
    ")
sink()

mod_negbin_year_no_season <- jags(model = "model_negbin_year_no_season.txt", 
                        data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,
                                    Year=Year,n.year=n.year,Reg=Reg),
                        inits = function(){list(b0=rnorm(1),b2=rnorm(1),w=rep(1,N),
                                                alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1),
                                                mu_int=rnorm(1),sigma_int=rlnorm(1))},
                        param = c("b0","b2","alpha1","alpha2","delta","mu_int","sigma_int",
                                  "prereg","postreg",
                                  
                                  "change","log_lik"),
                        n.chains = 3, 
                        n.burnin = 10000, 
                        n.iter = 50000, 
                        n.thin = 5,
                        parallel = T)

print(mod_negbin_year_no_season, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_negbin_year,c("b0")) #wonderful!
whiskerplot(mod_negbin_year,c("b0"))
log_lik6 <- mod_negbin_year_no_season$sims.list$log_lik
waic6 <- waic(as.matrix(log_lik6))
loo6 <- loo(as.matrix(log_lik6))

sink ("model_zip2.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    w[i] ~ dbern(psi)
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- w[i]*lambda[i]
    log(lambda[i]) <- log(HooksObserved[i])+b0+b1*Season[i]+b2*Reg[i]+alpha[Year[i]]
    #log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    #log_lik0 <- sum(log_lik[])
    psi ~ dunif(0,1)
    b0 ~ dnorm(0,.001)
    b1 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    mu_int~dnorm(0, 1) # Mean hyperparameter for random intercepts (quite tight prior from Gelman for keeping answer from wandering)
    sigma_int~dexp(.5)#dunif(0, 10) # SD hyperparameter for random intercepts
    tau_int <- 1/(sigma_int*sigma_int)
    for (i in 1:n.year) {
    alpha[i]~dnorm(mu_int, tau_int) # Random intercepts
    }
    
    
    R.lpsi <- logit(1-psi)
    summerprereg <- exp(b0)*1000*psi
    summerpostreg <- exp(b0+b2)*1000*psi
    winterprereg <- exp(b0+b1)*1000*psi
    winterpostreg <- exp(b0+b1+b2)*1000*psi
    }
    ")
sink()

mod_zip <- jags(model = "model_zip.txt", 
                data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                            Year=Year,n.year=n.year,Reg=Reg),
                inits = function(){list(b0=rlnorm(1),b1=rlnorm(1),b2=rlnorm(1),w=rep(1,N),
                                        mu_int=rnorm(1),sigma_int=rlnorm(1))},
                param = c("b0","b1","b2","psi","alpha","R.lpsi",
                          "summerprereg","summerpostreg",
                          "winterprereg","winterpostreg","mu_int","sigma_int"),
                n.chains = 3, 
                n.burnin = 100000, 
                n.iter = 500000, 
                n.thin = 10,
                parallel = T)

print(mod_zip, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
#traceplot(mod_zip,c("b0")) #wonderful!
#densityplot(mod_zip)
log_lik2 <- mod_zip$sims.list$log_lik
waic2 <- waic(as.matrix(log_lik2))
loo2 <- loo(as.matrix(log_lik2))


sink ("model_negbin2.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b2*Reg[i]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    
    
    prereg <- exp(b0)*1000*mean.rho
    postreg <- exp(b0+b2)*1000*mean.rho
    }
    ")
sink()

mod_negbin2 <- jags(model = "model_negbin2.txt", 
                    data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,Season=Season,
                                Year=Year,n.year=n.year,Reg=Reg),
                    inits = function(){list(b0=rnorm(1),b2=rnorm(1),w=rep(1,N),
                                            alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1))},
                    param = c("b0","b2","alpha1","alpha2",
                              "prereg","postreg"),
                    n.chains = 3, 
                    n.burnin = 10000, 
                    n.iter = 50000, 
                    n.thin = 5,
                    parallel = T)

print(mod_negbin2, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 




#######################################################################################
#### SPECIFY AND RUN WHAT IS PROBABLY THE FINAL MODEL ACTUALLY REPORTED IN THE PAPER
#######################################################################################



sink ("model_negbin_trip_no_season.txt")
cat(" model {
    
    #likelihood
    
    for (i in 1:N){
    mortality[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- rho[i]* mu[i]
    log(mu[i]) <- log(HooksObserved[i])+b0+b2*Reg[i]+delta[Trip[i]]
    rho[i] ~ dgamma(alpha1,alpha2)        #rho is variable with a gamma distribution with the shape and scale the same
    log_lik[i] <- logdensity.pois(mortality[i], eff.lambda[i])
    
    }
    log_lik0 <- sum(log_lik[])
    alpha1 ~ dlnorm(.5,.5)
    alpha2 ~ dlnorm(.5,.5)
    mean.rho <- alpha1/alpha2
    b0 ~ dnorm(0,.001)
    b2 ~ dnorm(0,.001)
    mu_int~dnorm(0, 1) # Mean hyperparameter for random intercepts (quite tight prior from Gelman for keeping answer from wandering)
    sigma_int~dexp(.5)#dunif(0, 10) # SD hyperparameter for random intercepts
    tau_int <- 1/(sigma_int*sigma_int)
    for (i in 1:n.trip) {
    delta[i]~dnorm(mu_int, tau_int) # Random intercepts
    }
    
    
    prereg <- exp(b0+mu_int)*1000*mean.rho
    postreg <- exp(b0+b2+mu_int)*1000*mean.rho
    change <- (prereg-postreg)/prereg
    }
    ")
sink()

mod_negbin_trip_no_season <- jags(model = "model_negbin_trip_no_season.txt", 
                                  data = list(mortality=mortality,HooksObserved=HooksObserved,N=N,
                                              Trip=Trip,n.trip=n.trip,Reg=Reg),
                                  inits = function(){list(b0=rnorm(1),b2=rnorm(1),w=rep(1,N),
                                                          alpha1 = rlnorm(1,1,1),alpha2 = rlnorm(1,1,1),
                                                          mu_int=rnorm(1),sigma_int=rlnorm(1))},
                                  param = c("b0","b2","alpha1","alpha2","delta","mu_int","sigma_int",
                                            "prereg","postreg",
                                            
                                            "change","log_lik"),
                                  n.chains = 3, 
                                  n.burnin = 10000, 
                                  n.iter = 50000, 
                                  n.thin = 5,
                                  parallel = T)

print(mod_negbin_trip_no_season, dig=3)   #Mean of b0 not the same as the mean (also not even up and down); all suggest not a good actual fit (but this is all just flying a kite about it, not "real" method) 
log_lik2 <- mod_negbin_trip_no_season$sims.list$log_lik
waic2 <- waic(as.matrix(log_lik2))
loo2 <- loo(as.matrix(log_lik2))

looic       676.3 54.1



par(mfrow=c(2,2))

#Breeding score
plot(mod_negbin_trip_no_season$sims.list$prereg,xlim=c(0,2),ylim=c(2,5),xlab="Estimated catch rate (birds/1000 hooks)",ylab="",type="n",axes=F,main="")
axis(1)
axis(2,at=3:4,labels=c("Pre-regs","Post-regs"),las=1)
abline(v=c(-0.5,0.5),col="grey"):abline(v=0)
for(k in 3:4){
  denstrip(unlist(mod_negbin_trip_no_season$sims.list$b[,k]),at=k,width=.4,ticks=mod_negbin_trip_no_season$summary[(k+1),c(3,5,7)])
}

denstrip(unlist(mod_negbin_trip_no_season$sims.list$prereg),at=3,width=.4,ticks=mod_negbin_trip_no_season$summary[34,c(3,5,7)])
denstrip(unlist(mod_negbin_trip_no_season$sims.list$postreg),at=4,width=.4,ticks=mod_negbin_trip_no_season$summary[35,c(3,5,7)])

