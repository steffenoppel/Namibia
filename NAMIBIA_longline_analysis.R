### ##########################################
###
### Namibia demersal longline bycatch analysis
###
### ##########################################

### written by Steffen Oppel 12 August 2019
## data cleaning and preparation in "NAMIBIA_longline_data_prep.R"
## model based on Gardner et al. 2008 and Field et al. 2019
## originally started by Tim Reid in March 2018
## very little of Tim's JAGS code used

## modifications included on 15 Aug 2019 based on Adam Butler's suggestions
## included regulation in occurrence part of model [leads to invalid parent error]
## added uncertainty in estimation of n hooks that were actually observed - included extra logistic regression to properly estimate n hooks observed
## v2: changed link function for observed hooks offset in occurrence part of the model to cloglog
## included transformed output (lifted from Tim's code) to report mean bycatch per 1000 hooks - DOES NOT INCLUDE OCCURRENCE due to invalid parent error
## v3: changed abundance part of model from poisson to negative binomial due to lack of fit of poisson

## v4: included suggestions from Adam Butler on 19 Aug 2019
## v5: due to non-convergence and uncertainty about GoF test - removed godness-of-fit test from model


## UPDATED on 13 Sept to include extrapolation of fleet-wide effort


##############################################################
#### load ALL NECESSARY libraries
##############################################################

library(tidyverse)
library(lubridate)
library(data.table)
library(jagsUI)
library(dplyr)
library(tidyr)
library(ggplot2)


##############################################################
#### LOAD PRE_ARRANGED DATA
##############################################################

setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")
data<-fread("ATF_Namibia_Longline_Bycatch_data2009_2019.csv")
head(data)


##############################################################
#### FAO OBSERVERS DID NOT RECORD THE NUMBER OF ACTUALLY OBSERVED HOOKS
##############################################################

## Calculate the proportion of observed hooks from ATF
## assume that proportion for FAO was similar


## RATIO for ATF
data %>% dplyr::filter(!is.na(Hooks_Recovered)) %>%
  dplyr::filter(!is.na(Hooks_Observed)) %>%
  mutate(ratio=Hooks_Observed/Hooks_Recovered) %>%
  group_by(Year, Regulation) %>%
  summarise(mean=mean(ratio,na.rm=T), sd=sd(ratio,na.rm=T))


## THE FOA Observer ratio is much higher (and cannot be trusted)
data %>% dplyr::filter(!is.na(Total_Hooks_Set)) %>%
  mutate(ratio=Hooks_Recovered/Total_Hooks_Set) %>%
  group_by(Year, Regulation) %>%
  summarise(mean=mean(ratio,na.rm=T), sd=sd(ratio,na.rm=T))



##############################################################
#### INSPECT DATA DISTRIBUTION
##############################################################

### OCCURRENCE OF BYCATCH REDUCED FROM >60% to <10%

data %>% mutate(bycatch=ifelse(Birds_Obs_Caught>0,1,0)) %>%
  group_by(Regulation) %>%
  summarise(byc_occ=mean(bycatch))


### MEDIAN NUMBER OF BIRDS CAUGHT ON AVERAGE REDUCED FROM 6 to 1

data %>% mutate(bycatch=ifelse(Birds_Obs_Caught>0,1,0)) %>%
  dplyr::filter(bycatch==1) %>%
  group_by(Regulation) %>%
  summarise(byc_rate=median(Birds_Obs_Caught))



### NUMBER OF BIRDS CAUGHT PER THOUSAND HOOKS

data %>% mutate(Hooks_Observed=ifelse(is.na(Hooks_Observed),Hooks_Recovered*0.56,Hooks_Observed)) %>%
  mutate(rate=(Birds_Obs_Caught/Hooks_Observed)*1000) %>%
  group_by(Regulation) %>%
  summarise(byc_rate=mean(rate))


### show distribution of count data (>0)

data %>% mutate(bycatch=ifelse(Birds_Obs_Caught>0,1,0)) %>%
  dplyr::filter(bycatch==1) %>%
  ggplot() + geom_histogram(aes(x=Birds_Obs_Caught))


### inspect outlier:

data %>% dplyr::filter(Birds_Obs_Caught>35)




##############################################################
#### EXTRACT SAMPLE SIZES AND NUMBERS FOR MANUSCRIPT
##############################################################

head(data)

### for Methods
data %>% group_by(Regulation) %>% mutate(count=1) %>%
  summarise(nships=length(unique(VESSEL_ID)),ntrips=length(unique(Trip_ID)),nsets=sum(count))


### for Results
data %>%
  dplyr::filter(!is.na(Hooks_Observed)) %>%
  group_by(Regulation) %>% 
  summarise(nset=sum(Total_Hooks_Set, na.rm=T),nretr=sum(Hooks_Recovered, na.rm=T),nobs=sum(Hooks_Observed, na.rm=T),nbird=sum(Birds_Obs_Caught))


data %>%
  dplyr::filter(!is.na(Hooks_Observed)) %>%
  group_by(Regulation) %>% 
  summarise(nset=sum(Total_Hooks_Set, na.rm=T),nretr=sum(Hooks_Recovered, na.rm=T),nobs=sum(Hooks_Observed, na.rm=T),nbird=sum(Birds_Obs_Caught))



data %>% mutate(bycatch=ifelse(Birds_Obs_Caught==0,0,1)) %>%
  group_by(Regulation) %>% 
  summarise(nset=sum(bycatch, na.rm=T),perc=mean(bycatch))



data %>% mutate(count=1) %>%
  group_by(Regulation,BSL) %>% 
  summarise(nset=sum(count))

data %>% mutate(count=1) %>%
  mutate(bycatch=ifelse(Birds_Obs_Caught==0,0,1)) %>%
  group_by(BSL) %>% 
  summarise(nset=sum(bycatch),rate=mean(bycatch))



##############################################################
#### SPECIFY AND RUN JAGS MODELS 
##############################################################


sink ("ATF_Nam_LongLine_Bycatch_v5.jags")
cat(" 
model{
  
  # PRIORS FOR REGRESSION PARAMETERS
  intercept.occu ~ dnorm(0, 0.01)  ## intercept for occurrence of bycatch
  intercept.abund ~ dnorm(0, 0.01)  ## intercept for quantity of bycatch
  rho ~ dunif(0,50)  ## overdispersion parameter for negative binomial distribution

  treat.occu ~ dnorm(0, 0.01)
  treat.abund ~ dnorm(0, 0.01)
  
  logitprop.obs.mn ~ dnorm(0,0.001)
  
  # RANDOM TRIP EFFECTS FOR OCCURRENCE AND ABUNDANCE
  for(t in 1:ntrips){
    occ.trip[t]~dnorm(0,tau.occ.trip)    ## trip-specific random effect for occurrence
    abund.trip[t]~dnorm(0,tau.ab.trip)    ## trip-specific random effect for abundance
    obs.trip[t]~dnorm(0,tau.obs.trip)    ## trip-specific random effect for occurrence
    }
  tau.obs.trip<-1/(sigma.obs.trip*sigma.obs.trip)
  sigma.obs.trip~dunif(0,10)
  tau.occ.trip<-1/(sigma.occ.trip*sigma.occ.trip)
  sigma.occ.trip~dunif(0,10)
  tau.ab.trip<-1/(sigma.ab.trip*sigma.ab.trip)
  sigma.ab.trip~dunif(0,10)
  

  # RANDOM VESSEL EFFECTS FOR OCCURRENCE AND ABUNDANCE
  for(t in 1:nships){
    occ.ship[t]~dnorm(0,tau.occ.ship)    ## ship-specific random effect for occurrence
    abund.ship[t]~dnorm(0,tau.ab.ship)    ## ship-specific random effect for abundance
  }
  tau.occ.ship<-1/(sigma.occ.ship*sigma.occ.ship)
  sigma.occ.ship~dunif(0,10)
  tau.ab.ship<-1/(sigma.ab.ship*sigma.ab.ship)
  sigma.ab.ship~dunif(0,10)
  

  ####  ESTIMATE OBSERVED HOOKS (NO DATA FOR FOA OBSERVERS)
  ## obsHook are provided data for 2 data sets, but NA for the third data set - need to interpolate

    for(i in 1:Nobs){

      obsHook[i] ~ dbin(prop.obs[i],retrHook[i])
      logit(prop.obs[i])<-logitprop.obs.mn + obs.trip[trip[i]]						### random effect for observer on each trip

    } ## end loop over each observation that has data for retrieved hooks 
  
  
  #### LIKELIHOOD LOOP OVER  every set of longlines

  for(i in 1:N){
    
    # define the logistic regression model, where psi is the probability of bycatch occurring at all
    cloglog(psi[i]) <- intercept.occu + log(obsHook[i]) + occ.trip[trip[i]] + occ.ship[ship[i]] + treat.occu*REGULATION[i]  ###  replaced log(-log(1 - psi[i])) with cloglog(psi[i])
    z[i]~dbern(psi[i])
    
    # define the negative binomial regression model for abundance and multiply with bycatch probability
    mortality[i] ~ dnegbin(phi[i],rho)
    phi[i] <- rho/(rho+(z[i])*lambda[i]) - 1e-10*(1-z[i])
    log(lambda[i])<- log(obsHook[i]) + intercept.abund + treat.abund*REGULATION[i] + abund.trip[trip[i]] + abund.ship[ship[i]]
    
  } ## end loop over each observation
  
  
  # CONVERT TO ESTIMATES PER 1000 HOOKS BEFORE AND AFTER REGULATION
  prereg <- (((1-exp(-exp(intercept.occu+log(1000)))))*(exp(intercept.abund)*1000))
  postreg <- (((1-exp(-exp(intercept.occu+treat.occu+log(1000)))))*(exp(intercept.abund + treat.abund)*1000))

  
} ## end model
    ")
sink()





##############################################################
#### PREPARE DATA FOR JAGS
##############################################################


jags.data <- list(mortality=data$Birds_Obs_Caught,
                  retrHook=round(data$Hooks_Recovered),   ## maybe need to scale as in the 1000s? model runs forever if we do, but only 5 min if we leave original N
                  obsHook=round(data$Hooks_Observed),
                  N=dim(data)[1],
                  Nobs=dim(data[!is.na(data$Hooks_Recovered),])[1],
                  ntrips=length(unique(data$Trip_ID)),
                  nships=length(unique(data$VESSEL_ID)),
                  trip=as.numeric(as.factor(data$Trip_ID)),
                  ship=as.numeric(as.factor(data$VESSEL_ID)),
                  REGULATION=ifelse(data$Regulation=="BEFORE",0,1))

inits = function(){list(treat.occu=rnorm(1,0,0.1),
                        treat.abund=rnorm(1,0,0.1))}

params <- c("treat.abund","treat.occu","prereg","postreg")




##############################################################
#### RUN JAGS MODEL [takes 35 minutes]
##############################################################
n.chains = 4 
n.burnin = 50000
n.iter = 100000
n.thin = 5


NamLLmodel <- jagsUI(data=jags.data,
                  model = "C:/STEFFEN/RSPB/Marine/Bycatch/Namibia/ATF_Nam_LongLine_Bycatch_v5.jags",
                  inits=inits,
                  parameters.to.save =params,
                  n.chains=n.chains,
                  n.thin = n.thin,
                  n.iter = n.iter,
                  n.burnin = n.burnin, parallel=T, n.cores=4)





###############################################################################
####   CALCULATE CHANGE FROM POSTERIOR SAMPLES   #############################
###############################################################################


change<-(NamLLmodel$sims.list$prereg-NamLLmodel$sims.list$postreg)/NamLLmodel$sims.list$prereg
quantile(change,0.5)
quantile(change,0.025)
quantile(change,0.975)





###############################################################################
####   CREATE OUTPUT TABLE FOR MANUSCRIPT   #############################
###############################################################################


LLsum<-data %>% mutate(Hooks_Observed=ifelse(is.na(Hooks_Observed),Hooks_Recovered*0.56,Hooks_Observed)) %>%
  mutate(rate=(Birds_Obs_Caught/Hooks_Observed)*1000) %>%
  group_by(Regulation) %>%
  summarise(byc_rate=mean(rate)) %>%
  bind_cols(as.data.frame(NamLLmodel$summary[4:3,c(1,3,7)])) %>%
  rename(lcl=`2.5%`,ucl=`97.5%`) %>%
  arrange(desc(Regulation))


#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
LLsum[3,2]<-apply(as.matrix(LLsum[,2]),2,percchange)
LLsum[3,3:5]<-c(mean(change,na.rm=T)*100,quantile(change,0.025)*100,quantile(change,0.975)*100)
LLsum[3,1]<-"CHANGE(%)"
LLsum

fwrite(LLsum,"Namibia_longline_bycatch_REG_comparison.csv")




###############################################################################
####   EVALUATE MODEL FIT WITH BAYESIAN P VALUE   #############################
###############################################################################
## does not fit - but not sure whether fit statistic is indeed calculated correctly
## abandoned from v5 onwards

# plot(NamLLmodel$sims.list$fit, NamLLmodel$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE)
# abline(0, 1, lwd = 2, col = "black")
# mean(NamLLmodel$sims.list$fit.new > NamLLmodel$sims.list$fit)
# mean(NamLLmodel$mean$fit) / mean(NamLLmodel$mean$fit.new)









##############################################################
#### EXTRAPOLATION OF FLEET-WIDE MORTALITY
##############################################################
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")

## read in fleet-wide effort data
LLsum<-fread("Namibia_longline_bycatch_REG_comparison.csv")

LLeff <- read_excel("LL data combined 2016-2018.xlsx", 
                    sheet="Sheet1")
head(LLeff)




## manipulate and summarise fleet-wide seabird mortality based on fatal interactions

LLsummary<- LLeff %>%
  rename(effort=`Number of HOOKS_SET`) %>%
  mutate(Regulation=if_else(Year>2015,"AFTER","BEFORE")) %>%
  group_by(Regulation,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  mutate(bycatch=tot_effort*LLsum$mean[match(Regulation,LLsum$Regulation)]/1000,
         bycatch.lcl=tot_effort*LLsum$lcl[match(Regulation,LLsum$Regulation)]/1000,
         bycatch.ucl=tot_effort*LLsum$ucl[match(Regulation,LLsum$Regulation)]/1000) %>%
  mutate(Fishery="Longline")




fwrite(LLsummary,"Namibia_fleetwide_longline_seabird_mortality.csv")








##############################################################
#### TEST BASIC MODEL TO ESTIMATE NUMBER OF HOOKS OBSERVED 
##############################################################


sink ("ATF_obsHooks.jags")
cat(" 
    model{
    
    ####  ESTIMATE OBSERVED HOOKS (NO DATA FOR FAO OBSERVERS)
    ## obsHook are provided data for 2 data sets, but NA for the third data set - need to interpolate
    
    ### PRIORS FOR REGRESSION PARAMETERS

    logitprop.obs.mn ~ dnorm(0,0.001)

    # RANDOM TRIP EFFECTS FOR OCCURRENCE AND ABUNDANCE
    for(t in 1:ntrips){
      obs.trip[t]~dnorm(0,tau.obs.trip)    ## trip-specific random effect for occurrence
    }
    tau.obs.trip<-1/(sigma.obs.trip*sigma.obs.trip)
    sigma.obs.trip~dunif(0,10)
    
    for(i in 1:Nobs){

    obsHook[i] ~ dbin(prop.obs[i],retrHook[i])
    logit(prop.obs[i])<-logitprop.obs.mn + obs.trip[trip[i]]						### random effect for observer on each trip
    #logit(prop.obs[i]) ~ dnorm(logitprop.obs.mn, logitprop.obs.tau)   ### this results in a syntax error

    } ## end loop over each observation that has  
    
    ratio<-mean(prop.obs[])


    
    } ## end model
    ")
sink()





##############################################################
#### PREPARE DATA FOR JAGS
##############################################################


jags.data <- list(retrHook=round(data$Hooks_Recovered),   ## maybe need to scale as in the 1000s? model runs forever if we do, but only 5 min if we leave original N
                  obsHook=round(data$Hooks_Observed),
                  ntrips=length(unique(data$Trip_ID)),
                  trip=as.numeric(as.factor(data$Trip_ID)),
                  Nobs=dim(data[!is.na(data$Hooks_Recovered),])[1])

inits = function(){list(logitprop.obs.mn=rnorm(1,0,0.1),
                        logitprop.obs.tau=rnorm(1,0,0.1))}

params <- c("ratio")




##############################################################
#### RUN JAGS MODEL [takes>25 hours]
##############################################################
n.chains = 3 
n.burnin = 10000
n.iter = 50000
n.thin = 1


test.obs <- jagsUI(data=jags.data,
                  model = "C:/STEFFEN/RSPB/Marine/Bycatch/Namibia/ATF_obsHooks.jags",
                  inits=inits,
                  parameters.to.save =params,
                  n.chains=n.chains,
                  n.thin = n.thin,
                  n.iter = n.iter,
                  n.burnin = n.burnin, parallel=T, n.cores=8)
