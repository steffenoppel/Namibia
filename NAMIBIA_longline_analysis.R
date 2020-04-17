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

## REVISION of manuscript on 24 March 2020 - included sunrise to quantify pre-dawn setting proportion
## calculated observer coverage as proportion of hooks deployed on monitored trips


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
library(suncalc)  ## for sunrise calculations
library(readxl)
filter<-dplyr::filter
select<-dplyr::select




#######################################################################
#### PLOT FIGURE 1 WITH REGULATION AND OBSERVER TYPE (Reviewer request)
#######################################################################

### READ IN THE LONGLINE DATA
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")
LLdat<-read_excel("Data\\LL data combined 2016-2018.xlsx", sheet="Sheet1") ## not sure this has all data?
head(LLdat)

LLdat <- LLdat %>% 
  mutate(LAT=LAT_DEG_Shoot+(LAT_MIN_Shoot/60)) %>%
  mutate(LON=LON_DEG_Shoot+(LON_MIN_Shoot/60)) %>%
  mutate(Period=ifelse(Year<2015,"before regulation","with regulation")) %>%
  select(Period,Year,Month,Day,LAT,LON,Effort) %>%
  mutate(Fishery="Demersal longline")

LLdatprereg<-fread("Data\\Namibia Longline Master sheet 05November2013.csv") ## pre-regulation data
LLdat <- LLdatprereg %>% 
  rename(LAT=LatS, LON=LongE, Effort=HooksPerLine) %>%
  mutate(LAT=LAT*(-1)) %>%
  mutate(Period=ifelse(Year<2015,"before regulation","with regulation")) %>%
  mutate(Month=month(Date),Day=day(Date)) %>%
  select(Period,Year,Month,Day,LAT,LON,Effort) %>%
  mutate(Fishery="Demersal longline") %>%
  bind_rows(LLdat)

### READ IN THE TRAWL DATA
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia\\Data")
dat1 <- read_excel("Wetfish data 2009-2010, 2016-2018.xlsx", sheet="2009-2010")
dat2 <- read_excel("Wetfish data 2009-2010, 2016-2018.xlsx", sheet="2016-2017")
WetFishdat<-rbind(dat1,dat2) %>%
  mutate(Period=ifelse(year(DATE)<2015,"before regulation","with regulation")) %>%
  mutate(LAT=LatDeg+(LatMin/60)) %>%
  mutate(LON=LongDeg+(LongMin/60)) %>%
  rename(Effort=`DURATION(HOURS)`) %>%
  mutate(Year=year(DATE),Month=month(DATE),Day=day(DATE)) %>%
  select(Period,Year,Month,Day,LAT,LON,Effort) %>%
  mutate(Fishery="Trawl (wet fish)")

PLOTDAT<-bind_rows(WetFishdat,LLdat) %>%
  mutate(LAT=LAT*(-1))

### NINA PROVIDED THIS FILE ON 16 APRIL 2020 WHICH INCLUDES ALL DATA
PLOTDAT<-read_excel("ALL_fishing_effort_spatial.xlsx", sheet="Data for plot") %>% ## not sure this has all data?
	mutate(LAT=LatDeg+(LatMin/60)) %>%
  	mutate(LON=LongDeg+(LongMin/60)) %>%
  	mutate(period=ifelse(period=='pre',"before regulation","with regulation")) %>%
  	mutate(fishery=ifelse(fishery=="trawl","Trawl (wet fish)","Longline"))#%>%
  	#mutate(LAT=LAT*(-1))

head(PLOTDAT)




### LOAD EEZ AND BASELINE DATA
library(sf)
setwd("C:\\STEFFEN\\RSPB\\Marine\\World_EEZ")
EEZ<-st_read("World_EEZ_v8_2014_HR.shp") %>% filter (Country=="Namibia")

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")


### CREATE FIGURE 1 ###

ggplot(data = world) + 
  geom_sf(fill="lightgrey",color = "black", lwd=0.5) +
  geom_sf(data=EEZ, color = "midnightblue", lwd=1.5, fill=NA) +
  coord_sf(ylim = c(-30,-17),  xlim = c(11,17))+
  #coord_sf(ylim = c(-85,30),  xlim = c(-50,50))+

  ### ADD FISHING LOCATIONS
  geom_point(data=PLOTDAT, aes(x=LongDeg, y=LatDeg), size=1,color='red') +   #, colour=Period,colour=observed
  facet_grid(period~fishery) +
  geom_sf(fill="lightgrey",color = "black", lwd=0.5) +
  
  ### ADJUST AXES AND LABELS
  guides(colour=guide_legend(title="Observer"))+
  scale_color_viridis_d(begin = .2, end = .8, direction = 1, name = "Observer:”) + for the tracks
  scale_fill_viridis_d(begin = .2, end = .8, direction = -1, option = "B", name = "fate:”) + for fates

  xlab("Longitude")+
  ylab("Latitude")+

  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"),
        plot.margin=unit(c(5,0,5,0),"mm"),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=12, color="black", angle=45,vjust=0.5), 
        axis.title=element_text(size=16),
        legend.background = element_rect(),
        legend.title = element_text(size=16),
        legend.key = element_blank(),
        legend.text=element_text(size=14),
        strip.text=element_text(size=15, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Figure1.jpg", width=7, height=12)






##############################################################
#### CALCULATE PROPORTION OF LONGLINE SETS THAT WERE COMPLETED BEFORE NAUTICAL DAWN (Reviewer request)
##############################################################

setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")
#setdat<-read_excel("Data\\Namibia_Demersal_Longline_Data_October 18.xlsx", sheet="4_Set") ## did not work because dates and times were messed up
setdat<-fread("Data\\Namibia_Demersal_Longline_SettingTimes.csv")
setdat <- setdat %>% 
  mutate(START=dmy_hm(paste(Date_Start_Set,Time_Start_Set," "))) %>%
  mutate(END=dmy_hm(paste(Date_End_Set,Time_End_Set," "))) %>%
  mutate(date=as.Date(END,origin="1970-01-01")) %>%
  mutate(lat=as.numeric(Latitude_End_Set),lon=as.numeric(Longitude_End_Set)) %>%
  select(START,END,date,lat,lon)
dim(setdat)

## add pre-regulation data
LLdatprereg<-fread("Data\\Namibia Longline Master sheet 05November2013.csv") 
setdat <- LLdatprereg %>% 
  mutate(START=dmy_hm(paste(Date,ShootStarttime," "))) %>%
  mutate(END=dmy_hm(paste(Date,ShootEndTime," "))) %>%
  mutate(date=as.Date(END,origin="1970-01-01")) %>%
  mutate(lat=as.numeric(LatS),lon=as.numeric(LongE)) %>%
  filter(!is.na(START)) %>%
  filter(!is.na(END)) %>%
  select(START,END,date,lat,lon) %>%
  bind_rows(setdat) %>%
  mutate(Observer="ATF")
dim(setdat)

## calculate mean setlength so we can add it to the FOA data
setdat %>% mutate(setlength=as.numeric(difftime(END,START,units="hours"))) %>%
	#group_by(year(END)) %>%
	summarise(dtime=mean(setlength))

## add FOA observer data
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")
foadat<-read_excel("Data\\OBSERVER_Data_Final_OBSERVERS AND TRIPS_REVISED OCTOBER 2019.xlsx", sheet="LONGLINE_DATA") ## did not work because dates and times were messed up
hour(foadat$Date_Start_Set)<-hour(foadat$Time_Start_Set)
minute(foadat$Date_Start_Set)<-minute(foadat$Time_Start_Set)
setdat <- foadat %>% 
  rename(START=Date_Start_Set) %>%
  filter(!is.na(START)) %>%
  mutate(END=START + hours(2)) %>%
  mutate(date=as.Date(END,origin="1970-01-01")) %>%
  mutate(lat=as.numeric(Latitude_Start_Set),lon=as.numeric(Longitude_Start_Set)) %>%
  select(START,END,date,lat,lon) %>%
  mutate(Observer="FOA") %>%
  bind_rows(setdat)
dim(setdat)
head(setdat)

## SET THE TIME ZONE TO NAMIBIAN TIME
tz(setdat$END)<-"Africa/Windhoek"

## CALCULATE DAWN TIME AND ADJUST TIME ZONE TO NAMIBIA
setdat$dawn <- getSunlightTimes(data=setdat, keep = c("sunrise"), tz = "UTC")[,4]
setdat$dawn <- with_tz(setdat$dawn,tzone="Africa/Windhoek")
tail(setdat)

## SUMMARISE PROPORTION OF SETS COMPLETED BEFORE DAWN
export<-setdat %>% mutate(beforeDawn=ifelse(END < dawn,1,0)) %>%
  mutate(beforeDawn=ifelse(hour(END)>19,1,beforeDawn)) %>%
  mutate(dawndiff=ifelse(hour(END)>19,as.numeric(difftime(END,dawn,units="hours"))-24,as.numeric(difftime(END,dawn,units="hours")))) %>%
  mutate(year=year(START)) %>%
  #filter(year==2016)
  group_by(year,Observer) %>%
  summarise(prop=sum(beforeDawn)/length(beforeDawn), diff=mean(dawndiff), diff_sd=sd(dawndiff)) %>%
  rename(`prop set before sunrise`=prop, `mean time to sunrise (hrs)`=diff,`sd time to sunrise (hrs)`=diff_sd)

fwrite(export,"Namibia_LL_set_sunrise_proportions.csv")




##############################################################
#### LOAD PRE_ARRANGED DATA
##############################################################

setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")
data<-fread("Data\\ATF_Namibia_Longline_Bycatch_data2009_2019.csv")
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
  group_by(Regulation) %>%
  summarise(mean=mean(ratio,na.rm=T), sd=sd(ratio,na.rm=T), min=min(ratio,na.rm=T), max=max(ratio,na.rm=T))


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

data %>% mutate(OBS=ifelse(is.na(Hooks_Observed),"FAO","ATF")) %>%
  mutate(count=1) %>%
  filter(OBS=="ATF") %>%
  group_by(OBS,Regulation) %>% 
  summarise(nsets=sum(count))


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
    obs.trip[t]~dnorm(0,tau.obs.trip)    ## trip-specific random effect for observation effort
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
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia\\Data")

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
#### ASSESSMENT OF SAMPLING REPRESENTATIVITY
##############################################################
LLsummary<-LLsummary %>% select(Regulation,Year,tot_effort) %>%
  bind_rows(data.frame(Regulation="BEFORE",Year =as.numeric(c(2009,2010,2011,2012)), tot_effort=c(47481331,33071604,26131173,31225687)))


data %>% 
  mutate(Total_Hooks_Set=ifelse(is.na(Total_Hooks_Set),Hooks_Recovered/0.913,Total_Hooks_Set)) %>%
  mutate(Total_Hooks_Set=ifelse(is.na(Total_Hooks_Set),Hooks_Observed/(0.913*0.58),Total_Hooks_Set)) %>%
  group_by(Year, Regulation) %>%
  summarise(obs=sum(Total_Hooks_Set)) %>%
  inner_join(LLsummary, by=c("Year","Regulation")) %>%
  ungroup() %>%
  group_by(Regulation) %>%
  summarise(obs=sum(obs),tot_effort=sum(tot_effort)) %>%
  mutate(obs_ratio=obs/tot_effort) %>%
  select(tot_effort,obs,obs_ratio)



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
