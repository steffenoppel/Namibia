### ##########################################
###
### Namibia trawl bycatch analysis
### DATA PREPARATION AND ANALYSIS
### based on scripts by Tim Reid, March 2018
###
### ##########################################

### cleaned up by Steffen Oppel 23 August 2019
### only retained data cleaning/preparation - data anlysis in separate script

## each line is the interaction of one species during a given trawl set - hence the effort and interactions need to be SUMMED over the different lines of data

## FILE RECOVERED ON 13 Sept 2019 after request from Nina da Rocha
## catastrophic file overwrite occurred on 27 Aug, and some portion of the script may have been lost
## REVISION included new data and ensured output in MS matches with these data and results

## ADDED BSL operation - some BSLs were deployed incorrectly

## MINOR ADJUSTMENTS BY Nina da Rocha on 18th Sep 2019
## Data from experimental trials 2009-2010 excluded

## re-run by Steffen Oppel on 19 Sept 2019 with revised dataset

## REVISION of manuscript on 24 March 2020 - included separate analysis for setting, trawling, and haul
## added observer coverage proportion



##############################################################
#### load ALL NECESSARY libraries
##############################################################

library(boot)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(janitor)
filter<-dplyr::filter
select<-dplyr::select



##############################################################
#### LOAD ALL NECESSARY DATASETS
##############################################################
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia\\Data")

# read in data
# first 19 columns are data, others are reference/lookup
# Trawls with torilines in 2009-2010 are experimental and not representative of standard fishing practices
dat1 <- read_excel("2009-2010 demersal trawl data Namibia new.xlsx", 
                   sheet="6_Interactions")[,1:19]
dat1<- dat1 %>% filter(Tori_line_deployed != "Yes")

dat2 <- read_excel("Trawl data Aug 2019.xlsx", 
                  sheet="6_Interactions")[,1:19]
dat2<- dat2 %>% filter(OBSTRIP_ID != "BLEB2")
names(dat2)

dat<-rbind(dat1,dat2)


##############################################################
#### REMOVE ALL NOCTURNAL TRAWLS WHERE OBSERVATION EFFORT WAS ZERO
##############################################################

NIGHTTRAWLS<-dat %>% mutate(Minutes_observed=minute(Minutes_observed)) %>%
  group_by(OBSTRIP_ID,OBSTRAWL_ID) %>%
  summarise(obs_eff=sum(Minutes_observed))%>%
  filter(obs_eff==0)
dim(dat)
dat <- dat %>% filter(!OBSTRAWL_ID %in% NIGHTTRAWLS$OBSTRAWL_ID) %>%
  filter(!is.na(Tori_line_deployed))
dim(dat)



### INFO ON CORRECT DEPLOYMENT OF BSL
BSL <- read_excel("Trawl data Aug 2019.xlsx", 
                   sheet="3_Trawl")

BSL<- BSL %>% select(OBSTRIP_ID,OBSTRAWL_ID,BSL_Deployment) %>%
  mutate(BSLcorrect=if_else(BSL_Deployment=="Immediately After Doors","Yes","No")) %>%
  filter(!OBSTRAWL_ID %in% NIGHTTRAWLS$OBSTRAWL_ID)




##############################################################
#### MANIPULATE THE DATA 
##############################################################
# remove blank rows
dat <- subset(dat, OBSPERIOD_ID != "_")
head(dat)
names(dat)

# convert minutes observed from POSIXct to numeric & express in hours 
dat$t.lub <- ymd_hms(dat$Minutes_observed)
dat$h.lub <- lubridate::hour(dat$t.lub) + lubridate::minute(dat$t.lub)/60

# assimilate descriptions of Outcome
dat$Outcome[dat$Outcome=="possibly dead"]<- "Dead"
dat$Outcome[dat$Outcome=="Possibly Dead"]<- "Dead"
dat$Outcome[dat$Outcome=="Possibly dead"]<- "Dead"
dat$Outcome[dat$Outcome=="Not injured"]<- "Unharmed"
dat$Outcome[dat$Outcome=="N/A"]<- "Unknown"
dat$Outcome[dat$Outcome=="n/a"]<- "Unknown"
dat$Outcome[is.na(dat$Outcome)]<- "Unknown"

# define seasons
dat$Month <- lubridate::month(dat$Date_Start_Observation)
dat$Season <- "Summer"
dat$Season[dat$Month>3 & dat$Month<11]<- "Winter"

#define presence of regulation 
dat$REG<-ifelse(lubridate::year(dat$Date_Start_Observation)>2015,1,0)

# add information on correct BSL deployment
dat<- dat %>% left_join(BSL, by=c('OBSTRIP_ID','OBSTRAWL_ID')) %>%
  mutate(BSLcorrect=if_else(is.na(BSLcorrect),"No",BSLcorrect))
dim(dat)

## assess whether it is worth to split outcomes and interactions or whether too much data are missing:
unique(dat$Outcome)

## The n/a are associted with observations periods in which no interactions took place
## 910 entries listed as interaction species NONE with number of inteaction = 0 
dat$Species[dat$Species=="none"]<- "NONE"
dat$Species[dat$Species=="None"]<- "NONE"

table (dat$Species, dat$Outcome)
table(dat$Outcome)

## All but 5 observed interactions have an associated outcome
sum(dat$Number_interactions)
5/854 ## Under 1 % of observed interactions have an unkown outcome 





##############################################################
#### SUMMARISE THE DATA 
##############################################################


# SUMMARISE INTERACTION DATA PER TRAWL

obs.effort <- dat %>%   
  group_by(OBSTRAWL_ID,OBSPERIOD_ID, 
           Offal_during_observation,
           Tori_line_deployed,BSLcorrect,
           Vessel_Activity,
           Date_Start_Observation,REG) %>%
  summarize(obs.effort = sum(h.lub), tot.interactions=sum(Number_interactions)) %>%
  filter(obs.effort>0) %>%
  mutate(Interaction_rate = tot.interactions / obs.effort)
tail(obs.effort)


# SUMMARISE ALL FATAL INTERACTIONS

dat.mort <- dat %>%
  filter(Outcome %in% c("Dead","Possibly Dead")) %>%   
  group_by(OBSPERIOD_ID,Species) %>%
  summarize(fatal.interactions = sum(Number_interactions)) 
dat.mort


# SUMMARISE ALL HEAVY INTERACTIONS (or heavy and medium)

dat.heavy <- dat %>%
  #filter(Interaction_type=="Heavy") %>%   
  filter(Interaction_type %in% c("Heavy","Medium")) %>% ### alternatively use %in% c("Heavy","Medium")
  group_by(OBSPERIOD_ID) %>%
  summarize(heavy.interactions = sum(Number_interactions)) 
dat.heavy
sum(dat.heavy$heavy.interactions)


# COMBINE DATA TO SUMMARISE BY SETTING OPERATION ETC

NAM.trawl<- left_join(obs.effort,dat.mort, by="OBSPERIOD_ID", fill=0) %>%
  left_join(dat.heavy, by="OBSPERIOD_ID", fill=0) %>%
  filter(!is.na(Tori_line_deployed)) %>%
  mutate(fatal.interactions=ifelse(is.na(fatal.interactions),0,fatal.interactions)) %>%
  mutate(fatal.rate = fatal.interactions/obs.effort) %>%
  mutate(heavy.interactions=ifelse(is.na(heavy.interactions),0,heavy.interactions)) %>%
  mutate(heavy.rate = heavy.interactions/obs.effort) 
dim(NAM.trawl)

##fwrite(NAM.trawl,"Namibia.trawl.interactions.csv")



# SUMMARISE ALL FATAL INTERACTIONS BY SPECIES FOR RESULTS

dat %>%
  filter(Outcome %in% c("Dead","Possibly Dead")) %>%   
  group_by(Species) %>%
  summarize(fatal.interactions = sum(Number_interactions)) 



# SUMMARISE ALL HEAVY INTERACTIONS (or heavy and medium)

deathtally<-dat %>%
  #filter(Interaction_type=="Heavy") %>%   
  filter(Interaction_type %in% c("Heavy","Medium")) %>% ### alternatively use %in% c("Heavy","Medium")
  group_by(REG,Species) %>%
  summarize(heavy.interactions = sum(Number_interactions)) %>%
  mutate(prop=(heavy.interactions/sum(heavy.interactions))*100)

fwrite(deathtally,"Namibia.trawl.heavy.interactions_bySpecies.csv")



##############################################################
#### BASIC SUMMARY OF DATA FOR REPORTING IN MANUSCRIPT
##############################################################

# DEPLOYMENT OF BSL AFTER REGULATION
obs.effort %>% #filter(REG=="YES") %>%
  group_by(Tori_line_deployed) %>%
  summarise(n=count(OBSTRAWL_ID))



## Compliance levels in the post-reg period

unique(obs.effort$Tori_line_deployed)
table(obs.effort$Tori_line_deployed)

## No BSL deployment data available from 44 trawls

table(obs.effort$Tori_line_deployed, obs.effort$REG)
254/(254+23)

table(obs.effort$BSLcorrect[obs.effort$Tori_line_deployed=='Yes']) 
132/(132+122)   #when used, BSLs deployed correctly on 52% of trawls

table(obs.effort$BSLcorrect[obs.effort$REG==1]) 
132/(132+145)   #BSLs deployed correctly on 48% of post-regulation trawls





# BASIC SUMMARY OF DATA

rawsummary<-NAM.trawl %>% mutate(count=1) %>% group_by(REG,Tori_line_deployed,Vessel_Activity) %>%
  summarise(nsets=sum(count),mean.tot.rate=mean(Interaction_rate), sd.tot.rate=sd(Interaction_rate),mean.fatal.rate=mean(fatal.rate), sd.fatal.rate=sd(fatal.rate),mean.heavy.rate=mean(heavy.rate), sd.heavy.rate=sd(heavy.rate))
rawsummary
fwrite(rawsummary,"Namibia.trawl.interactions_summary.csv")

## n trips and obs effort
dat %>%
  filter(!is.na(Tori_line_deployed)) %>%
  group_by(REG) %>%
  summarise(ntrips=length(unique(OBSTRIP_ID)),nsets=length(unique(OBSTRAWL_ID)),eff=sum(h.lub,na.rm=T))


### proportion of heavy and fatal interactions

dat %>% filter(Outcome!="Unknown") %>%
  mutate(count=1) %>%
  group_by(Outcome) %>%
  summarise(n=sum(count))
14/(14+439)

## reviewer requested proportion of fatal in heavy interactions
dat %>% filter(Outcome!="Unknown") %>%
  filter(Interaction_type %in% c("Heavy","Medium")) %>% 
  mutate(count=1) %>%
  group_by(Outcome) %>%
  summarise(n=sum(count))
14/(14+175)


dat %>% filter(Interaction_type!="n/a") %>%
  mutate(count=1) %>%
  group_by(Interaction_type) %>%
  summarise(n=sum(count))
(75+115)/(75+268+115)  ## proportion of heavy and medium interactions

268/(75+268+115) ## proportion of light interactions




##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR TORI LINES
##############################################################


## stratify the samples
NAM.trawl<- NAM.trawl %>%
	mutate(group=paste(Vessel_Activity,Tori_line_deployed, sep="_"))
NAM.trawl$stratum1<-match(NAM.trawl$Vessel_Activity,unique(NAM.trawl$Vessel_Activity))
head(NAM.trawl)

samplemean <- function(x, d) {
  return(mean(x[d]))
}


#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######

prereg <- boot(NAM.trawl$Interaction_rate[NAM.trawl$Tori_line_deployed=="No"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="No"])
prereg.ci<-boot.ci(prereg,conf=0.95)

prereg.fat <- boot(NAM.trawl$fatal.rate[NAM.trawl$Tori_line_deployed=="No"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="No"])
prereg.fat.ci<-boot.ci(prereg.fat,conf=0.95)

prereg.heavy <- boot(NAM.trawl$heavy.rate[NAM.trawl$Tori_line_deployed=="No"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="No"])
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)



#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######

postreg <- boot(NAM.trawl$Interaction_rate[NAM.trawl$Tori_line_deployed=="Yes"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="Yes"])
postreg.ci<-boot.ci(postreg,conf=0.95)

postreg.fat <- boot(NAM.trawl$fatal.rate[NAM.trawl$Tori_line_deployed=="Yes"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="Yes"])
postreg.fat.ci<-boot.ci(postreg.fat,conf=0.95)

postreg.heavy <- boot(NAM.trawl$heavy.rate[NAM.trawl$Tori_line_deployed=="Yes"], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$Tori_line_deployed=="Yes"])
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)


#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.trawl %>% mutate(count=1) %>% group_by(Tori_line_deployed) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean<-c(prereg$t0,postreg$t0)
bootsummary$boot.lcl<-c(prereg.ci$normal[1,2],postreg.ci$normal[1,2])
bootsummary$boot.ucl<-c(prereg.ci$normal[1,3],postreg.ci$normal[1,3])
bootsummary$boot.mean.fatal<-c(prereg.fat$t0,postreg.fat$t0)
bootsummary$boot.lcl.fatal<-c(prereg.fat.ci$percent[1,4],postreg.fat.ci$percent[1,4])
bootsummary$boot.ucl.fatal<-c(prereg.fat.ci$percent[1,5],postreg.fat.ci$percent[1,5])
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])


#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary

fwrite(bootsummary,"Namibia_trawl_interactions_BSL_comparison.csv")


#### PLOT OUTPUT ####
ggplot(bootsummary[1:2,], aes(x=Tori_line_deployed, y=boot.mean.heavy)) +
	geom_point(size=2) +
  geom_errorbar(aes(ymin=boot.lcl.heavy, ymax=boot.ucl.heavy), width=.1)+

  ## format axis ticks
  scale_y_continuous(name="Number of medium and heavy interactions/hour", limits=c(0,1.5), breaks=seq(0,1.5,0.3), labels=seq(0,1.5,0.3))+
	xlab("Tori lines deployed") +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=18, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(margin=margin(0,20,0,0)), 
        strip.background=element_rect(fill="white", colour="black"))


# ##############################################################
# #### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR CORRECTLY DEPLOYED TORI LINES
# ##############################################################
#  THIS ANALYSIS IS NOT PARTICULARLY CONVINCING OR STRONG BECAUSE MANY LINES WERE UNOBSERVED AND HAVE NO INFO ABOUT CORRECTNESS
###For some reason this isn't calculating the right number of sets when I run it?? 
## NOTE that this should only be looking at post-regulation data in any case, preferably compaing no BSL use to correct BSL use
## updated on 18 Sept 2019 by Steffen after Nina's request
## no fatal interactions post-reg, so that aspect has been removed
# 
# ## stratify the samples
NAM.correct<- NAM.trawl %>% #filter(REG==1) %>%
  filter(!(Tori_line_deployed=="Yes" & BSLcorrect=="No")) %>%
   mutate(group=paste(Vessel_Activity,BSLcorrect, sep="_"))
NAM.correct$stratum1<-match(NAM.correct$Vessel_Activity,unique(NAM.correct$Vessel_Activity))
head(NAM.correct)

## this should be the same but isn't - there are 4 trawls where no tori line was 'correctly' deployed!!??
table(NAM.correct$Tori_line_deployed)
table(NAM.correct$BSLcorrect)

NAM.correct %>% filter(Tori_line_deployed=="No") %>% filter(BSLcorrect=="Yes")

#### SUMMARISE MEAN AND CI FOR SAMPLES WITHOUT BSL #######
 
prereg <- boot(NAM.correct$Interaction_rate[NAM.correct$BSLcorrect=="No"], samplemean, R=10000, strata=NAM.correct$stratum1[NAM.correct$BSLcorrect=="No"])
prereg.ci<-boot.ci(prereg,conf=0.95)
 
prereg.heavy <- boot(NAM.correct$heavy.rate[NAM.correct$BSLcorrect=="No"], samplemean, R=10000, strata=NAM.correct$stratum1[NAM.correct$BSLcorrect=="No"])
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)
 

#### SUMMARISE MEAN AND CI FOR SAMPLES WITH CORRECTLY DEPLOYED BSL #######
 
postreg <- boot(NAM.correct$Interaction_rate[NAM.correct$BSLcorrect=="Yes"], samplemean, R=10000, strata=NAM.correct$stratum1[NAM.correct$BSLcorrect=="Yes"])
postreg.ci<-boot.ci(postreg,conf=0.95)

postreg.heavy <- boot(NAM.correct$heavy.rate[NAM.correct$BSLcorrect=="Yes"], samplemean, R=10000, strata=NAM.correct$stratum1[NAM.correct$BSLcorrect=="Yes"])
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)


#### COMPILE OUTPUT INTO A SINGLE TABLE #######




bootsummary2<-NAM.correct %>% mutate(count=1) %>% group_by(BSLcorrect) %>%
   summarise(nsets=sum(count))
 bootsummary2$boot.mean<-c(prereg$t0,postreg$t0)
 bootsummary2$boot.lcl<-c(prereg.ci$normal[1,2],postreg.ci$normal[1,2])
 bootsummary2$boot.ucl<-c(prereg.ci$normal[1,3],postreg.ci$normal[1,3])
 bootsummary2$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
 bootsummary2$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
 bootsummary2$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])
bootsummary2

#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary2[3,3:8]<-apply(as.matrix(bootsummary2[,3:8]),2,percchange)
bootsummary2[3,1]<-"CHANGE(%)"
bootsummary2

fwrite(bootsummary2,"Namibia_trawl_interactions_correctBSL_comparison.csv")

#### PLOT OUTPUT ####
ggplot(bootsummary2[1:2,], aes(x=BSLcorrect, y=boot.mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=boot.lcl, ymax=boot.ucl), width=.1)+

  ## format axis ticks
  scale_y_continuous(name="Interactions per hour", limits=c(0,5), breaks=seq(0,5,1), labels=seq(0,5,1))+
  xlab("Tori lines correctly deployed") +

  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=18, color="black"),
        axis.title=element_text(size=18),
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        strip.background=element_rect(fill="white", colour="black"))





##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR REGULATION
##############################################################


## stratify the samples
NAM.trawl<- NAM.trawl %>%
  mutate(group=paste(Vessel_Activity,REG, sep="_"))
NAM.trawl$stratum1<-match(NAM.trawl$Vessel_Activity,unique(NAM.trawl$Vessel_Activity))
head(NAM.trawl)


#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######

prereg <- boot(NAM.trawl$Interaction_rate[NAM.trawl$REG==0], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==0])
prereg.ci<-boot.ci(prereg,conf=0.95)

prereg.fat <- boot(NAM.trawl$fatal.rate[NAM.trawl$REG==0], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==0])
prereg.fat.ci<-boot.ci(prereg.fat,conf=0.95)

prereg.heavy <- boot(NAM.trawl$heavy.rate[NAM.trawl$REG==0], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==0])
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)



#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######

postreg <- boot(NAM.trawl$Interaction_rate[NAM.trawl$REG==1], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==1])
postreg.ci<-boot.ci(postreg,conf=0.95)

postreg.fat <- boot(NAM.trawl$fatal.rate[NAM.trawl$REG==1], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==1])
postreg.fat.ci<-boot.ci(postreg.fat,conf=0.95)

postreg.heavy <- boot(NAM.trawl$heavy.rate[NAM.trawl$REG==1], samplemean, R=10000, strata=NAM.trawl$stratum1[NAM.trawl$REG==1])
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)



#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.trawl %>% mutate(count=1) %>% group_by(REG) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean<-c(prereg$t0,postreg$t0)
bootsummary$boot.lcl<-c(prereg.ci$normal[1,2],postreg.ci$normal[1,2])
bootsummary$boot.ucl<-c(prereg.ci$normal[1,3],postreg.ci$normal[1,3])
bootsummary$boot.mean.fatal<-c(prereg.fat$t0,postreg.fat$t0)
bootsummary$boot.lcl.fatal<-c(prereg.fat.ci$percent[1,4],postreg.fat.ci$percent[1,4])
bootsummary$boot.ucl.fatal<-c(prereg.fat.ci$percent[1,5],postreg.fat.ci$percent[1,5])
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])


#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary



fwrite(bootsummary,"Namibia_trawl_interactions_REG_comparison.csv")


#### PLOT OUTPUT ####
ggplot(bootsummary[1:2,], aes(x=REG, y=boot.mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=boot.lcl, ymax=boot.ucl), width=.1)+
  
  ## format axis ticks
  scale_y_continuous(name="N seabird-cable interactions per hour", limits=c(0,8), breaks=seq(0,8,2), labels=seq(0,8,2))+
  xlab("Fisheries regulation in effect") +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=18, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(margin=margin(0,20,0,0)), 
        strip.background=element_rect(fill="white", colour="black"))





##############################################################
#### EXTRAPOLATION OF FLEET-WIDE MORTALITY
##############################################################

## read in fleet-wide effort data
##cannot read in these files using this script for some reason

FReff <- read_excel("NAM_fleet_effort_freeze_trawl.xlsx", 
                    sheet="ALL")
head(FReff)

WETeff <- read_excel("NAM_fleet_effort_wetfish_trawl.xlsx", 
                     sheet="ALL")
head(WETeff)


## manipulate and summarise fleet-wide seabird mortality based on fatal interactions

FReffsummary<- FReff %>% mutate(Year=year(DATE)) %>%
  rename(effort=`DURATION (HOURS)`) %>%
  mutate(REG=if_else(Year>2015,1,0)) %>%
  group_by(REG,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  mutate(bycatch=tot_effort*bootsummary$boot.mean.fatal[match(REG,bootsummary$REG)],
         bycatch.lcl=tot_effort*bootsummary$boot.lcl.fatal[match(REG,bootsummary$REG)],
         bycatch.ucl=tot_effort*bootsummary$boot.ucl.fatal[match(REG,bootsummary$REG)])
  

WETeffsummary<- WETeff %>% mutate(Year=year(DATE)) %>%
  rename(effort=`DURATION(HOURS)`) %>%
  mutate(REG=if_else(Year>2015,1,0)) %>%
  group_by(REG,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  mutate(bycatch=tot_effort*bootsummary$boot.mean.fatal[match(REG,bootsummary$REG)],
         bycatch.lcl=tot_effort*bootsummary$boot.lcl.fatal[match(REG,bootsummary$REG)],
         bycatch.ucl=tot_effort*bootsummary$boot.ucl.fatal[match(REG,bootsummary$REG)])


fwrite(FReffsummary,"Namibia_freeze_trawl_extrapolations.csv")
fwrite(WETeffsummary,"Namibia_wetfish_trawl_extrapolations.csv")



## manipulate and summarise fleet-wide seabird mortality based on heavy interactions


FReff %>% mutate(Year=year(DATE)) %>%
  rename(effort=`DURATION (HOURS)`) %>%
  mutate(REG=if_else(Year>2015,1,0)) %>%
  group_by(REG,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  mutate(bycatch=tot_effort*bootsummary$boot.mean.heavy[match(REG,bootsummary$REG)],
         bycatch.lcl=tot_effort*bootsummary$boot.lcl.heavy[match(REG,bootsummary$REG)],
         bycatch.ucl=tot_effort*bootsummary$boot.ucl.heavy[match(REG,bootsummary$REG)])

WETeff %>% mutate(Year=year(DATE)) %>%
  rename(effort=`DURATION(HOURS)`) %>%
  mutate(REG=if_else(Year>2015,1,0)) %>%
  group_by(REG,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  mutate(bycatch=tot_effort*bootsummary$boot.mean.heavy[match(REG,bootsummary$REG)],
         bycatch.lcl=tot_effort*bootsummary$boot.lcl.heavy[match(REG,bootsummary$REG)],
         bycatch.ucl=tot_effort*bootsummary$boot.ucl.heavy[match(REG,bootsummary$REG)])




##############################################################
#### CALCULATION OF OBSERVER COVERAGE
##############################################################

### summarise total observation hours ###
EFFORT<-obs.effort %>%  mutate(Year=year(Date_Start_Observation)) %>% 
  group_by(Year,REG) %>%
  summarize(obs.effort = sum(obs.effort))

### summarise total fishing hours ###
WETeff %>% mutate(Year=year(DATE)) %>%
  rename(effort=`DURATION(HOURS)`) %>%
  mutate(REG=if_else(Year>2015,1,0)) %>%
  group_by(REG,Year) %>%
  summarise(tot_effort=sum(effort,na.rm=T)) %>%
  inner_join(EFFORT, by=c("Year","REG")) %>%
  ungroup() %>%
  group_by(REG) %>%
  summarise(tot_effort=sum(tot_effort), obs.effort=sum(obs.effort)) %>%
  mutate(obs_rate=(obs.effort/tot_effort)*100)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REQUESTED UPON REVISION - ADDED ON 24 March 2020
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PHASE-SPECIFIC COMPARISON


##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR REGULATION FOR SETTING
##############################################################

## subset the data
NAM.set<- NAM.trawl %>% #filter(!(REG==0 & Tori_line_deployed=="Yes")) %>%   ### Nina requested that on 31 May for rebuttal letter
  filter(Vessel_Activity=="Set")

#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######
prereg.heavy <- boot(NAM.set$heavy.rate[NAM.set$REG==0], samplemean, R=10000)
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)


#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######
postreg.heavy <- boot(NAM.set$heavy.rate[NAM.set$REG==1], samplemean, R=10000)
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)

#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.set %>% mutate(count=1) %>% group_by(REG) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])

#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:5]<-apply(as.matrix(bootsummary[,3:5]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary




##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR REGULATION FOR TRAWLING
##############################################################

## subset the data
NAM.trawling<- NAM.trawl %>%
  filter(Vessel_Activity=="Trawl")

#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######
prereg.heavy <- boot(NAM.trawling$heavy.rate[NAM.trawling$REG==0], samplemean, R=10000)
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)


#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######
postreg.heavy <- boot(NAM.trawling$heavy.rate[NAM.trawling$REG==1], samplemean, R=10000)
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)

#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.trawling %>% mutate(count=1) %>% group_by(REG) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])

#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:5]<-apply(as.matrix(bootsummary[,3:5]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary




##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR REGULATION FOR HAULING
##############################################################

## subset the data
NAM.haul<- NAM.trawl %>%
  filter(Vessel_Activity=="Haul")

#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######
prereg.heavy <- boot(NAM.haul$heavy.rate[NAM.haul$REG==0], samplemean, R=10000)
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)


#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######
postreg.heavy <- boot(NAM.haul$heavy.rate[NAM.haul$REG==1], samplemean, R=10000)
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)

#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.haul %>% mutate(count=1) %>% group_by(REG) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])

#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:5]<-apply(as.matrix(bootsummary[,3:5]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary



##############################################################
#### PRODUCE BOOTSTRAPPED CONFIDENCE INTERVALS FOR BSL FOR SETTING
##############################################################

## subset the data
NAM.set<- NAM.trawl %>%
  filter(Vessel_Activity=="Set")

#### SUMMARISE MEAN AND CI FOR PRE-REG SAMPLES #######
prereg.heavy <- boot(NAM.set$heavy.rate[NAM.set$Tori_line_deployed=="No"], samplemean, R=10000)
prereg.heavy.ci<-boot.ci(prereg.heavy,conf=0.95)


#### SUMMARISE MEAN AND CI FOR POST-REG SAMPLES #######
postreg.heavy <- boot(NAM.set$heavy.rate[NAM.set$Tori_line_deployed=="Yes"], samplemean, R=10000)
postreg.heavy.ci<-boot.ci(postreg.heavy,conf=0.95)

#### COMPILE OUTPUT INTO A SINGLE TABLE #######
## >80% reduction of seabird-cable interactions when tori lines are deployed

bootsummary<-NAM.set %>% mutate(count=1) %>% group_by(Tori_line_deployed) %>%
  summarise(nsets=sum(count))
bootsummary$boot.mean.heavy<-c(prereg.heavy$t0,postreg.heavy$t0)
bootsummary$boot.lcl.heavy<-c(prereg.heavy.ci$percent[1,4],postreg.heavy.ci$percent[1,4])
bootsummary$boot.ucl.heavy<-c(prereg.heavy.ci$percent[1,5],postreg.heavy.ci$percent[1,5])

#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1]-x[2])/x[1])*100}
bootsummary<- as.data.frame(bootsummary) %>% adorn_totals("row") 
bootsummary[3,3:5]<-apply(as.matrix(bootsummary[,3:5]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary



