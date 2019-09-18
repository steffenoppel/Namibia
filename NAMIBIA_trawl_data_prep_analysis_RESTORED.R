### ##########################################
###
### Namibia bycatch analysis
### output figure
###
### ##########################################

### written by Steffen Oppel 27 August 2019


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
filter<-dplyr::filter
select<-dplyr::select



##############################################################
#### LOAD ALL NECESSARY DATASETS
##############################################################
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\Namibia")

# read in model output summaries

trawl<-fread("Namibia_trawl_interactions_REG_comparison.csv")
longline<-fread("Namibia_longline_bycatch_REG_comparison.csv")
trawlBSL<-fread("Namibia_trawl_interactions_BSL_comparison.csv")




##############################################################
#### MANIPULATE THE DATA 
##############################################################


plotdat<-trawl[1:2,c(1,3:5)] %>%
  rename(Regulation=REG, mean=boot.mean, lcl=boot.lcl,ucl=boot.ucl) %>%
  mutate(Regulation=ifelse(Regulation==0,"BEFORE","AFTER")) %>%
  mutate(Fishery="Trawl") %>%
  bind_rows(longline[1:2,c(1,3:5)] %>%
              mutate(Fishery="Longline")) %>%
  mutate(Regulation=ifelse(Regulation=="BEFORE","Before regulation","With regulation")) %>%
  mutate(ylab=c(5.2,5.2,1.4,1.4),xlab=c(2.5,2.5,2.5,2.5))




##############################################################
#### CREATE FIGURE FOR MANUSCRIPT 
##############################################################

#### PLOT OUTPUT ####
ggplot(plotdat, aes(x=Regulation, y=mean)) +
	geom_point(size=2) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  facet_wrap(~Fishery, scales="free_y", ncol=1,
             strip.position = "left", 
             labeller = as_labeller(c(Trawl = "cable interactions/hour", Longline = "dead birds/1000 hooks")))  +
  ylab(NULL) +
  xlab("") +

  ## insert manual facet labels
  geom_text(aes(label = Fishery, x=xlab, y=ylab), vjust=0.5, hjust = 1, size=7) +
  
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=18, color="black"), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(margin=margin(0,20,0,0)), 
        #strip.background=element_rect(fill="white", colour="black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title=element_text(size=18))




############ RECOVERED #########

##############################################################
# SUMMARISE OBSERVATION EFFORT PER TRAWL
obs.effort <- dat %>%
  group_by(OBSTRAWL_ID,OBSPERIOD_ID,
           Offal_during_observation,
           Tori_line_deployed,
           Vessel_Activity,
           Date_Start_Observation,REG) %>%
  summarize(obs.effort = sum(h.lub), tot.interactions=sum(Number_interactions)) %>%
  filter(obs.effort>0) %>%
  mutate(Interaction_rate = tot.interactions / obs.effort)
tail(obs.effort)
# SUMMARISE ALL FATAL INTERACTIONS
dat.mort <- dat %>%
  filter(Outcome %in% c("Dead","Possibly Dead")) %>%
  group_by(OBSPERIOD_ID) %>%
  summarize(fatal.interactions = sum(Number_interactions))
dat.mort
# SUMMARISE ALL HEAVY INTERACTIONS
dat.heavy <- dat %>%
  filter(Interaction_type=="Heavy") %>%
  group_by(OBSPERIOD_ID) %>%
  summarize(heavy.interactions = sum(Number_interactions))
dat.heavy
# COMBINE DATA TO SUMMARISE BY SETTING OPERATION ETC
NAM.trawl<- left_join(obs.effort,dat.mort, by="OBSPERIOD_ID", fill=0) %>%
  left_join(dat.heavy, by="OBSPERIOD_ID", fill=0) %>%
  filter(!is.na(Tori_line_deployed)) %>%
  mutate(fatal.interactions=ifelse(is.na(fatal.interactions),0,fatal.interactions)) %>%
  mutate(fatal.rate = fatal.interactions/obs.effort) %>%
  mutate(heavy.interactions=ifelse(is.na(heavy.interactions),0,heavy.interactions)) %>%
  mutate(heavy.rate = heavy.interactions/obs.effort)
dim(NAM.trawl)
fwrite(NAM.trawl,"Namibia.trawl.interactions.csv")
##############################################################
#### BASIC SUMMARY OF DATA FOR REPORTING IN MANUSCRIPT
##############################################################
# BASIC SUMMARY OF DATA
rawsummary<-NAM.trawl %>% mutate(count=1) %>% group_by(REG,Tori_line_deployed,Vessel_Activity) %>%
  summarise(nsets=sum(count),mean.tot.rate=mean(Interaction_rate), sd.tot.rate=sd(Interaction_rate),mean.fatal.rate=mean(fatal.rate), sd.fatal.rate=sd(fatal.rate),mean.heavy.rate=mean(heavy.rate), sd.heavy.rate=sd(heavy.rate))
rawsummary
dat %>%
  filter(!is.na(Tori_line_deployed)) %>%
  group_by(REG) %>%
  summarise(ntrips=length(unique(OBSTRIP_ID)),nsets=length(unique(OBSTRAWL_ID)),eff=sum(h.lub,na.rm=T))
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
bootsummary
fwrite(bootsummary,"Namibia.trawl.interactions_bootstrap.summary.REGULATION.csv")
#### PLOT OUTPUT ####
ggplot(bootsummary, aes(x=Tori_line_deployed, y=boot.mean)) +
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
bootsummary
fwrite(bootsummary,"Namibia.trawl.interactions_bootstrap.summary.csv")
#### PLOT OUTPUT ####
ggplot(bootsummary, aes(x=Tori_line_deployed, y=boot.mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=boot.lcl, ymax=boot.ucl), width=.1)+
  ## format axis ticks
  scale_y_continuous(name="N seabird-cable interactions per hour", limits=c(0,8), breaks=seq(0,8,2), labels=seq(0,8,2))+
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
bootsummary
fwrite(bootsummary,"Namibia.trawl.interactions_bootstrap.summary.REGULATION.csv")
#### PLOT OUTPUT ####
ggplot(bootsummary, aes(x=Tori_line_deployed, y=boot.mean)) +
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
bootsummary
percchange<-function(x){(x[1,]-x[2,]/x[1,])*100}
apply(bootsummary,2,percchange)
?apply(array, margin, ...)
percchange(bootsummary[,3])
#### CALCULATE THE CHANGE IN INTERACTION RATE ####
percchange<-function(x){((x[1,]-x[2,])/x[1,])*100}
percchange(bootsummary[,3])
apply(bootsummary,2,percchange)
apply(bootsummary,1,percchange)
apply(as.matrix(bootsummary[,3:11],2,percchange)
      ?apply)
apply(as.matrix(bootsummary[,3:11]),2,percchange)
percchange<-function(x){((x[1]-x[2])/x[1])*100}
apply(as.matrix(bootsummary[,3:11]),2,percchange)
dat.mort
rawsummary
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
bootsummary
percchange<-function(x){((x[1]-x[2])/x[1])*100}
apply(as.matrix(bootsummary[,3:11]),2,percchange)
NAM.trawl %>% filter(Outcome!="Unknown") %>%
  mutate(count=1) %>%
  group_by(Outcome) %>%
  summarise(n=sum(count))
NAM.trawl
dat %>% filter(Outcome!="Unknown") %>%
  mutate(count=1) %>%
  group_by(Outcome) %>%
  summarise(n=sum(count))
14/460
names(dat)
table(dat$Interaction_type)   ## 1260/dim(dat)[1] = 73% of observations are n/a
dat %>% filter(Interaction_type!="n/a") %>%
  mutate(count=1) %>%
  group_by(Interaction_type) %>%
  summarise(n=sum(count))
dat %>% filter(Interaction_type!="n/a") %>%
  mutate(count=1) %>%
  #group_by(Interaction_type) %>%
  summarise(n=sum(count))
75/465
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
bootsummary
percchange<-function(x){((x[1]-x[2])/x[1])*100}
apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary
bootsummary[3,1]<-"CHANGE(%)"
bootsummary
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
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary
fwrite(bootsummary,"Namibia_trawl_interactions_BSL_comparison.csv")
#### PLOT OUTPUT ####
ggplot(bootsummary, aes(x=Tori_line_deployed, y=boot.mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=boot.lcl, ymax=boot.ucl), width=.1)+
  ## format axis ticks
  scale_y_continuous(name="N seabird-cable interactions per hour", limits=c(0,8), breaks=seq(0,8,2), labels=seq(0,8,2))+
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
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary
fwrite(bootsummary,"Namibia_trawl_interactions_BSL_comparison.csv")
#### PLOT OUTPUT ####
ggplot(bootsummary, aes(x=Tori_line_deployed, y=boot.mean)) +
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
bootsummary
fwrite(bootsummary,"Namibia_trawl_interactions_REG_comparison.csv")
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
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary
fwrite(bootsummary,"Namibia_trawl_interactions_BSL_comparison.csv")
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
bootsummary[3,3:11]<-apply(as.matrix(bootsummary[,3:11]),2,percchange)
bootsummary[3,1]<-"CHANGE(%)"
bootsummary
fwrite(bootsummary,"Namibia_trawl_interactions_REG_comparison.csv")



