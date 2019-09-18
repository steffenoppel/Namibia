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




