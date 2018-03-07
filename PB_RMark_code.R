# Run MARK on different chunks of time
# EKB, code modified from S. Supp and RMark examples
# March 6, 2018

# LIBRARIES
library(dplyr)
library(RCurl)

# DATA FILES

# rodent file from repo
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv") 
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# trapping file from repo


#---------------------------------------------------------
# Figure out PB "burn in" period
#---------------------------------------------------------

# read in data
all = read.table('rawdata/all_1980-2009.txt', sep = ',', header = TRUE)

# get PB per plot per period
PB <- all %>% filter(species == 'PB') 
PB_plot_count <- PB %>% 
  select(period, Treatment_Number, plot) %>% 
  group_by(period, Treatment_Number) %>% 
  summarise(count = n_distinct(plot))

PB_min <- min(PB$period) # when PB first show up
PB_max <- min(PB_plot_count$period[PB_plot_count$count == 8]) #first time PBs are found in all 8 krat exclosures

#---------------------------------------------------------
# Create Capture Histories for PP
#---------------------------------------------------------

# mostly take from Sarah's rodent_data.r

# call sources
source("movement_fxns.r")
source("additional_movement_fxns.r")
source("additional_fxns_EKB.r")

# get rodent file from repo and read it in
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv") 
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# make it match Sarah Supp's data to use her code
all <- repo_data_to_Supp_data(rdat)

# select on PPs from the data and use Sarah's code to clean
PP_only <- filter(all, species == 'PP')
PP_only <- clean_data_for_capture_histories(PP_only)

# don't use periods with negative period numbers and periods with only one day of trapping
PP_only = subset(PP_only, period != 267 & period != 277 & period != 278 &
                         period != 283 & period != 284 & period != 300 &
                         period != 311 & period != 313 & period != 314 &
                         period != 318 & period != 321 & period != 323 &
                         period != 337 & period != 339 & period != 344 &
                         period != 351 & period > 0)


### Create a set of capture histories by treatment and by plot
tags = unique(sort(PP_only$tag))
periods = unique(sort(PP_only$period))

# make two different capture histories based on PB_max
