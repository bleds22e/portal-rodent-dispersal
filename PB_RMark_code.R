# Run MARK on different chunks of time
# EKB, code modified from S. Supp and RMark examples
# March 7, 2018

# LIBRARIES
library(dplyr)
library(RCurl)

# DATA FILES

# rodent file from repo
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv") 
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# trapping file from repo
trapping <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv")
tdat <- read.csv(text = trapping, header = TRUE, stringsAsFactors = FALSE)

#---------------------------------------------------------
# Clean the Data 
#---------------------------------------------------------

# mostly taken from Sarah's rodent_data.r

# call sources
source("movement_fxns.r")
source("additional_movement_fxns.r")
source("additional_fxns_EKB.r")

# Clean the Rodent Data

# make it match Sarah Supp's data to use her code
all <- repo_data_to_Supp_data(rdat)

# remove bad or unclear data
all_clean <- clean_data_for_capture_histories(all)

# Find and Remove Periods with One Day of Trapping

# summarize trapping
trap_count <- tdat %>% 
  group_by(period) %>% 
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 24)
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_clean = all_clean[-which(all_clean$period %in% bad_periods),]

# select on PPs from the data and use Sarah's code to clean
PP_only <- filter(all_clean, species == 'PP')

#---------------------------------------------------------
# Figure out PB "burn in" period
#---------------------------------------------------------

# get PB per plot per period
PB <- all_clean %>% filter(species == 'PB') 
PB_plot_count <- PB %>% 
  select(period, Treatment_Number, plot) %>% 
  group_by(period, Treatment_Number) %>% 
  summarise(count = n_distinct(plot))

PB_min <- min(PB$period) # when PB first show up
PB_max <- min(PB_plot_count$period[PB_plot_count$count == 8]) #first time PBs are found in all 8 krat exclosures

#-----------------------------------------------------------
# Create Capture Histories
#-----------------------------------------------------------

# make two different capture histories based on PB_max
pre_PB_max <- PP_only[(PP_only$period < 233),]
post_PB_max <- PP_only[(PP_only$period >= 233),]

### Create a set of capture histories by treatment and by plot
tags_pre = unique(sort(PP_only$tag))
periods_pre = unique(sort(pre_PB_max$period))
periods_post = unique(sort(post_PB_max$period))

mark_trmt_pre = create_trmt_hist(pre_PB_max, tags, periods_pre)
mark_trmt_post = create_trmt_hist(post_PB_max, tags, periods_post)
