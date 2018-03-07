# Run MARK on different chunks of time
# EKB, code modified from S. Supp and RMark examples
# March 6, 2018

# LIBRARIES
library(dplyr)

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

# same as rodent_data.r

# call 
source("movement_fxns.r")
source("additional_movement_fxns.r")

clean_rodents <- read.csv("rawdata/cleaned_1989-2009.csv", stringsAsFactors = FALSE)
PP_only = filter(clean_rodents, species == "PP") 

# treatment types have already been included:
#   treatment type 1 = controls: 1, 2, 4, 8, 9, 11, 12, 14, 17, 22
#   treatment type 2 = krat exclosures: 3, 6, 13, 15, 18, 19, 20, 21
#   treatment type 3 = full exclosures: 5, 7, 10, 16, 23, 24

# make sure text is text and not atomic
PP_only$tag = as.character(small_rodents$tag) 
PP_only$sex = as.character(small_rodents$sex)
PP_only$species = as.character(small_rodents$species)

# give untagged indivs unique tag numbers (7 digits)
# small_rodents = id_unknowns(small_rodents)        

# get list of unique tags
tags = unique(PP_only$tag)   

# output list of flagged data
flags = find_bad_spp_data(PP_only, tags, 9)

# get list of unique "bad tags"
badtags=unique(flags$tag)

# delete bad tags from dataset for analysis
for (i in 1:length(badtags)) {
  PP_only = subset(PP_only, tag != badtags[i])
}

# don't use periods with negative period numbers and periods with only one day of trapping
small_rodents = subset(PP_only, period != 267 & period != 277 & period != 278 &
                         period != 283 & period != 284 & period != 300 &
                         period != 311 & period != 313 & period != 314 &
                         period != 318 & period != 321 & period != 323 &
                         period != 337 & period != 339 & period != 344 &
                         period != 351 & period > 0)


### Create a set of capture histories by treatment and by plot
tags = unique(sort(PP_only$tag))
periods = unique(sort(PP_only$period))

# make two different capture histories based on PB_max
