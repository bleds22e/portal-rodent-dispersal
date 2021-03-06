# Looks at rodent movement
# EKB modified
# Jan 2018

library(dplyr)

source("movement_fxns.r")
source("additional_movement_fxns.r")

clean_rodents <- read.csv("rawdata/cleaned_1989-2009.csv", stringsAsFactors = FALSE)
small_rodents = filter(clean_rodents, species == "PB" | species == "PP") # check 0021PP

# treatment types have already been included:
#   treatment type 1 = controls: 1, 2, 4, 8, 9, 11, 12, 14, 17, 22
#   treatment type 2 = krat exclosures: 3, 6, 13, 15, 18, 19, 20, 21
#   treatment type 3 = full exclosures: 5, 7, 10, 16, 23, 24

# make sure text is text and not atomic
small_rodents$tag = as.character(small_rodents$tag) 
small_rodents$sex = as.character(small_rodents$sex)
small_rodents$species = as.character(small_rodents$species)

# give untagged indivs unique tag numbers (7 digits)
# small_rodents = id_unknowns(small_rodents)        

# get list of unique tags
tags = unique(small_rodents$tag)   

# output list of flagged data
flags = find_bad_spp_data(small_rodents, tags, 9)

# get list of unique "bad tags"
badtags=unique(flags$tag)

# delete bad tags from dataset for analysis
for (i in 1:length(badtags)) {
  small_rodents = subset(small_rodents, tag != badtags[i])
  }

# don't use periods with negative period numbers and periods with only one day of trapping
small_rodents = subset(small_rodents, period != 267 & period != 277 & period != 278 &
                                      period != 283 & period != 284 & period != 300 &
                                      period != 311 & period != 313 & period != 314 &
                                      period != 318 & period != 321 & period != 323 &
                                      period != 337 & period != 339 & period != 344 &
                                      period != 351 & period > 0)
  

### Create a set of capture histories by treatment and by plot
tags = unique(sort(small_rodents$tag))
periods = unique(sort(small_rodents$period))

mark_trmt = create_trmt_hist(small_rodents, tags, periods) 
mark_plot = create_plot_hist(small_rodents, tags, periods) 

# get list of indivs that moved plots or treatment, species is included
moving_rats = find_rats_that_move(small_rodents, tags)

# get rodents that move plots
moving_tags_plot = unique(moving_rats$tag)
outcount = 0
MARK_movers_plot = data.frame("ch"=1, "censored"=1, "tag"=1, "spp"=1, "sex"=1, "mass"=1)
for (i in 1:length(moving_tags_plot)) {
  mover_plot = subset(mark_plot, tags == moving_tags_plot[i])
  if (nrow(mover_plot) > 0) {
  outcount = outcount + 1
  MARK_movers_plot[outcount,] <- mover_plot
  }}   

# get rodents that move treatments
moving_tags_trmt = unique(moving_rats$tag)
outcount = 0
MARK_movers_trmt = data.frame("ch"=1, "censored"=1, "tag"=1, "spp"=1, "sex"=1, "mass"=1)
for (i in 1:length(moving_tags_trmt)) {
  mover_trmt = subset(mark_trmt, tags == moving_tags_trmt[i])
  if (nrow(mover_trmt) > 0) {
    outcount = outcount + 1
    MARK_movers_trmt[outcount,] <- mover_trmt
  }}  

#count non-movers
moving_tags_plot = unique(mark_plot$tag)
nonmover_plot = mark_plot
for (i in 1:length(moving_tags_plot)) {
  nonmover_plot = subset(nonmover_plot, tags != moving_tags_plot[i])
}

moving_tags_trmt = unique(mark_trmt$tag)
nonmover_trmt = mark_trmt
for (i in 1:length(moving_tags_trmt)) {
  nonmover_trmt = subset(nonmover_trmt, tags != moving_tags_trmt[i])
}

# find num captures/rat
rat_catches=num_captures(small_rodents, tags)

# plot captures to see if MARK is a good way to look at these
#pdf(file="output/PP_PB_recaptures.pdf",10,10)
#par(mfrow=c(1,1))

hist(rat_catches$captures, labels=TRUE, xlab = "# captures / rodent", ylab = "frequency",
      main = "Rodent recaptures", col = "orange3", breaks=31)

hist(rat_catches[which(rat_catches$spp=="PB"),4], labels=TRUE, xlab = "# captures / rodent", ylab = "frequency",
      main = "PB recaptures", col="plum4", breaks=31)
      
hist(rat_catches[which(rat_catches$spp=="PP"),4], labels=TRUE, xlab = "# captures / rodent", ylab = "frequency",
      main = "PP recaptures", col="cadetblue", breaks=18, xlim=c(0,20), ylim=c(0,2000))

dev.off()

# look at juveniles?

# how often are movers moving trmt?
trmt_moves = examine_trmt_moves(small_rodents, moving_tags_trmt) # only counts one movement, different issues (not only first move, not only B -> A or A -> B)
  trmt_moves = trmt_moves[which(trmt_moves$num_moves > 0),]
  trmt_moves$num_moves = as.numeric(trmt_moves$num_moves)
  trmt_moves$c2r = as.integer(trmt_moves$c2r)
  trmt_moves$r2c = as.numeric(trmt_moves$r2c)
  trmt_moves$c2e = as.numeric(trmt_moves$c2e)
  trmt_moves$r2e = as.numeric(trmt_moves$r2e)
  
plot_moves = examine_plot_moves(small_rodents, moving_tags_plot)

hist(trmt_moves[,7], labels = TRUE, xlab = "c2r")
hist(trmt_moves[,8], labels = TRUE, xlab = "r2c")
hist(trmt_moves[,9], labels = TRUE, xlab = "c2e")
hist(trmt_moves[,10], labels = TRUE, xlab = "r2e")

M = trmt_moves[which(trmt_moves[,3]=="M"),]
F = trmt_moves[which(trmt_moves[,3]=="F"),]

PB = trmt_moves[which(trmt_moves[,2]=="PB"),]
PP = trmt_moves[which(trmt_moves[,2]=="PP"),]

PBmale = subset(M, sp == "PB")
PBfemale = subset(F, sp == "PB")
PPmale = subset(M, sp == "PP")
PPfemale = subset(F, sp == "PP")

hist(PB[,7], labels=T, main = "C - R")
hist(PB[,8], labels=T, main = "R - C")
hist(PP[,7], labels=T, main = "C - R")
hist(PP[,8], labels=T, main = "R - C")

