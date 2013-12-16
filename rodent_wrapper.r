# Code for working with individual-level rodent data
# movement with stakes

library(reshape2)
library(ggplot2)
library(calibrate)
library(fields)
library(stringr)
library(plyr)

#---------------------------------------------------------------------------------
#          setup - select wd, import data, source code,  file to collect results
#---------------------------------------------------------------------------------
#set working directory
wd = "/Users/sarah/Documents/GitHub/portal-rodent-dispersal"
wd = "C:/Users/sarah/Documents/GitHub/portal-rodent-dispersal"
setwd(wd)
source("movement_fxns.R")

#import all data
all = read.table('rawdata/all_1980-2009.txt', sep = ',', header = TRUE)

#import cleaned data
allclean = read.csv('rawdata/cleaned_1989-2009.csv', sep = ',', header = TRUE)
  #all7 == allclean, if have allclean, can skip the datacleaning steps
  all7=allclean

#---------------------------------------------------------------------------------
#          clean up the data -- skip this section if allclean exists
#---------------------------------------------------------------------------------

# change some cols from factor to character class
all$tag = as.character(all$tag)
all$species = as.character(all$species)
all$sex = as.character(all$sex)

allmonths = count_months(all, c(1980:2009))

#subset data where species are known (e.g., no "unidentified rodents" or genus-only)
all2 = subset(all, species!="DX" & species!="UR" & species!="RX" & species!="SX" & species!="PX" & species != "OX")

# give untagged individuals a unique 7-number code
all2 = id_unknowns(all2, 16)

# make sure when note2 == "*" it is counted as a new tag
# necessary if using ALL data (ear and toe tags)
# returns the dataset with new IDs for checking for duplicate tags that occur over a suspiciously long time period
tags = unique(all2$tag)
all3 = starred_tags(all2, tags, 9, 16)

#check for dead individuals, give all with same tag after marked dead a new unique ID
tags = unique(all3$tag)
all4 = is_dead(all3, tags, 9, 16) #get warnings, but it seems to work - not sure what they are indicating?

# check for individuals with same tag, but captured over long timespan (may be able to separate them) 
# necessary if using ALL data (ear and toe tags)
# returns the dataset with new IDs for those that can easily be sorted out based on sex and year
tags = unique(all4$tag)
dups = is_duplicate_tag(all4, tags, 10, 9, 16) #check to see if can separate tags based

#eliminate bad data based on tags identified in dups$bad
duptags = unique(dups$bad$tag)
all5 = dups$data[-which(dups$data$tag %in% duptags),] #delete rows flagged as duplicates without clear resolution

# get rid of 'bad data'; deletes data where species is inconsistent. 
all6 = subsetDat(all5)

#subset data for years of analysis
all7 = subset(all6, yr > 1988)


#---------------------------------------------------------------------------------
#          calculate life-history details - temporal persistence, reproduction
#---------------------------------------------------------------------------------
spplist = unique(all7$species)
totalyears = c(1989:2009) #will need to be adjusted based on time frame want to use
controlplots = c(1,2,4,8,9,11,12,14,17,22)
months = count_months(all7, totalyears)

persistence = data.frame(species=NA, propyrs=NA, propmos=NA, meanabun=NA, maxabun=NA)
  outcount = 1
yearly_control_abundance = data.frame(year = totalyears)
reprod_mo_yr = data.frame(year=NA, month=NA, proprepro=NA, numfemales=NA, species=NA)
avg_mo_reprod = data.frame(species=NA, month=NA, proprepro=NA)

for (i in 1:length(spplist)){
  #subset species data
  spdata = subset(all7, species == spplist[i])
  
    # proportion of years they were seen in CONTROL plots
    conspdata = subset(spdata, plot %in% controlplots)
    conpropyrs = length(unique(conspdata$yr))/length(totalyears)
    # average proportion of months they were seen in during years in which they were present on CONTROLS
    conavgmos = mean_win_yr_occ(conspdata, totalyears, months)
    # number of unique individuals in each year on control plots only
    conabun = allyrs_abun(conspdata, totalyears)
    # mean number of unique individuals on control plots only, includes ZEROES
    meanconabun = mean(conabun)
    # max number of unique individuals on control plots only
    maxconabun = max(conabun)
  
  #record persistence and abundance data from control plots
  persistence[outcount,] = c(as.character(spplist[i]), round(conpropyrs,4), round(conavgmos,4), round(meanconabun,4), maxconabun)
  yearly_control_abundance = cbind(yearly_control_abundance, conabun)
  outcount = outcount + 1
  
  #subset females for each species
  spdataF = subset(spdata, sex == "F")
  
    #average proportion of reproductive females by month across all years
    reprd = mean_mo_repro(spdataF, totalyears) #vector with 12 items (one for each month)
      avg_mo_reprod = rbind(avg_mo_reprod, reprd)    
  
  # proportion of reproductive females by month and year
    reprdyr = mo_repro(spdataF) #matrix with 120 x 5
      reprod_mo_yr = rbind(reprod_mo_yr, reprdyr)
  
    # track the number of times individual females uniquely reproduce within each year
    irep = indiv_repro(spdataF) #matix with cols "tag", "year" and "num_reprod"
      assign(paste0(spplist[i], 'irep'), irep)
}

#add species names to the dataframe of abundance vectors
names(yearly_control_abundance) = c("year", as.character(spplist))
#make sure cols are numeric
persistence$propyrs = as.numeric(persistence$propyrs)
persistence$propmos = as.numeric(persistence$propmos)
persistence$meanabun = as.numeric(persistence$meanabun)
persistence$maxabun = as.numeric(persistence$maxabun)
avg_mo_reprod$proprepro = as.numeric(avg_mo_reprod$proprepro)
avg_mo_reprod$month = as.numeric(avg_mo_reprod$month)
#delete Null rows
avg_mo_reprod = avg_mo_reprod[-1,]
reprod_mo_yr = reprod_mo_yr[-1,]


#melt control abundance data frame for later plotting
yrcontrolabuns = melt(yearly_control_abundance, id.vars=c("year"))
names(yrcontrolabuns) = c("year", "species", "abun")


#Identify Core species based on annual persistence, following Coyle et al. 2013 (American Naturalist)
corespecies = persistence[which(persistence$propyrs >= 0.666),1]
intermediatespecies = persistence[which(persistence$propyrs > 0.333 & persistence$propyrs < 0.666),1]
transientspecies = persistence[which(persistence$propyrs <= 0.333),1]

#Categorize species by feeding guild
granivores = c("DO", "DM", "DS", "PB", "PP", "PF", "PH", "PI",
               "PE", "PM", "PL", "RM", "RF", "RO", "BA")
folivores = c("SH", "SF", "SO", "NAO")
carnivores = c("OT", "OL")

#add columns to persistence (for later plotting) for status and guild
persistence$guild = rep(NA, nrow(persistence))
persistence$status = rep(NA, nrow(persistence))

for (row in 1:nrow(persistence)){
  if (persistence[row,]$species %in% granivores){ persistence[row,]$guild = "granivore" }
  else if (persistence[row,]$species %in% folivores) { persistence[row,]$guild = "folivore" }
  else { persistence[row,]$guild = "carnivore" }

  if (persistence[row,]$species %in% corespecies){ persistence[row,]$status = "core" }
  else if (persistence[row,]$species %in% intermediatespecies) { persistence[row,]$status = "intermediate" }
  else { persistence[row,]$status = "transient" }
}

#add status and guild to yrcontrolabuns
yrcontrolabuns$guild = rep(NA, nrow(yrcontrolabuns))
yrcontrolabuns$status = rep(NA, nrow(yrcontrolabuns))

for (row in 1:nrow(yrcontrolabuns)){
  if (yrcontrolabuns[row,]$species %in% granivores){ yrcontrolabuns[row,]$guild = "granivore" }
  else if (yrcontrolabuns[row,]$species %in% folivores) { yrcontrolabuns[row,]$guild = "folivore" }
  else { yrcontrolabuns[row,]$guild = "carnivore" }
  
  if (yrcontrolabuns[row,]$species %in% corespecies){ yrcontrolabuns[row,]$status = "core" }
  else if (yrcontrolabuns[row,]$species %in% intermediatespecies) { yrcontrolabuns[row,]$status = "intermediate" }
  else { yrcontrolabuns[row,]$status = "transient" }
}


#---------------------------------------------------------------------------------
#          calculate movement distances, multi-state capture histories
#---------------------------------------------------------------------------------

#set names for the distance list
spnames = as.character(spplist)

#create lists with sublist names reflecting each species
meterlist = sapply(spnames,function(x) NULL)
taglist = sapply(spnames,function(x) NULL)


for (i in 1:length(spplist)){
  #subset species data
  spdata = subset(all7, species == spplist[i])
  
    # get a vector unique tags, then get a vector of distances moved for all recaptured individuals, by SPECIES
    tags = unique(spdata$tag)
    mtrs = distance_moved(spdata, tags)
    meterlist[i] = list(mtrs)
    taglist[i] = list(tags)
}

#get a list of all the new distance vectors
meterlist = ls(pattern = "meters")


#---------------------------------- From this point on, the analysis will be separated by foraging guild
#------------------------ Granivores, Folivores, and Carnivores

# concatenate core guild data - used to ask if these species behave differently from others
# Check that the definition of "core" is still the same, need to change this chunk of code by hand, if necessary
coremeters = meterlist[which(names(meterlist) %in% corespecies]
coregran = coremeters[which(names(coremeters) %in% granivores]  #  c(DMmeters, DOmeters, PBmeters, PPmeters, PFmeters, PEmeters, RMmeters)
corefoli = coremeters[which(names(coremeters) %in% folivores]
corecarn = coremeters[which(names(coremeters) %in% carnivores]

  # find breakpoints to use in MARK data structure for future analyses
  # data reasonably well fits a lognormal distribution (eyeball and J. Powell)
  # breakpoint = mean(logdata) + sd(logdata) of all the distances traveled by recaptured individuals    
  # using log1p, and back transforming using expm1 should solve the problem of lots of zeros 
  coregran_brkpt = expm1(mean(log1p(coregran)) + sd(log1p(coregran)))
  corefoli_brkpt = expm1(mean(log1p(corefoli)) + sd(log1p(corefoli)))
  corecarn_brkpt = expm1(mean(log1p(corecarn)) + sd(log1p(corecarn)))


distances = ls(pattern = "*meters") #see all the vectors

graniv_dist = list(PMmeters, DMmeters, RMmeters, RFmeters, PEmeters, DSmeters, DOmeters, PPmeters,
                   PFmeters, BAmeters, PHmeters, ROmeters, PImeters, PLmeters, PBmeters)

#make a new matrix with the breakpoint for each species
brkpt_out<-sapply(graniv_dist,function(x){
  expm1(mean(log1p(x)) + sd(log1p(x)))
})

mode_out <-sapply(graniv_dist,function(x){
  as.numeric(names(sort(-table(x)))[1])
})

mean_out <-sapply(graniv_dist,function(x){
  as.numeric(mean(x))
})

max_out <-sapply(graniv_dist,function(x){
  as.numeric(max(x))
})

#concatenate data for granivores only, and 
#add in the transition, modal, mean, and max distances traveled by each species for later plotting
graniv_persist = persistence[which(persistence$species %in% granivores),] 
granivdata = cbind(graniv_persist, brkpt_out, mode_out, mean_out, max_out)
granivdata$oneval = granivdata$propyrs * granivdata$propmos


#TODO: fix this chunk of code so that it is not so repetitive
#----------------------------------------------------------------------
#             Get MARK capture histories for granivores
#----------------------------------------------------------------------
spplist = c("DM", "DO", "DS", "PB", "PP", "PF", "PM", 
            "RM", "RF", "PE", "BA", "PH", "RO", "PI", "PL")
periods = c(130:380) # all sampling periods 1989-2009
all_excl = c(5, 7, 10, 16, 23, 24) 
krat_excl = c(5, 7, 10, 16, 23, 24, 3, 6, 13, 15, 18, 19, 20, 21)

for (i in 1:length(spplist)){
  
  #subset species data, for each species in turn
  spdata = subset(all7, species == spplist[i])
  
  if (spplist[i] %in% list("DM", "DS", "DO")) { exclosures = krat_excl} 
  else { exclosures = all_excl} 
  
  #the first species begins the new data matrix for MARK
  if (i == 1) {
  MARK = noplacelikehome(spdata, periods, exclosures, coregran_brkpt)
  }
  
  #all subsequent species are appended onto the end of the existing MARK data matrix
  else {
  nextMARK = noplacelikehome(spdata, periods, exclosures, coregran_brkpt)
  MARK = rbind(MARK, nextMARK)
  }
}

write.table(MARK, file = "mark_datafiles//gran_mark.inp", row.names = F, col.names = F, quote = F)


#--------------- Get MARK capture histories for folivores
#------------------------------
spplist = c("NAO", "SH", "SF", "SO")
periods = c(130:380) #1989-2009
all_excl = c(5, 7, 10, 16, 23, 24) 

for (i in 1:length(spplist)){
  
  #subset species data, for each species in turn
  spdata = subset(all7, species == spplist[i])
   exclosures = all_excl
  
  #the first species begins the new data matrix for MARK
  if (i == 1) {
    MARK = noplacelikehome(spdata, periods, exclosures, corefoli_brkpt)
  }
  
  #all subsequent species are appended onto the end of the existing MARK data matrix
  else {
    nextMARK = noplacelikehome(spdata, periods, exclosures, corefoli_brkpt)
    MARK = rbind(MARK, nextMARK)
  }
}

write.table(MARK, file = "mark_datafiles//foli_mark.inp", row.names = F, col.names = F, quote = F)


#-----------------Get MARK capture histories for carnivores
#------------------------------
spplist = c("OT", "OL")
periods = c(130:380) #1989-2009
all_excl = c(5, 7, 10, 16, 23, 24) 

for (i in 1:length(spplist)){
  
  #subset species data, for each species in turn
  spdata = subset(all7, species == spplist[i])
  exclosures = all_excl
  
  #the first species begins the new data matrix for MARK
  if (i == 1) {
    MARK = noplacelikehome(spdata, periods, exclosures, corecarn_brkpt)
  }
  
  #all subsequent species are appended onto the end of the existing MARK data matrix
  else {
    nextMARK = noplacelikehome(spdata, periods, exclosures, corecarn_brkpt)
    MARK = rbind(MARK, nextMARK)
  }
}

write.table(MARK, file = "mark_datafiles//carn_mark.inp", row.names = F, col.names = F, quote = F)


#-----------------------------------------------------------------------------------
#        Make a table of total capture/recapture and gaps in data for each species
#-----------------------------------------------------------------------------------
#right now, this code only uses the most recently generated MARK data table 
spplist = unique(MARK[,7])
TableDat = data.frame("species"=1, "numindiv"=1, "numindivrecap"=1, "nocaps"=1, "norecaps"=1)

for (i in 1:length(spplist)){
  
  #subset species data, for each species in turn
  spdata = MARK[which(MARK[,7]==spplist[i]),]
  
  sp = spplist[i]
  numindiv = nrow(spdata)
  
  #break MARK data out into a list
  list.all = list()
  for (row in 1:nrow(spdata)){
    dat = spdata[row,1]
    list.all[row] = dat
  }
  
  #make a new matrix with each period as a value
  out<-t(sapply(list.all,function(x){
    as.numeric(as.vector(strsplit(x,"")[[1]]))
  }))
  
  #For each row find the sum things that are not 0
  recaps<-apply(out,1,function(x){
    length(which(!x %in% 0))-1
  })
  
  numinds_recaps = length(recaps[recaps>0])
  
  #count the num of captures per period
  caps = apply(out,2,function(x) {
    length(which(!x %in% 0))
  })

  #count the num of recaptures per period
  recaps2 = apply(out,2,function(x) {
    length(which(x %in% 2))
  })
  
  nummos_nocaps = length(caps[caps == 0])
  nummos_norecaps = length(recaps2[recaps2 == 0])
  
  results = c(sp, numindiv, numinds_recaps, nummos_nocaps, nummos_norecaps)
  TableDat[i,] <- results
}

print(TableDat)


#---------------------------------------------------------------------------------
#          plot results
#---------------------------------------------------------------------------------
#----------------------------- plot abundance vs. years, ala core v. transient literature

ggplot(persistence, aes(propyrs, propmos)) + geom_point(aes(size = meanabun, col=guild)) + 
  theme_bw() + xlab("proportion years present") +
  ylab("proportion of months present") + xlim(0,1) + ylim(0,1) + 
  geom_vline(xintercept=0.66, linetype="dotted", col = "red", size = 1.5) + 
#  geom_hline(yintercept=0.66, linetype="dotted", col = "red", size = 1.5) +
  geom_vline(xintercept=0.33, linetype="dotted", col = "red", size = 1.5) + 
#  geom_hline(yintercept=0.33, linetype="dotted", col = "red", size = 1.5) +
  ggtitle("Rodents 1989 - 2009") + geom_text(aes(label = species), hjust=0, vjust=0)

#------------------------- plot abundance for all species across timeseries
ggplot(yrcontrolabuns, aes(x=year, y=abun, group=species)) + 
#  geom_point(aes(col=status, pch=guild)) + 
  geom_line(aes(col=status), size=1.5) + theme_bw()

#------------------------- plot monthly reproduction
ggplot(avg_mo_reprod, aes(month, proprepro)) + geom_point() + 
  geom_line() + facet_wrap(~species)

#------------------------- plot meters traveled by all species
#plot modal distance by persistence for all granivores, color code points by status
ggplot(granivdata, aes(propyrs, mode_out)) + 
  geom_point(aes(col=as.factor(status), size = 3, pch = as.factor(status))) + theme_bw() + 
  xlab("proportion of years present") + ylab("Modal Distance between trap locations") +
  geom_text(aes(label=species), hjust=0, vjust=0)


#------------------------- attempting to build up to making bean plots
# ggplot(data=data.frame(value=DOmeters)) +
#   stat_density(aes(x=value)) +
#   geom_segment(aes(x=value,xend=value),y=0,yend=0.0025,col='white')
# 
# ggplot(data=data.frame(value=DOmeters))+
#   geom_boxplot(aes(x="DO", y=value))
# 
# ggplot(df)+
#   geom_violin(aes(x=species,y=distance),fill='grey',trim=F)
# +
#   geom_segment(aes(
#     x=match(Distribution,levels(Distribution))-0.1,
#     xend=match(Distribution,levels(Distribution))+0.1,
#     y=Value,yend=Value),
#                col='black')




#------------------------------------------ FIGURE - for ESA talk

#plot the relative abundance of females who represent each number of reproductive events per year
plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "core granivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:3), table(DOirep$num_reprod)/sum(table(DOirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(DMirep$num_reprod)/sum(table(DMirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(PBirep$num_reprod)/sum(table(PBirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(PPirep$num_reprod)/sum(table(PPirep$num_reprod)), type = "b", pch = 29, col = "black")

plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "transient granivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:2), table(PMirep$num_reprod)/sum(table(PMirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:2), table(PEirep$num_reprod)/sum(table(PEirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(PLirep$num_reprod)/sum(table(PLirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:2), table(PFirep$num_reprod)/sum(table(PFirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(RMirep$num_reprod)/sum(table(RMirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(ROirep$num_reprod)/sum(table(ROirep$num_reprod)), type = "b", pch = 15, col = "black")
points(0, table(RFirep$num_reprod)/sum(table(RFirep$num_reprod)), type = "b", pch = 15, col = "black")
points(0, table(DSirep$num_reprod)/sum(table(DSirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(PIirep$num_reprod)/sum(table(PIirep$num_reprod)), type = "b", pch = 15, col = "black")


#------------------------------------------ FIGURE - for ESA talk 
#                                 REQUIRES DATA FROM MARK ANALYSES!
#             #FIXME: Should be moved since it depends on other results and is thus out of order

#granivore data from dissertation chapter table 2-3
Psi = c(0.09, 0.13, 0.08, 0.14, 0.48, 0.23, 0.49, 0.41)
S = c(0.76, 0.78, 0.80, 0.81, 0.67, 0.81, 0.76, 0.62)
distbench = c(28.36, 32.64, 25.57, 36.22, 93.05, 74.29, 29.12, 93.30)
littersize = c(2.37, 2, 3.6, 4.72, 2.53, 3.6, 4, 4.29) #from Mammals of Arizona - Hoffmeister

lm1 = lm(S~Psi)
lm2 = lm(S~distbench)
lm3 = lm(littersize~S)
lm4 = lm(littersize~distbench)

# S vs. Psi
plot(Psi, S, pch = 19, xlim = c(0:1), ylim = c(0:1), bty = "n", cex.axis = 1.5, cex.lab = 1.5, cex = 1.5,
     ylab = "survival probability", xlab = "long distance movement probability")
    abline(lm1, col = "turquoise")
# S vs. distance benchmark
plot(distbench, S, pch = 19, bty = "n", xlab = "transition distance (meters)", ylab = "survival", 
     xlim = c(0,100), ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, cex = 1.5)
    abline(lm2, col = "turquoise")
# littersize vs. S
plot(S, littersize, pch = 19, bty = "n", xlab = "survival probability", ylab = "mean litter size",
     xlim = c(0,1), ylim = c(1,6))
#littersize vs. distance benchmark
plot(distbench, littersize, pch = 19)

  