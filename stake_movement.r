# Code for working with individual-level rodent data
# movement with stakes

library(calibrate)

wd = "C://Users//sarah//Desktop//Dropbox//Active Research Projects//Rodent Movement"
setwd(wd)
source("C://Users//sarah//Documents//GitHub//portal-rodent-dispersal//movement_fxns.R")

heteros = read.csv("PF-PP-PB_2000-2009_w_stakes.csv") #hetero = heteromyid
PE = read.csv('PE2000_2009.csv')
  PE$tag = as.character(PE$tag)
  PE$species = as.character(PE$species)
PM = read.csv('PM2000_2009.csv')
  PM$tag = as.character(PM$tag)
  PM$species = as.character(PM$species)
OT = read.csv('OT2000_2009.csv')
  OT$tag = as.character(OT$tag)
  OT$species = as.character(OT$species)
OL = read.csv('OL2000_2009.csv')
  OL$tag = as.character(OL$tag)
  OL$species = as.character(OL$species)
DO = read.csv('DO2000_2009.csv')
  DO$tag = as.character(DO$tag)
  DO$species = as.character(DO$species)
DM = read.csv('DM2000_2009.csv')
  DM$tag = as.character(DM$tag)
  DM$species = as.character(DM$species)
SH = read.csv('SH2000_2009.csv')
  SH$tag = as.character(SH$tag)
  SH$species = as.character(SH$species)
SF = read.csv('SF2000_2009.csv')
  SF$tag = as.character(SF$tag)
  SF$species = as.character(SF$species)
NAO = read.csv('NA2000_2009.csv')
  NAO$tag = as.character(NAO$tag)
  NAO$species = as.character(NAO$species)
RM = read.csv('RM2000_2009.csv')
  RM$tag = as.character(RM$tag)
  RM$species = as.character(RM$species)


# make sure text is text and not atomic
heteros$tag = as.character(heteros$tag)
heteros$sex = as.character(heteros$sex)
heteros$species = as.character(heteros$species)

#loop through the list of species-level dataframes, so don't have to do everything separately  ##FIXME
for (df in list(PE, PM, OT, OL, DO, DM, SH, SF, NAO, RM, heteros)) id_unknowns(df,13)

# give untagged indivs unique tag numbers (7 digits)
heteros = id_unknowns(heteros, 13)
PE = id_unknowns(PE,13)
PM = id_unknowns(PM,13)
OT = id_unknowns(OT,13)
OL = id_unknowns(OL,13)
DO = id_unknowns(DO,13)
DM = id_unknowns(DM,13)
SH = id_unknowns(SH,13)
SF = id_unknowns(SF,13)
NAO = id_unknowns(NAO,13)
RM = id_unknowns(RM,13)

# get rid of 'bad data'
heteros = subsetDat(heteros)
PE = subsetDat(PE)
PM = subsetDat(PM)
OT = subsetDat(OT)
OL = subsetDat(OL)
DO = subsetDat(DO)
DM = subsetDat(DM)
SH = subsetDat(SH)
SF = subsetDat(SF)
NAO = subsetDat(NAO)
RM = subsetDat(RM)

# separate the pocketmouse data
PB = subset(heteros, species == 'PB')
PP = subset(heteros, species == 'PP')
PF = subset(heteros, species == 'PF')


# DISTANCES, want to calculate distances moved for a single species
# plot captures to see if MARK is a good way to look at these

# get unique tags for each species
pftags = unique(PF$tag)
pptags = unique(PP$tag)
pbtags = unique(PB$tag)
petags = unique(PE$tag)
pmtags = unique(PM$tag)
ottags = unique(OT$tag)
oltags = unique(OL$tag)
dotags = unique(DO$tag)
dmtags = unique(DM$tag)
shtags = unique(SH$tag)
sftags = unique(SF$tag)
naotags = unique(NAO$tag)
rmtags = unique(RM$tag)

#make vectors of distances moved for all recaptured individuals, by SPECIES
pf_meters = distance_moved(PF, pftags)
pp_meters = distance_moved(PP, pptags)
pb_meters = distance_moved(PB, pbtags)
pe_meters = distance_moved(PE, petags)
pm_meters = distance_moved(PM, pmtags)
ot_meters = distance_moved(OT, ottags)
ol_meters = distance_moved(OL, oltags)
do_meters = distance_moved(DO, dotags)
dm_meters = distance_moved(DM, dmtags)
sh_meters = distance_moved(SH, shtags)
sf_meters = distance_moved(SF, sftags)
nao_meters = distance_moved(NAO, naotags)
rm_meters = distance_moved(RM, rmtags)

#make vectores of distances moved for all recaptured individuals, by GUILD
Hgran = c(pf_meters, pp_meters, pb_meters, dm_meters, do_meters)
Cgran = c(pe_meters, pm_meters, rm_meters)
foli = c(sh_meters, sf_meters) #doesn't include NA because similar resource, but majorly different strategies - midden
insec = c(ot_meters, ol_meters)

#plot histogram of all consecutive movement for rodents within a species 2000-2009
#create vector of breaks, incrementing by 6 meters (represents approx. 1 stake) since data are not continuous
v6 = seq(-3,500,6)
Hgcount = hist(Hgran, breaks = v6, col = 'mediumpurple4', xlim = c(0,500), ylim = c(0, 2500), main = 'Heteromyids - PF, PP, PB, DO, DM')      
Cgcount = hist(Cgran, breaks = v6, col = 'mediumpurple4', xlim = c(0,500), ylim = c(0,20), main = 'Cricetids - PE, PM, RM')
focount = hist(foli, breaks = v6, col = 'mediumpurple4', xlim = c(0,500), main = 'folivores - SH, SF, NA')
nacount = hist(nao_meters, breaks = v6, col = 'mediumpurple4', xlim = c(0,500), main = 'neotoma - NA')
incount = hist(insec, breaks = v6, col = 'mediumpurple4', xlim = c(0,500), ylim = c(0,80), main = 'insectivores - OT, OL')

# plot density of movment by guild for 2000-2009
plot(density(Hgran), main = 'Portal movement by guild', xlab = 'meters', lwd = 2, col = 'hotpink2')
  lines(density(Cgran), col = 'deepskyblue3', lwd = 3, lty = 6)
  lines(density(nao_meters), col = 'indianred4', lwd = 4, lty = 3)
  lines(density(foli), col = 'mediumpurple4', lwd = 4, lty = 3)
  lines(density(insec), col = 'darkgreen', lwd = 2)
  legend('topright', c('Hgran', 'Neotoma', 'foliv', 'Cgran', 'insec'), bty = 'n', lty = c(1,3,6,3,1), lwd = 5, seg.len = 2,
         col = c('hotpink2', 'indianred4', 'mediumpurple4', 'deepskyblue3', 'darkgreen'))

#### Make an occupancy plot for 2000-2009 (similar to Morgan)
#proportion of years they were seen in
pfyr = length(unique(PF$year))/10
ppyr = length(unique(PP$year))/10
pbyr = length(unique(PB$year))/10
peyr = length(unique(PE$yr))/10
pmyr = length(unique(PM$yr))/10
otyr = length(unique(OT$yr))/10
olyr = length(unique(OL$yr))/10
doyr = length(unique(DO$yr))/10
dmyr = length(unique(DM$yr))/10
shyr = length(unique(SH$yr))/10
sfyr = length(unique(SF$yr))/10
naoyr = length(unique(NAO$yr))/10
rmyr = length(unique(RM$yr))/10

#proportion of within-year trapping periods they were seen in 
pfmo = mean_win_yr_occ(PF)
ppmo = mean_win_yr_occ(PP)
pbmo = mean_win_yr_occ(PB)
pemo = mean_win_yr_occ(PE)
pmmo = mean_win_yr_occ(PM)
otmo = mean_win_yr_occ(OT)
olmo = mean_win_yr_occ(OL)
domo = mean_win_yr_occ(DO)
dmmo = mean_win_yr_occ(DM)
shmo = mean_win_yr_occ(SH)
sfmo = mean_win_yr_occ(SF)
naomo = mean_win_yr_occ(NAO)
rmmo = mean_win_yr_occ(RM)  

#mean abundance within all years
pfabun = mean_allyrs_abun(PF)
ppabun = mean_allyrs_abun(PP)
pbabun = mean_allyrs_abun(PB)
peabun = mean_allyrs_abun(PE)
pmabun = mean_allyrs_abun(PM)
otabun = mean_allyrs_abun(OT)
olabun = mean_allyrs_abun(OL)
doabun = mean_allyrs_abun(DO)
dmabun = mean_allyrs_abun(DM)
shabun = mean_allyrs_abun(SH)
sfabun = mean_allyrs_abun(SF)
naoabun = mean_allyrs_abun(NAO)
rmabun = mean_allyrs_abun(RM)


# plot temporal occupancy - for month and year
plot(pfyr, pfmo, xlim = c(0,1), ylim = c(0,1), xlab = "acros-year occupancy", ylab = "within-year occupancy", pch = 19, col = "hotpink")
    #textxy(pfyr, pfmo, "PF")
  points(ppyr, ppmo, pch = 19, col = "hotpink")
    #textxy(ppyr, ppmo, "PP")
  points(pbyr, pbmo, pch = 19, col = "hotpink")
    #textxy(pbyr, pbmo, "PB")
  points(doyr, domo, pch = 19, col = "hotpink")
    #textxy(doyr, domo, "DO")
  points(dmyr, dmmo, pch = 19, col = "hotpink")
    #textxy(dmyr, dmmo, "DM")
  points(peyr, pemo, pch = 19)
    textxy(peyr, pemo, "Cgran")
  points(pmyr, pmmo, pch = 19)
    textxy(pmyr, pmmo, "Cgran")
  points(otyr, otmo, pch = '*', cex = 1.5)
    textxy(otyr, otmo, "insectiv")
  points(olyr, olmo, pch = '*', cex = 1.5)
    textxy(olyr, olmo, "insectiv")
  points(shyr, shmo, pch = 19)
    textxy(shyr, shmo, "foliv")
  points(sfyr, sfmo, pch = 19)
    textxy(sfyr, sfmo, "foliv")
  points(naoyr, naomo, pch = 19, col = "purple")
    textxy(naoyr, naomo, "NA")
  points(rmyr, rmmo, pch = 19)
    textxy(rmyr, rmmo, "Cgran")
  abline(v = 0.5, lty = 2, col = 'gray40', lwd = 2)
  abline(h = 0.5, lty = 2, col = 'gray40', lwd = 2)

##------- plot number of individuals captured in each period for each group, save in pdf file
pdf(file="indivs_per_year.pdf",11,7.5)
par(mfrow=c(3,2))

plot_freq_by_prd(PB, "PB")
plot_freq_by_prd(PP, "PP")
plot_freq_by_prd(PF, "PF")
plot_freq_by_prd(DM, "DM")
plot_freq_by_prd(DO, "DO")
plot_freq_by_prd(PE, "PE")
plot_freq_by_prd(PM, "PM")
plot_freq_by_prd(RM, "RM")
plot_freq_by_prd(NAO, "NAO")
plot_freq_by_prd(SF, "SF")
plot_freq_by_prd(SH, "SH")
plot_freq_by_prd(OT, "OT")
plot_freq_by_prd(OL, "OL")

dev.off()

##------ histograms of the number of recaptures (some species are recaptured far less often), save in pdf file
pdf(file = "recaps_by_species.pdf", 11, 7.5)
par(mfrow = c(2,4))

plot_recap_hist(PB, "PB")
plot_recap_hist(PP, "PP")
plot_recap_hist(PF, "PF")
plot_recap_hist(DM, "DM")
plot_recap_hist(DO, "DO")
plot_recap_hist(PE, "PE")
plot_recap_hist(PM, "PM")
plot_recap_hist(RM, "RM")
plot_recap_hist(NAO, "NAO")
plot_recap_hist(SF, "SF")
plot_recap_hist(SH, "SH")
plot_recap_hist(OT, "OT")
plot_recap_hist(OL, "OL")

dev.off()

######################### EXTRA STUFF, PROBABLY DON'T NEED.... ##############################


############## MOVING
# get list of indivs that moved plots or treatment, species is included
moving_rats = find_rats_that_move(heteros, tags, 8, 9, 3, 4)

# subset tags that never leave a plot
stationary_hets = heteros
plotmovers = unique(moving_rats$tag)
for (i in 1:length(plotmovers)) {
  stationary_hets = subset(stationary_hets, tag != plotmovers[i])
}

# list the stakes it inhabits
tags = unique(stationary_hets$tag)
stakemoves = examine_stake_moves(stationary_hets, tags, 5, 6, 7, 8, 9)

# calculate the distances between each trapping location
plot_stake_moves(stationary_hets, tags, 5, 4, 8, 9)


# plot density of all consecutive movement from rodents within a species 2000-2009
plot(density(pf_meters), main = paste("P. flavus (", length(pftags), " = i, ", length(pf_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(pp_meters), main = paste("C. penicillatus (", length(pptags), " = i, ", length(pp_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(pb_meters), main = paste("C. baileyi (", length(pbtags), " = i, ", length(pb_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(do_meters), main = paste("D. ordii (", length(dotags), " = i, ", length(do_meters), " = N)", sep = ''), lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(dm_meters), main = paste("D. merriami (", length(dmtags), "= i, ", length(dm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))

plot(density(pe_meters), main = paste("P. eremicus (", length(petags), " = i, ", length(pe_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(pm_meters), main = paste("P. maniculatus (", length(pmtags), " = i, ", length(pm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(rm_meters), main = paste("R. megalotis (", length(rmtags), "= i, ", length(rm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))

plot(density(sh_meters), main = paste("S. hispidus (", length(shtags), " = i, ", length(sh_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(sf_meters), main = paste("S. fulviventer (", length(sftags), " = i, ", length(sf_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(nao_meters), main = paste("N. albigula (", length(naotags), " = i, ", length(nao_meters), " = N)", sep = ''), lwd = 2, xlim = c(0,500), ylim = c(0,.1))

plot(density(ot_meters), main = paste("O. torridus (", length(ottags), " = i, ", length(ot_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
plot(density(ol_meters), main = paste("O. leucogaster (", length(oltags), " = i, ", length(ol_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))


#plot all together
plot(density(pb_meters), main = 'Portal rodent movement', xlab = 'meters', lwd = 2, col = 'peru')
lines(density(pp_meters), col = 'black', lwd = 2)
lines(density(do_meters), col = 'mediumpurple4', lwd = 4, lty = 3)
lines(density(dm_meters), col = 'maroon4', lwd = 4, lty = 3)
lines(density(pf_meters), col = 'hotpink', lwd = 2)

lines(density(pe_meters), col = 'deepskyblue3', lwd = 3, lty = 6)
lines(density(pm_meters), col = 'royalblue4', lwd = 3, lty = 6)
lines(density(rm_meters), col = 'cadetblue', lwd = 3, lty = 6)

lines(density(sh_meters), col = 'indianred', lwd = 3)
lines(density(sf_meters), col = 'brown', lwd = 3)
lines(density(nao_meters), col = 'gray60', lwd = 3)

lines(density(ot_meters), col = 'darkgreen', lwd = 2)
lines(density(ol_meters), col = 'darkolivegreen', lwd = 2)

legend('topright', c('PB','PP','DO','DM','PE','PM','OT','OL'), bty = 'n', lty = c(1,1,3,3,6,6,1,1), lwd = 10, seg.len = 2, 
       col = c('peru', 'black', 'mediumpurple4','maroon4','deepskyblue3', 'royalblue3','darkgreen', 'darkolivegreen'))
abline(v = 70.71, lty = 2, col = 'gray60', lwd = 2)
