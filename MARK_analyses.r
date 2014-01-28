# for manipulating  rodent movement data in MARK
## analyze survival and dispseral probabilities for species
# Use multistrata models that give S(survival), p(capture probability) and Psi(transition probability)
# Will compare data among species and guilds in post-hoc analyses. 
# Input files (.inp) generated using rodent_wrapper.r and movement_fxns.r

# Do psi and S significantly differ among species/guilds?


rm(list=ls(all=TRUE))   # clears the memory

#---------------------------------------------------------------------------------
#          bring in the data and source files
#---------------------------------------------------------------------------------
#set working directory and import source code
setwd("C://Users//sarah//Documents//GitHub//portal-rodent-dispersal//mark_datafiles//")
#setwd("~/portal-rodent-dispersal/")

#grab all the .inp files to loop over for analysis
files = list.files(getwd(), pattern = "mark.txt", full.name=T, recursive=T)

for (f in 1:length(files)){
  
  require(RMark)
  
# bring in the inp files and convert to RMark format 
ms_data = import.chdata(files[f], field.types=c("n","f"))

#convert to factor
ms_data$species = as.factor(ms_data$species)
spname = ms_data$species[1]

print(spname)
cat("Imported data.", file="outfile.txt", sep="\n")

#---------------------------------------------------------------------------------
#          process multistrata data, includes capture at home, and dipsersal transitions 
#---------------------------------------------------------------------------------
# Build up the model. 
# begin.time == first period number
ms_process = process.data(ms_data, model = "Multistrata", begin.time = 130, group = c("species"))

#ddl = design data list
ms_ddl = make.design.data(ms_process, parameters=list(S=list(pim.type="time"), 
                                                      p=list(pim.type="time"), 
                                                      Psi=list(pim.type="time"))) 

#---------------------------------------------------------------------------------
#          make dummy variables and covariates
#---------------------------------------------------------------------------------
# Add dummy variables for operating on specific states(strata) or transitions
# Species start in 1, movement between 1 and 2 (or vice versa) indicates a relatively long distance movement
# Switching states indicates making a transition

# SURVIVAL probability given that the individual is in A
ms_ddl$S$inA = 0
ms_ddl$S$inA[ms_ddl$S$stratum == "1"] = 1

# SURVIVAL probability given that the individual is in B
ms_ddl$S$inB = 0
ms_ddl$S$inB[ms_ddl$S$stratum == "2"] = 1

# RECAPTURE probability given that the individual is in A
ms_ddl$p$strataA = 0
ms_ddl$p$strataA[ms_ddl$p$stratum == "1"] = 1

# RECAPTURE probability given that the individual is in B
ms_ddl$p$strataB = 0
ms_ddl$p$strataB[ms_ddl$p$stratum == "2"] = 1

# TRANSITION probability given that the individual switches states
# TODO: Change this to fix 1-->2 = 2-->1, since we are now interested in "switching"
#ms_ddl$Psi$movement = 0
#ms_ddl$Psi$movement[ms_ddl$Psi$stratum %in% c("1","2") & ms_ddl$Psi$tostratum == "2"] = 1

  
#--------------------------------------------------------------------------------
#           Build up the models
#---------------------------------------------------------------------------------
#          Define model structures for S (survival probability)
#---------------------------------------------------------------------------------
Snull = list(formula = ~1)          


#---------------------------------------------------------------------------------
#          Define model structures for p (capture probability)
#---------------------------------------------------------------------------------
# fix recapture probabilities for unsampled or omitted months
#    skipped_periods = c(237, 241, 267, 277, 278, 283, 284, 300, 311, 313, 314, 318, 321, 323, 337, 339, 344, 351): p = 0

  ## KTS: I removed these for now- eventually we need to deal with untrapped periods though.
  ###   can we just remove these periods from the CH and set the time interval between bouts appropriately?
  # select periods that were omitted from the study - untrapped
  # TODO: this may need to be redefined, since using the parameters, pim.type arguments
# select periods that were omitted from the study - untrapped
p237 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 237,]))
p241 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 241,]))
p267 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 267,])) 
p277 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 277,]))
p278 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 278,]))
p283 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 283,]))
p284 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 284,]))
p300 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 300,]))
p311 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 311,]))
p313 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 313,]))
p314 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 314,]))
p318 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 318,]))
p321 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 321,]))
p323 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 323,]))
p337 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 337,]))
p339 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 339,]))
p344 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 344,]))
p351 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 351,]))

# set those periods to p = 0, because they *can't* be anything else
p237val = rep(0, length(p237))
p241val = rep(0, length(p241))
p267val = rep(0, length(p267))
p277val = rep(0, length(p277))
p278val = rep(0, length(p278))
p283val = rep(0, length(p283))
p284val = rep(0, length(p284))
p300val = rep(0, length(p300))
p311val = rep(0, length(p311))
p313val = rep(0, length(p313))
p314val = rep(0, length(p314))
p318val = rep(0, length(p318))
p321val = rep(0, length(p321))
p323val = rep(0, length(p323))
p337val = rep(0, length(p337))
p339val = rep(0, length(p339))
p344val = rep(0, length(p344))
p351val = rep(0, length(p351))

cat("Fixed omitted periods to zero.", sep="\n", file="outfile.txt", append=TRUE)


# look for effects on recapture probability, given that some p are fixed to 0 (listed below)
# link = "logit" is the default. "cloglog" may be esp. useful when there are fewer recaptures

#Null Model
pnull = list(formula = ~1, fixed = list(index = c(p237, p241, p267, p277, p278, p283, p284, p300, p311, p313, p314,
                                                   p318, p321, p323, p337, p339, p344, p351), 
                                         value = c(p237val, p241val, p267val, p277val, p278val, p283val, p284val, p300val, p311val,
                                                   p313val, p314val, p318val, p321val, p323val, p337val, p339val,
                                                   p344val, p351val), link = "cloglog"))

  pnull = list(formula = ~1)
  
cat("Model for period effect on recapture probability.", sep="\n", file="outfile.txt", append=TRUE)


#---------------------------------------------------------------------------------
#          Define model structures for Psi (transition probability)
#---------------------------------------------------------------------------------
# TODO: change Psi model to new version
  Psinull = list(formula = ~1, link = "logit")   # KTS: Psinull is correct if we want psi 1 >> 2 == psi 2 >> 1
  
 # Psimovement = list(formula = ~movement, link = "logit") 

cat("Defined model structure for Psi", sep="\n", file="outfile.txt", append=TRUE)

  
#---------------------------------------------------------------------------------
#          Run Models and collect results
#---------------------------------------------------------------------------------
#send results to new folder - change working directory
#wd = "~/portal-rodent-dispersal/mark_output/"
setwd("C://Users//sarah//Documents//GitHub//portal-rodent-dispersal//mark_output//")

cat("running the multistrata models", sep="\n", file="outfile.txt", append=TRUE)

# SIMANNEAL should be best for multistrata models, but may take longer to run
#TODO: make sure to refer to the correct Psi model here
movemodel = mark(ms_process, ms_ddl, model.parameters = list(S=Snull,  p=pnull, Psi=Psinull),
                            options="SIMANNEAL", external=FALSE)

cat("Null model is finished", sep="\n", file="outfile.txt", append=TRUE)


#-----------------------------------------------------
#             summarize results
#-----------------------------------------------------
#ms_results <- collect.models(type = "Multistrata")
#print(ms_results)
cat("Summarized results.", sep="\n", file="outfile.txt", append=TRUE)

print (movemodel$results$beta[1:3,])
print (movemodel$results$real[1:3,])


#---------------------------------------------------------------------------------
#          Write result data to csv files
#---------------------------------------------------------------------------------
write.csv(movemodel$results$beta, paste("movemodel_beta_", spname, ".csv", sep=""))
write.csv(movemodel$results$real, paste("movemodel_real_", spname, ".csv", sep=""))

cat("End Code. Look for your csv files.", sep="\n", file="outfile.txt", append=TRUE)
print( paste(spname, " is done.", sep = ""))
  
rm(list=ls()[!ls() %in% c("f", "files")])   # clears the memory of everything except the file list
}





#---------------------------------------------------------------------------------
#          analyze the data from the Program Mark analysis
#---------------------------------------------------------------------------------
#---------- concatenate results
library(stringr)

#grab all the .inp files to loop over for analysis
files = list.files(getwd(), pattern = "real", full.name=T, recursive=T)

#make a new dataframe with the results
estimates = data.frame(species=NA, S=1, S_se=1, p=1, p_se=1, Psito2=1, Psi2_se = 1, Psito1=1, Psi1_se=1)
outcount = 1

for (f in 1:length(rfiles)){
  dat = read.csv(rfiles[f], header=T, sep=",")
  spname = str_sub(rfiles[f],-6,-5)
  estimates[outcount,1] = spname
  estimates[outcount,2:9] = c(dat[1,2], dat[1,3], dat[2,2], dat[2,3], dat[3,2], dat[3,3], 
                              dat[4,2], dat[4,3])
  outcount = outcount + 1
}

#check that this is in correct order, if redoing the analysis #FIXME/TODO
estimates$status = as.factor(c("core", "core", "intermediate", "core", "core", "core", "core", "core", "core",
                     "intermediate", "core", "core", "intermediate", "transient", "transient", "transient"))


SbyPsi = ggplot(estimates, aes(Psito2, S, col=status)) + geom_point(size = 3) + theme_bw() + 
          theme(text = element_text(size=20)) + 
          xlab("Long-distance movement probability") + ylab("Survival probability") + 
          geom_errorbar(aes(x = Psito2, ymin = S - S_se, ymax = S + S_se), width=0.01) +
          geom_errorbarh(aes(xmin = Psito2 - Psi2_se, xmax = Psito2 + Psi2_se))

Sbyp = ggplot(estimates, aes(p, S, col=status)) + geom_point(size = 3) + theme_bw() + 
         theme(text = element_text(size=20)) + 
         xlab("recapture probability") + ylab("Survival probability") + 
         geom_errorbar(aes(x = p, ymin = S - S_se, ymax = S + S_se), width=0.01) +
         geom_errorbarh(aes(xmin = p - p_se, xmax = p + p_se))

Psibyp =  ggplot(estimates, aes(Psito2, p, col=status)) + geom_point(size = 3) + theme_bw() + 
  theme(text = element_text(size=20)) + 
  xlab("long-distance movement probability") + ylab("recapture probability") + 
  geom_errorbar(aes(x = Psito2, ymin = p - p_se, ymax = p + p_se), width=0.01) +
  geom_errorbarh(aes(xmin = Psito2 - Psi2_se, xmax = Psito2 + Psi2_se))


