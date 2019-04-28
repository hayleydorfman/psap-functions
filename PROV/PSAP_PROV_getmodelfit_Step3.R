# Get model fit information

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")
#fitDat <- read.csv("PROV_model_summary_noharmparam.csv") #no harm param
#fitDat <- read.csv("PROV_model_summary_harmparam.csv") #harm param
fitDat <- read.csv("PROV_model_summary_harmparam.csv") #harm param 0 to 1 bound




fullData <- data.frame()

for (i in 1:length(fitDat$participant)){
  a <- fitDat$best.agg[i]
  betaT <- fitDat$best.tempr[i]
  participant <- fitDat$participant[i]
  modelFit <- aggFunc(a, betaT, participant, data = alltrialsdat)
  #thresholded <- sum(abs(fullData$actualChoicebin[i] - fullData$modelChoicebin[i]))
  #print(fullData$actualChoicebin[i]- fullData$modelChoicebin[i])
  #betterThanChance <- sum(pbinom(0:thresholded, 108, 0.5))  #PPR version
  #betterThanChance <- sum(pbinom(0:thresholded, 80, 0.5))  #PRO version
  fullData <- rbind(fullData, modelFit)
  
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"PROV_noharmparam_model_fit", ".csv"), collapse = "")) #without harm param
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"PROV_harmparam_model_fit", ".csv"), collapse = "")) #with harm param
  write.csv(modelFit, file = paste(c(fitDat$participant[i],"PROV_harmparam_model_fit_01", ".csv"), collapse = "")) #with harm param 0 to 1 bound
}

