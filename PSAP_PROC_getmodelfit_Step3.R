# Get model fit information

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/")
fitDat <- read.csv("aggPROC_model_summary.csv") #optimx




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
  
  #write.csv(fullData, file = paste(c(fitDat$participant[i],"agg_model_fit", ".csv"), collapse = ""))
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx", ".csv"), collapse = "")) #Condition A
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optim_bound0to1", ".csv"), collapse = "")) #optim with bound 0 to 1
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx2", ".csv"), collapse = "")) #optimx2
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx3", ".csv"), collapse = "")) #optimx3
  write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_PROC_model_fit", ".csv"), collapse = "")) #optimx4
  #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx5", ".csv"), collapse = "")) #optimx5
}

