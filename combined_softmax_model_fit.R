#USED


# Get model fit information

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")
#fitDat <- read.csv("hyper_model1_agg_comb_model_summary.csv") #steal/steal+a
#fitDat <- read.csv("hyper_model2_agg_comb_model_summary.csv") #steal/1+a
#fitDat <- read.csv("hyper_model3_agg_comb_model_summary.csv") #steal*a (original)

fitDat <- read.csv("PROV_model_summary_harmparam.csv") #harm param
#fitDat <- read.csv("PROV_model_summary_harmparam.csv_01") #harm param

trialLen <- 0; subsLL <- 0;
fullData <- data.frame()

for (i in 1:length(fitDat$participant)){
    a <- fitDat$best.agg[i]
    betaT <- fitDat$best.tempr[i]
    participant <- fitDat$participant[i]
    modelFit <- aggFunc(a, betaT, participant, data = alltrialsdat)
    modelFit$trial <- nrow(modelFit)
    trialLen[i] <- nrow(modelFit)
    #thresholded <- sum(abs(fullData$actualChoicebin[i] - fullData$modelChoicebin[i]))
    #print(fullData$actualChoicebin[i]- fullData$modelChoicebin[i])
    #betterThanChance <- sum(pbinom(0:thresholded, 108, 0.5))  #PPR version
    #betterThanChance <- sum(pbinom(0:thresholded, 80, 0.5))  #PRO version
    #modelFit$newLL <- ifelse((modelFit$actualChoice == modelFit$modelChoice) & modelFit$modelChoice == "bank", log(modelFit$probBank), log(modelFit$probSteal))
    modelFit$newLL <- fullData$LL
    
    fullData <- rbind(fullData, modelFit)
    
    subsLL[i] <- sum(modelFit$newLL)
    
    
    
    #write.csv(fullData, file = paste(c(fitDat$participant[i],"agg_model_fit", ".csv"), collapse = ""))
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx", ".csv"), collapse = "")) #Condition A
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optim_bound0to1", ".csv"), collapse = "")) #optim with bound 0 to 1
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx2", ".csv"), collapse = "")) #optimx2
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx3", ".csv"), collapse = "")) #optimx3
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"hyper_model1_agg_comb_model_fit", ".csv"), collapse = ""))
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"hyper_model2_agg_comb_model_fit", ".csv"), collapse = ""))
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"hyper_model3_agg_comb_model_fit", ".csv"), collapse = ""))
    
    #write.csv(modelFit, file = paste(c(fitDat$participant[i],"agg_model_fit_A_optimx5", ".csv"), collapse = "")) #optimx5
}

#Sum of Null LL for all subjects
nullLoglike <- trialLen * log(.5)
nullLoglike <- sum(nullLoglike)

#Get BIC for null

nBIC <- -2*nullLoglike + (2*log(82)) #insert number of trials (82)
nBIC

# Get BIC for model

modelLL <- sum(fullData$newLL)
BIC <- -2*modelLL + (2*log(82)) #insert number of trials (82)
BIC

#--------------------------------------#
#Sum of Null LL for EACH subject
#nullLoglike <- trialLen * log(.5)
#nullLoglike <- sum(nullLoglike)

#Get BIC for null

#nBIC <- -2*nullLoglike + 2*log(80) 
#nBIC #10153.67

# Get BIC for each subject
modelLL <- fullData$newLL
BIC <- -2*modelLL + 2*log(trialLen)

blah<- data.frame(subsLL, trialLen)
blah$subjectBICs <- -2*blah$subsLL + (2*log(blah$trialLen))
blah$nullLL <- blah$trialLen * log(.5)
blah$nullBIC <- -2*blah$nullLL + (2*log(blah$trialLen))
blah$subs <- u.ppl
blah$subs[blah$nullBIC < blah$subjectBICs] #Subjects where model does not fit better than chance

