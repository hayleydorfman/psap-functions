if (!require(optimx)) {install.packages("optimx"); require(optimx)}
# #require(lme4)
if (!require(matrixStats)) {install.packages("matrixStats"); require(matrixStats)}
if (!require(matlab)) {install.packages("matlab"); require(matlab)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(data.table)) {install.packages("data.table"); require(data.table)}

# Run logreg model and do cross-validation to determine how well it's doing (train and test on subset of data)

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A/Task/Active/Experimental Files/PSAP_PRO_A_Task/data/")

alltrialsdat <- read.csv("alltrials.csv", row.names = 1, stringsAsFactors = FALSE)

alltrialsdat <- alltrialsdat[alltrialsdat$participant != "", ]
str(alltrialsdat)


# # # --------------------------------------------------------------------------------------------------
# 
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/LogReg/")
# 
# # --------------------------------------------------------------------------------------------------        
alltrialsdat$actualChoicebin <- ifelse(alltrialsdat$choiceType=="steal", 1, 0) #fix this to account for missed trials!
condDatA <- alltrialsdat[alltrialsdat$type=="A", ]
condDatA <- condDatA[!is.na(condDatA$choiceResponse.rt), ]
u.ppl <- unique(condDatA$participant)
n <- length(u.ppl)

# Training Sample

l.ppl <- length(u.ppl)

for (participant in 1:15) {
  print(participant)
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  probStealGLM <- glm(tempDat$actualChoicebin ~ tempDat$stealTrial + tempDat$earnTrial,
                      family = binomial(logit), data = tempDat) #FE logistic regression
}

mainDat <- data.frame()

# Test Sample

for (participant in 16:27){ #16:27
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  predTst <- predict(probStealGLM, tempDat, type="response") #value between 0 or 1 that represents the “odds” that that observation is a 1
  thresh  <- 0.5            # threshold for categorizing predicted probabilities; cutoff point on the range (0,1) for which you declare that observation to be a 1
  predFac <- cut(predTst, breaks=c(-Inf, thresh, Inf), labels=c("earn", "steal"))
  cTab = table(tempDat$actualChoicebin, predFac, dnn=c("actual", "predicted"))  #change this to test subset only - isn't it already? why so many trials? should be 880 (11*80)
  addmargins(cTab)
  prop.table(cTab, 1)
  cTabProb <- data.frame(prop.table(cTab, 1)) #compute probabilities
  cTabProb$participant <- unique(tempDat$participant)
  
  df <- as.data.frame(do.call("cbind", cTabProb))
  
  mainDat <- rbind(mainDat, df) #bind predicted probabilities across all choice types for each subject

    
}

write.csv(mainDat, file = paste(c("logreg_model_probs", ".csv"), collapse = ""))

logreg_model_probs <- read.csv("logreg_model_probs.csv")

finalDat <- data.frame()
finalDat$participant <- unique(logreg_model_probs$participant)
for (participant in logreg_model_probs){
  
  corrEarn <- ifelse(logreg_model_probs$actual == 1 & logreg_model_probs$predicted == 1, logreg_model_probs$Freq,"")
  corrSteal <- ifelse(logreg_model_probs$actual == 2 & logreg_model_probs$predicted == 2, logreg_model_probs$Freq,"")
  
  df1 <- as.data.frame(do.call("cbind", corrEarn))
  df2 <- as.data.frame(do.call("cbind", corrSteal))
  
  finalDat <- rbind(finalDat, df1) 
}

# Correct Prediction Table
newDat <- logreg_model_probs[(logreg_model_probs$actual == 1 & logreg_model_probs$predicted == 1) | 
                               (logreg_model_probs$actual == 2 & logreg_model_probs$predicted == 2),]
newDat$actual <- ifelse(newDat$actual == 1, "earn", "steal")

# # lme4
# # look at proportion of choices that it predicts correctly and look at choice probabilities (or look at average predicted probability)
# # or do cross-validation: fit model to subset of trials and then test on other subset of trials (training and test trials)
# # or compare model to chance (does it predict better than chance)
# # or use R code from the psych science
# 
