if (!require(optimx)) {install.packages("optimx"); require(optimx)}
# #require(lme4)
if (!require(matrixStats)) {install.packages("matrixStats"); require(matrixStats)}
if (!require(matlab)) {install.packages("matlab"); require(matlab)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(data.table)) {install.packages("data.table"); require(data.table)}

# Run logreg model and  look at proportion of choices that it predicts correctly and look at choice probabilities (or look at average predicted probability)

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_AB/Task/Active/Experimental Files/Code/Conditions/")

alltrialsdat <- read.csv("alltrials.csv", row.names = 1, stringsAsFactors = FALSE)

alltrialsdat <- alltrialsdat[alltrialsdat$participant != "", ]
str(alltrialsdat)


# # # --------------------------------------------------------------------------------------------------
# 
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_AB/Task/Active/Experimental Files/Code/model_tests/")
# 
# # --------------------------------------------------------------------------------------------------        
alltrialsdat$actualChoicebin <- ifelse(alltrialsdat$choiceType=="steal", 1, 0)
condDatA <- alltrialsdat[alltrialsdat$type=="A", ]
condDatA <- condDatA[!is.na(condDatA$choiceResponse.rt), ]
u.ppl <- unique(condDatA$participant)
n <- length(u.ppl)

l.ppl <- length(u.ppl)
mainDat <- data.frame()

for (participant in 1:27) {
  print(participant)
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  probStealGLM <- glm(tempDat$actualChoicebin ~ tempDat$stealTrial + tempDat$earnTrial,
                      family = binomial(logit), data = tempDat) #FE logistic regression
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  predTst <- predict(probStealGLM, tempDat, type="response")
  thresh  <- 0.5            # threshold for categorizing predicted probabilities
  predFac <- cut(predTst, breaks=c(-Inf, thresh, Inf), labels=c("earn", "steal"))
  cTab = table(tempDat$actualChoicebin, predFac, dnn=c("actual", "predicted"))
  addmargins(cTab)
  prop.table(cTab, 1)
  cTabProb <- data.frame(prop.table(cTab, 1)) #compute probabilities
  cTabProb$participant <- unique(tempDat$participant)
  
  df <- as.data.frame(do.call("cbind", cTabProb))
  
  mainDat <- rbind(mainDat, df)  
}

write.csv(mainDat, file = paste(c("logreg_model_probs", ".csv"), collapse = ""))


