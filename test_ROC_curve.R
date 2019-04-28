if (!require(optimx)) {install.packages("optimx"); require(optimx)}
# #require(lme4)
if (!require(matrixStats)) {install.packages("matrixStats"); require(matrixStats)}
if (!require(matlab)) {install.packages("matlab"); require(matlab)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(data.table)) {install.packages("data.table"); require(data.table)}

# Run logreg model and do cross-validation to determine how well it's doing (train and test on subset of data)

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/LogReg/PRO_A/")

alltrialsdat <- read.csv("alltrialsdatC.csv", row.names = 1, stringsAsFactors = FALSE)

alltrialsdat <- alltrialsdat[alltrialsdat$participant != "", ]
str(alltrialsdat)


# # # --------------------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------------------        
alltrialsdat$actualChoicebin <- ifelse(alltrialsdat$choiceType=="steal", 1, 0)
condDatA <- alltrialsdat[alltrialsdat$type=="A", ]
condDatA <- condDatA[!is.na(condDatA$choiceResponse.rt), ]
u.ppl <- unique(condDatA$participant)
n <- length(u.ppl)

#idxTrn <- u.ppl[1:15]                                 # training sample
#idxTst <- u.ppl[16:27]             # test sample
l.ppl <- length(u.ppl)

for (participant in 1) {
  print(participant)
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  probStealGLM <- glm(tempDat$actualChoicebin ~ tempDat$stealTrial + tempDat$earnTrial,
                      family = binomial(logit), data = tempDat) #FE logistic regression
}

mainDat <- data.frame()

for (participant in 16){ #16:27
  tempDat <- condDatA[condDatA$participant==u.ppl[participant],]
  predTst <- predict(probStealGLM, tempDat, type="response")
  predTsts <- prediction(predTst,tempDat)
  fitperf = performance(predTsts,"tpr","fpr")
  plot(fitperf,col="green",lwd=2,main="ROC Curve for Logistic:  Adult")
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
  
}
  #thresh  <- 0.5            # threshold for categorizing predicted probabilities
  #predFac <- cut(predTst, breaks=c(-Inf, thresh, Inf), labels=c("earn", "steal"))
  #cTab = table(tempDat$actualChoicebin, predFac, dnn=c("actual", "predicted"))  #change this to test subset only - isn't it already? why so many trials? should be 880 (11*80)
  #addmargins(cTab)
  #prop.table(cTab, 1)
  #cTabProb <- data.frame(prop.table(cTab, 1)) #compute probabilities
  #cTabProb$participant <- unique(tempDat$participant)
