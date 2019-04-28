if (!require(optimx)) {install.packages("optimx"); require(optimx)}
#require(lme4)
if (!require(matrixStats)) {install.packages("matrixStats"); require(matrixStats)}
if (!require(matlab)) {install.packages("matlab"); require(matlab)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}



setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/")

alltrialsdat <- read.csv("totalAlltrials.csv", row.names = 1, stringsAsFactors = FALSE)

alltrialsdat <- alltrialsdat[alltrialsdat$participant != "", ]
str(alltrialsdat)


# --------------------------------------------------------------------------------------------------        
# aggression function

aggFunc <- function(a, betaT, participant, data = alltrialsdat) { 
  singsub <- data[data$participant==participant, ]
  #singsub <- singsub[singsub$type=="A", ]
  singsub <- singsub[!is.na(singsub$choiceResponse.rt), ]
  actualChoicebin = ifelse(singsub$choiceType=="steal", 1, 0)

  probStealGLM <- glm(actualChoicebin ~ singsub$stealTrial + singsub$earnTrial, family = binomial(logit), data = singsub) #FE logistic regression
  output <- unname(coef(probStealGLM))
  B1 <- output[2]
  B2 <- output[3]
  a <- B1/B2
  a
  dat <- data.frame(
    participant = singsub$participant,
    stealTrial = singsub$stealTrial,
    earnTrial = singsub$earnTrial,
    actualChoice = singsub$choiceType, 
    actualChoicebin = actualChoicebin, 
    #probSteal = probSteal, 
    #probBank = 1-probSteal, 
    #modelChoice = ifelse(probSteal>probBank, "steal", "bank"), 
    #modelChoicebin = ifelse(probSteal>probBank, 1, 0), 
    #LL = loglike, 
    tempr = B2, 
    agg = a,
    stringsAsFactors = FALSE)  
  return(dat)
} 


# --------------------------------------------------------------------------------------------------  
# loop
mainDat <- data.frame()
df <- nrow
condDatA <- alltrialsdat
condDatA <- condDatA[!is.na(condDatA$choiceResponse.rt), ]
u.ppl <- unique(condDatA$participant)
n <- length(u.ppl)

for (participant in u.ppl[1:n]) {
  outputParam<-aggFunc(a = agg, betaT = B2, participant = participant)
  
  data <- list(participant, outputParam$agg, outputParam$tempr)
  
  df <- as.data.frame(do.call("cbind", data))
  
    
  names(df)<- c("participant", "best agg A", "best tempr A")
  mainDat <- rbind(mainDat, df)

names(mainDat) <- c("participant", "best agg A", "best tempr A")

write.csv(mainDat, file = paste(c("combined_model_summary_logreg", ".csv"), collapse = "")) 
 
  }
#------------------------------------------------------------------------------------------------#
# Train and Test Model #
x    <- rnorm(100, 175, 7)                     # predictor variable
y    <- 0.4*x + 10 + rnorm(100, 0, 3)          # continuous predicted variable
yFac <- cut(y, breaks=c(-Inf, median(y), Inf), labels=c("lo", "hi"))    # median split
d    <- data.frame(yFac, x)                    # data frame

# now set aside training sample and corresponding test sample
idxTrn <- 1:70                                 # training sample
idxTst <- !(1:nrow(d) %in% idxTrn)             # test sample -> all remaining obs
# if idxTrn were a logical index vector, this would just be idxTst <- !idxTrn

# fit logistic regression only to training sample
fitTrn <- glm(yFac ~ x, family=binomial(link="logit"), data=d, subset=idxTrn)

# apply fitted model to test sample (predicted probabilities)
predTst <- predict(fitTrn, d[idxTst, ], type="response")

thresh  <- 0.5            # threshold for categorizing predicted probabilities
predFac <- cut(predTst, breaks=c(-Inf, thresh, Inf), labels=c("lo", "hi"))
cTab    <- table(yFac[idxTst], predFac, dnn=c("actual", "predicted"))
addmargins(cTab)

# lme4
# look at proportion of choices that it predicts correctly and look at choice probabilities (or look at average predicted probability)
# or do cross-validation: fit model to subset of trials and then test on other subset of trials (training and test trials)
# or compare model to chance (does it predict better than chance)
# or use R code from the psych science

