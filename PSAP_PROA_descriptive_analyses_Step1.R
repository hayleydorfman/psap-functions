
if (!require(lattice)) {install.packages("lattice"); require(lattice)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(car)) {install.packages("car"); require(car)}     ## package for effect visualization
if (!require(effects)) {install.packages("effects"); require(effects)}     ## package for effect visualization
if (!require(WRS2)) {install.packages("WRS2"); require(WRS2)}   
if (!require(nnet)) {install.packages("nnet"); require(nnet)}              ## contains function for multinomial regression     
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}        ## package for multinomial regression
if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)}              ## comprehensive package for all sorts GLM and GAM specifications     
if (!require(ordinal)) {install.packages("ordinal"); require(ordinal)} 

if (!require(nnet)) {install.packages("nnet"); require(nnet)}              ## contains function for multinomial regression     
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}        ## package for multinomial regression
if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)}              ## comprehensive package for all sorts GLM and GAM specifications     
if (!require(ordinal)) {install.packages("ordinal"); require(ordinal)}     ## comprehensice package for ordinal logistic regression     
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}


# Force R not to use scientific notation
options("scipen" = 100, "digits" = 5)

##MAIN TASK PPR DESCRIPTIVES - ALL SUBJECTS BEHAV DATA##

#Setwd
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/Softmax/PRO_A/")

alltrialsdat <- read.csv("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/Softmax/PRO_A/alltrials.csv")

#Create choice value variable

alltrialsdat$choiceValue <- as.numeric(ifelse(alltrialsdat$choiceType=="steal", alltrialsdat$stealTrial, ifelse(alltrialsdat$choiceType=="bank", alltrialsdat$earnTrial, "missed"))) 



#Create summary df
participant <- c("PROA001","PROA002", "PROA003", "PROA004", "PROA006", "PROA007", "PROA008", "PROA009", "PROA010", "PROA011","PROA012","PROA013","PROA014","PROA015","PROA016","PROA017","PROA018","PROA019","PROA020","PROA021","PROA022")
summarydat <- data.frame(participant)

#mean RT choice
meanRT <- tapply(alltrialsdat$choiceResponse.rt,alltrialsdat$participant,mean, na.rm = T)
# 
summarydat$meanChoiceRT <- meanRT
# 
histogram(summarydat$meanChoiceRT, xlab="Mean Choice RT", col="red")
#
plot(summarydat$meanChoiceRT, summarydat$steals)


# Corr between RT and Value of choice
corr1 <- cor.test(alltrialsdat$choiceResponse.rt, alltrialsdat$choiceValue, method="pearson", na.omit=TRUE)
plot(alltrialsdat$choiceResponse.rt, alltrialsdat$choiceValue)
corr1 

# With log
alltrialsdat$logChoiceRT <- log(alltrialsdat$choiceResponse.rt)
corr2 <- cor.test(alltrialsdat$logChoiceRT, alltrialsdat$choiceValue, method="pearson", na.omit=TRUE)
plot(alltrialsdat$logChoiceRT, alltrialsdat$choiceValue)
corr2 #as the value goes up, the RT gets faster



#Look at catch trials
alltrialsdat$catchType <- ifelse(alltrialsdat$earnTrial >= alltrialsdat$stealTrial,"catch","reg")
alltrialsdat$catchFlag <- ifelse(alltrialsdat$choiceType == "steal" && alltrialsdat$catchType =="catch", "flag", "fine")
excludeSubjs <- length(which(alltrialsdat$catchFlag == "flag")) #Check to make sure you don't need to exclude these subjs for not doing task!
excludeSubjs

#Remove catch trials from alltrialsdat data frame#
alltrialsdat <- alltrialsdat[-which(alltrialsdat$catchType=="catch"),]
# 
# # number of banks and steals and missed
choiceType <- table(alltrialsdat$choiceType,alltrialsdat$participant)
banks <- choiceType[1,]
summarydat$banks <- banks
steals <- choiceType[3,]
summarydat$steals <- steals
missed <- choiceType[2,]
summarydat$missed <- missed

histogram(summarydat$banks, xlab="Banks", col="red")
histogram(summarydat$steals, xlab="Steals", col="red")
histogram(summarydat$missed, xlab="Missed", col="red")

# Plot individual subject steal & bank frequency
choiceFreqPlot <- ggplot((as.data.frame(choiceType)), aes(x=Var2, y = Freq, fill=Var1)) + geom_bar(stat="identity")+
  xlab("Participant")
choiceFreqPlot


#Mean RT for choice type
choiceRT <- data.frame(alltrialsdat$participant, alltrialsdat$choiceResponse.rt)
choiceRT$choiceType <- (alltrialsdat$choiceType)
choiceRT$choiceRT <- alltrialsdat$choiceResponse.rt


#test <- tapply(alltrialsdat$choiceResponse.rt,alltrialsdat$participant, alltrialsdat$choiceType, mean, na.rm = T)

summarydat$meanChoiceRT <- meanRT


histogram(summarydat$banks)
histogram(summarydat$steals)
histogram(summarydat$missed)




#Percent banks and steals and missed
f <- function(x){
  x/80
}


for (p in alltrialsdat$participant) {
  perBank <- f(banks)
}

for (p in alltrialsdat$participant) {
  perSteal <- f(steals)
}

for (p in alltrialsdat$participant) {
  perMissed <- f(missed)
}

summarydat$perBank <- perBank
summarydat$perSteal <- perSteal
summarydat$perMissed <- perMissed



# Plot steals for varying magnitudes of reward
g <- function(x,y){
  x/y
}


for (p in alltrialsdat$participant) {
  perReward <- g(alltrialsdat$earnTrial, alltrialsdat$stealTrial)
}


h <- function(x,y){
  y/x
}

for (p in alltrialsdat$participant) {
  perEarn <- h(alltrialsdat$earnTrial, alltrialsdat$earnTrial)
}

alltrialsdat$perReward <- perReward
alltrialsdat$perEarn <- perEarn
alltrialsdat$perDiff <- perEarn - perReward

# Corr between RT and Percent difference between reward options
corr3 <- cor.test(alltrialsdat$choiceResponse.rt, alltrialsdat$perDiff, method="pearson", na.omit=TRUE)
plot(alltrialsdat$perDiff, alltrialsdat$choiceResponse.rt)
corr3 #significant; neg correlated

# ANOVA between RT and Percent Diff between reward options
alltrialsdat$perDiffRound <- round(alltrialsdat$perDiff, 2)
alltrialsdat$perDiffFac <- as.factor(alltrialsdat$perDiffRound)
aov4 <- aov(choiceResponse.rt ~ perDiffFac, data = alltrialsdat)
summary(aov4) #significant


# ANOVA for Percent difference between choices and choice type
alltrialsdat$choiceType <- as.factor(alltrialsdat$choiceType)
aov1 <- aov(perDiff ~ choiceType, data = alltrialsdat)
summary(aov1)

#Visualize ANOVA
graph_summary <- ddply(alltrialsdat, c("choiceType"), summarize,
                       AVERAGE=mean(perDiff),
                       SE=sqrt(var(perDiff)/length(perDiff)))

ggplot(data = graph_summary, aes(x = choiceType, y = AVERAGE, colour = choiceType))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Choice Type")+
  scale_y_continuous("Reward Difference (%)")

#Visualize choiceType and amount of reward

graph_summary <- ddply(alltrialsdat, c("choiceType"), summarize,
                       AVERAGE=mean(stealTrial),
                       SE=sqrt(var(stealTrial)/length(stealTrial)))

ggplot(data = graph_summary, aes(x = choiceType, y = AVERAGE, colour = choiceType))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Choice Type")+
  scale_y_continuous("Steal Amount")

# ANOVA for RT for choices and choice type
aov2 <- aov(choiceResponse.rt ~ choiceType, data = alltrialsdat)
summary(aov2)

#Visualize ANOVA
graph_summary <- ddply(alltrialsdat, c("choiceType"), summarize,
                       AVERAGE=mean(choiceResponse.rt),
                       SE=sqrt(var(choiceResponse.rt)/length(choiceResponse.rt)))

ggplot(data = graph_summary, aes(x = choiceType, y = AVERAGE, colour = choiceType))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Choice Type")+
  scale_y_continuous("Choice RT")

#Hierarchical Mixed Models
require(nlme)
#1: DV = choice RT, IVs = reward magnitude; choice type (steal or earn)




fit1 <- lmer(choiceResponse.rt ~ choiceType + (1|participant), data = alltrialsdat)
summary(fit1)

fit2 <- lmer(choiceResponse.rt ~ choiceValue + (1|participant), data = alltrialsdat)
summary(fit2)

fit3 <- lmer(choiceResponse.rt ~ choiceType + choiceValue + (1|participant), data = alltrialsdat)
summary(fit3)


anova(fit1, fit3)   ## not a huge difference between the models
anova(fit1, fit2)  ##model 2 better than 1
anova(fit2, fit3)  ##no difference

## residuals check
plot(fitM6)   ## looks good

allEffects(fitM6)
plot(allEffects(fitM6))  ## plot all main effects 
## note that these means are based on the model after accounting to the random effects
## and are therefore not quite the same as the empirical means (they are "adjusted means", cf. ANCOVA unit)
tapply(surgeryData$Post_QoL, list(surgeryData$Surgery, surgeryData$Reason), mean)

VarCorr(fitM6)          ## extract variance components
coef(fitM6)             ## extract random and fixed coefficients





## Correlations with agg parameter

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/")

model <- read.csv("aggPRO_model_summary.csv")
summarydat$agg <- model$best.agg
summarydat$lm <- model$best.tempr

limesurvey <- read.csv("limesurvey_n8_PRO.csv")
corrDat <- cbind(summarydat, limesurvey)


corrDat$aggLog <- log1p(summarydat$agg)

num <- sapply(corrDat, is.numeric)
numDF <- corrDat[,num]



#Full corr matrix
if (!require("psych")) {install.packages("psych"); require("psych")}
fullCorr <- corr.test(numDF, method="pearson", use="pairwise", adjust="none")

write.csv(fullCorr$r, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO/Development/Experimental Files/Code/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_r.csv", sep = ""), row.names = T)
write.csv(fullCorr$p, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO/Development/Experimental Files/Code/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_p.csv", sep = ""), row.names = T)


#look at trials with low percent diffs and look at indiv variance in stealing vs high perc trials - pinpoint the agg people

# plot

plot(numDF$aggLog, numDF$total_barrat_11, xlab="Aggression Parameter", ylab="BIS-11")
plot(numDF$aggLog, numDF$total_buss_perry, xlab="Aggression Parameter", ylab="Buss Perry Total")
plot(numDF$aggLog, numDF$ppi_sf_total, xlab="Aggression Parameter", ylab="PPI Total")




# # Mean RT for space bar press - FIX
# 
require(reshape)
# 
DF <- data.frame(alltrialsdat$participant, FOO=alltrialsdat$gamePressTimes)
DF1 <- do.call('rbind', strsplit(as.character(DF$FOO),',',fixed=TRUE))
DF2 <- do.call('rbind', strsplit(as.character(DF1),'[',fixed=TRUE))


temp <- tapply(alltrialsdat$gamePressTimes,alltrialsdat$participant,mean, na.rm = T)
DF <- subjDat$gamePressTimes
test <- gsub("[[:punct:]]", "", DF)

foo <- as.matrix(do.call('rbind', strsplit(as.character(subjDat$gamePressTimes),',',fixed=TRUE)))




#temp <- tapply(alltrialsdat$gamePressTimes,alltrialsdat$participant,mean, na.rm = T)

summarydat$meanChoiceRTspace <- meanRTspace

histogram(summarydat$meanChoiceRTspace)
# 
# #Mean # of space bar press
meanSpacePress <- tapply(alltrialsdat$nKeyPress,alltrialsdat$participant,mean, na.rm = T)

summarydat$meanSpacePress <- meanSpacePress

histogram(summarydat$meanSpacePress)
plot(summarydat$meanSpacePress)

# 
#      
#Create summary CSV
summDat <- write.csv(summarydat, file="summarydat.csv")

#Create new alltrialsdat csv with catch trials excluded
alltrialsdatC <- write.csv(alltrialsdat, file ="alltrialsdatC.csv")

