
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
    ## comprehensice package for ordinal logistic regression     
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}


# Force R not to use scientific notation
options("scipen" = 100, "digits" = 5)

##MAIN TASK PSAP PROC DESCRIPTIVES - ALL SUBJECTS BEHAV DATA##

#Setwd
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/")

alltrialsdat <- read.csv("alltrials.csv", row.names = 1, stringsAsFactors = FALSE)

#Create choice value variable

alltrialsdat$choiceValue <- as.numeric(ifelse(alltrialsdat$choiceType=="steal", alltrialsdat$stealTrial, ifelse(alltrialsdat$choiceType=="earn", alltrialsdat$earnTrial,NA))) 


participant <- unique(alltrialsdat$participant)

#Create summary df
summarydat <- data.frame(participant)

par(mfrow=c(1,1))
#mean RT choice
meanRT <- tapply(alltrialsdat$choiceResponse.rt,alltrialsdat$participant,mean, na.rm = T)
# 
summarydat$meanChoiceRT <- meanRT
# 
histogram(summarydat$meanChoiceRT, xlab="Mean Choice RT", col="red")
#
plot(summarydat$meanChoiceRT, summarydat$steals)

#Look at catch trials
alltrialsdat$catchType <- ifelse(alltrialsdat$earnTrial >= alltrialsdat$stealTrial,"catch","reg")
alltrialsdat$catchFlag <- ifelse(alltrialsdat$choiceType == "steal" & alltrialsdat$catchType =="catch", "flag", "fine")
alltrialsdat[which(alltrialsdat$catchFlag == "flag"),] #Going to exclude PROCS005, but not PROCS007 given debrief info; go back to STEP 1 if you need to exclude and run these analyses again

excludeSubjs <- length(which(alltrialsdat$catchFlag == "flag")) #Check to make sure you don't need to exclude these subjs for not doing task!

#Remove catch trials from alltrialsdat data frame#
alltrialsdat <- alltrialsdat[-which(alltrialsdat$catchType=="catch"),]
# 
# # number of earns and steals and missed
choiceType <- table(alltrialsdat$choiceType,alltrialsdat$participant)
earns <- choiceType[1,]
summarydat$earns <- earns
steals <- choiceType[3,]
summarydat$steals <- steals
missed <- choiceType[2,]
summarydat$missed <- missed

histogram(summarydat$earns, xlab="Earns", col="red")
histogram(summarydat$steals, xlab="Steals", col="red")
histogram(summarydat$missed, xlab="Missed", col="red")

# Plot individual subject steal & earn frequency

choiceFreqPlot <- ggplot((as.data.frame(choiceType)), aes(x=Var2, y = Freq, fill=Var1)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
                                                       name="Choice",
                                                       breaks=c("bank", "steal", "missed"),
                                                       labels=c("Earn", "Steal", "Missed"))
choiceFreqPlot

#Mean RT for choice type
choiceRT <- data.frame(alltrialsdat$participant, alltrialsdat$choiceResponse.rt)
choiceRT$choiceType <- (alltrialsdat$choiceType)
choiceRT$choiceRT <- alltrialsdat$choiceResponse.rt


#test <- tapply(alltrialsdat$choiceResponse.rt,alltrialsdat$participant, alltrialsdat$choiceType, mean, na.rm = T)

summarydat$meanChoiceRT <- meanRT



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
# g <- function(x,y){
#   x/y
# }
# 
# 
# for (p in alltrialsdat$participant) {
#   perReward <- g(alltrialsdat$earnTrial, alltrialsdat$stealTrial)
# }
# 
# 
# h <- function(x,y){
#   y/x
# }
# 
# for (p in alltrialsdat$participant) {
#   perEarn <- h(alltrialsdat$earnTrial, alltrialsdat$stealTrial)
# }
# 
# alltrialsdat$perReward <- perReward
# alltrialsdat$perEarn <- perEarn
# alltrialsdat$perDiff <- perEarn - perReward
# 
# plot(perDiff, choiceType)
# 
# alltrialsdat$EV <- ifelse(alltrialsdat$choiceType=="bank",alltrialsdat$earnTrial,(ifelse(alltrialsdat$choiceType=="steal",alltrialsdat$stealTrial,NA)))
# 
# plot(alltrialsdat$EV, alltrialsdat$choiceType, ylim=c(15,20))
# 
# p = ggplot(data = alltrialsdat, aes(x = alltrialsdat$EV)) + 
#   geom_density()
# 
# require(plyr)
# dlply(alltrialsdat, .(alltrialsdat$particpant), function(x) p %+% x)
# 
# plots = dlply(alltrialsdat, .(alltrialsdat$particpant), function(x) p %+% x)
# plots[1]
# 
# p = ggplot(data = alltrialsdat, aes(x = alltrialsdat$EV)) + 
#   geom_density()
# 
# x11()
# par(mfrow=c(5,6))
# require(plyr)
# plots = dlply(alltrialsdat, .(alltrialsdat$participant), function(x) p %+% x+ facet_wrap(~participant))
# pdf()
# plots
# dev.off()

plot1 <- ggplot (data = alltrialsdat, aes(x=choiceType, y = EV))+
  geom_boxplot()
x11()
plot1

x11()
plot(alltrialsdat$EV, alltrialsdat$choiceType, ylab = "Detention", xlab = "Intelligence", main = "Intelligence and Detention")
rug(alltrialsdat$EV[alltrialsdat$choiceType == "steal"])
rug(alltrialsdat$EV[alltrialsdat$choiceType == "bank"], side = 3)
axis(4, c(0,1), labels = c("steal", "bank"))

donner <- arrange(alltrialsdat, choiceType)
donner <- ddply(donner, .(trials.thisN, EV), transform, stack = (0:(length(trials.thisN)-1))*0.015)

test <- ggplot(donner, aes(trials.thisN, EV, color = choiceType))
x11()
test
  geom_point(aes(y = abs(EV - stack)))
  #stat_smooth(method = "glm", family = binomial, formula = y ~ poly(x,2))

test2 <- ggplot(data = alltrialsdat, aes(x=participant, y = EV, color = choiceType)) +
  geom_point()
x11()
test2
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

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/")


model <- read.csv("aggPROC_model_summary.csv")
summarydat$agg <- model$best.agg
summarydat$lm <- model$best.tempr

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Subject Data/Qualtrics/")

qualtrics <- read.csv("qualtrics_summary_030716.csv")
corrDat <- cbind(summarydat, qualtrics)


corrDat$aggLog <- log1p(summarydat$agg)

num <- sapply(corrDat, is.numeric)
numDF <- corrDat[,num]



#Full corr matrix
if (!require("psych")) {install.packages("psych"); require("psych")}
fullCorr <- corr.test(numDF, method="pearson", use="pairwise", adjust="none")

write.csv(fullCorr$r, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_r.csv", sep = ""), row.names = T)
write.csv(fullCorr$p, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_p.csv", sep = ""), row.names = T)


#look at trials with low percent diffs and look at indiv variance in stealing vs high perc trials - pinpoint the agg people

# plot

plot(numDF$aggLog, numDF$BISTotal, xlab="Aggression Parameter", ylab="BIS-11")
plot(numDF$aggLog, numDF$BPAQ_Total, xlab="Aggression Parameter", ylab="Buss Perry Total")
plot(numDF$aggLog, numDF$ppi_sf_total, xlab="Aggression Parameter", ylab="PPI Total")

plot(numDF$agg, numDF$BISTotal, xlab="Aggression Parameter", ylab="BIS-11")
plot(numDF$agg, numDF$BPAQ_Total, xlab="Aggression Parameter", ylab="Buss Perry Total")
plot(numDF$aggLog, numDF$ppi_sf_total, xlab="Aggression Parameter", ylab="PPI Total")

## Make agg binary and then do logistic models
numDF$aggBin <- ifelse(numDF$agg >= 1, 1, 0)
numDF$aggBin <- as.factor(numDF$aggBin)

logreg1 <- glm(aggBin ~ BPAQ_Total, data = numDF, family = "binomial")
summary(logreg1)

logreg2 <- glm(aggBin ~ BISTotal, data = numDF, family = "binomial")
summary(logreg2)

# # Mean RT for space bar press - FIX
# 
require(reshape)
# 
df <- data.frame(alltrialsdat$participant, FOO=alltrialsdat$gamePressTimes, na.rm=TRUE)
x <- "[]"
str_replace_all(x, "[[:punct:]]", " ", df)
foo <- df(do.call('rbind', strsplit(as.character(alltrialsdat$gamePressTimes),',',fixed=TRUE)))

temp <- tapply(alltrialsdat$gamePressTimes,alltrialsdat$participant,mean, na.rm = T)
df <- subjDat$gamePressTimes
test <- gsub("[[:punct:]]", "", df)

foo <- as.matrix(do.call('rbind', strsplit(as.character(subjDat$gamePressTimes),',',fixed=TRUE)))




#temp <- tapply(alltrialsdat$gamePressTimes,alltrialsdat$participant,mean, na.rm = T)

#summarydat$meanChoiceRTspace <- meanRTspace

#histogram(summarydat$meanChoiceRTspace)
# 
# #Mean # of space bar press
# meanSpacePress <- tapply(alltrialsdat$nKeyPress,alltrialsdat$participant,mean, na.rm = T)
# 
# corrDat$meanSpacePress <- meanSpacePress
# 
# histogram(corrDat$meanSpacePress)
# plot(corrDat$meanSpacePress)

# 
#      
#Create summary CSV
summDat <- write.csv(corrDat, file="summarydat.csv")

#Create new alltrialsdat csv with catch trials excluded
alltrialsdatC <- write.csv(alltrialsdat, file ="alltrialsdatC.csv")

