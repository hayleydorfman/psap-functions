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

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROV/")
alltrialsdat <- read.csv("alltrials.csv", row.names = 1, stringsAsFactors = FALSE)

#Create choice value variable

alltrialsdat$choiceValue <- as.numeric(ifelse(alltrialsdat$choiceType=="steal", alltrialsdat$stealTrial, ifelse(alltrialsdat$choiceType=="bank", alltrialsdat$earnTrial,NA))) 


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
#alltrialsdat$catchType <- ifelse(alltrialsdat$earnTrial > alltrialsdat$stealTrial,"catch","reg")
alltrialsdat$catchFlag <- ifelse(alltrialsdat$choiceType == "steal" & alltrialsdat$trialType =="catch|zero", "flag", "fine")
alltrialsdat[which(alltrialsdat$catchFlag == "flag"),] 
#Going to exclude PROC009, PROC012, PROC014,PROC019, PROC020, PROC024
#PROC027, PROC028, PROC030, PROC031, PROC033, but not PROC015 or PROC017 or PROC032 given their single mess up appears accidental given their data
#go back to STEP 1 if you need to exclude and run these analyses again

excludeSubjs <- length(which(alltrialsdat$catchFlag == "flag")) #Check to make sure you don't need to exclude these subjs for not doing task!

#Remove catch trials from alltrialsdat data frame#
alltrialsdat <- alltrialsdat[-which(alltrialsdat$trialType=="catch"),]
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
#histogram(summarydat$missed, xlab="Missed", col="red")

# Plot individual subject steal & earn frequency

choiceFreqPlot <- ggplot((as.data.frame(choiceType)), aes(x=Var2, y = Freq, fill=Var1)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
                                                       name="Choice",
                                                       breaks=c("bank", "steal", "missed"),
                                                       labels=c("Earn", "Steal", "Missed"))
choiceFreqPlot


df<-as.data.frame(choiceType)
LoTRdataOrder <- df[order(df$Var2, df$Freq), ]

test <- df[with(df, order(Freq, Var1)),]

# install.packages("ggplot2", dependencies = TRUE)
require(ggplot2)

p <- ggplot(test, aes(x = Var2, y = Freq, fill = Var1))
p + geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + guides(fill = guide_legend())


#visualize bankers and stealers
choiceFreqPlot2 <- ggplot((as.data.frame(choiceType)), aes(x=Var2, y = Freq)) + geom_bar(stat="identity")+
  xlab("Participant")+
  facet_wrap(~ Var1)
choiceFreqPlot2


# ANOVA for RT for choices and choice type
aov2 <- aov(choiceResponse.rt ~ choiceType, data = alltrialsdat)
summary(aov2) # not significant

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

#Does choice RT predict earn or stealing?
##########


# Corr between RT and Value of choice
corr1 <- cor.test(alltrialsdat$choiceResponse.rt, alltrialsdat$choiceValue, method="pearson", na.omit=TRUE)
plot(alltrialsdat$choiceResponse.rt, alltrialsdat$choiceValue)
corr1 ## not SIGNIFICANT

# With log
alltrialsdat$logChoiceRT <- log(alltrialsdat$choiceResponse.rt)
corr2 <- cor.test(alltrialsdat$logChoiceRT, alltrialsdat$choiceValue, method="pearson", na.omit=TRUE)
plot(alltrialsdat$logChoiceRT, alltrialsdat$choiceValue)
corr2 #NOT SIGNIFICANT


## Let's cheat and use Bayesian model averaging to even see if any of the possible predictors
## would be useful to use in a model
##################
dim(alltrialsdat)
IVs <- alltrialsdat[,-1]
DV <- data[,39]
bma <- bicreg(IVs, DV)
summary(bma)  ## Not really...We will continue anyway with the predictors above

#Mean key press RT
meanKey <- tapply(alltrialsdat$PressTimeDiffMean, alltrialsdat$participant, mean, na.rm=T)

summarydat$meanKey <- meanKey
histogram(summarydat$meanKey, xlab="Mean Key RT", col="red")

#IF YOU WANT TO DO ALL OR NOTHING
summarydat$group <- ifelse(summarydat$steals >= 80, "stealer",(ifelse(summarydat$earns >= 80, "earner", "valuer"))) 
summarydat$group <- as.factor(summarydat$group)

#IF YOU WANT TO DO 80% #
#summarydat$group <- ifelse(summarydat$steals >= 64, "stealer",(ifelse(summarydat$earns >= 64, "earner", "valuer")))  #If they stole 80% or more of the time, 1 = stealer, 2 = earner, 3 = neither
#summarydat$group <- as.factor(summarydat$group)

## Percentage earners and stealers ##

rows <- nrow(summarydat)
table <- table(summarydat$group)

earners <- table[1]/rows #percent of sample that are "earners" 55%
earners
stealers <- table[2]/rows #percent of sample that are "stealers" 45%
stealers
valuers <- table[3]/rows #percent of sample that are "valuers" 0%
valuers
#100% choosing heuristically

#Does Space press RT predict group?

logreg1 <- glm(group ~ meanKey, data = summarydat, family = binomial)
summary(logreg1)  #NOT SIGNIFICANT


summarydat$stealPer <- summarydat$steals/(summarydat$steals+summarydat$earns)
summarydat$earnPer <- summarydat$earns/(summarydat$steals+summarydat$earns)


## Add log mean choice RT to summdat
meanlogChoiceRT <- tapply(alltrialsdat$logChoiceRT, alltrialsdat$participant, mean, na.rm=T)
summarydat$meanlogChoiceRT <- meanlogChoiceRT

## Add mean number key press to summdat
meanNkey <- tapply(alltrialsdat$nKeyPress, alltrialsdat$participant, mean, na.rm=T)
summarydat$meanNkey <- meanNkey


#Create summary CSV
summDat <- write.csv(summarydat, file="summarydatPROV.csv")

#Create new alltrialsdat csv with catch trials excluded
alltrialsdat <- write.csv(alltrialsdat, file ="alltrialsdatPROV.csv")

