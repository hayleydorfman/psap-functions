if (!require(cluster)) {install.packages("cluster"); require(cluster)}       ## package for clustering methods 
if (!require(mclust)) {install.packages("mclust"); require(mclust)}          ## normal mixture clustering 
if (!require(mix)) {install.packages("mix"); require(mix)}                   ## we need this for imputation
if (!require(flexmix)) {install.packages("flexmix"); require(flexmix)}       ## general mixture models 
if (!require(betareg)) {install.packages("betareg"); require(betareg)}
if (!require(clues)) {install.packages("clues"); require(clues)}
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}
if (!require(miscTools)) {install.packages("miscTools"); require(miscTools)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(wesanderson)) {install.packages("wesanderson"); require(wesanderson)}

#PRO
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PRO/")

datPro <- read.csv("alltrialsdatPRO.csv")
summDatPro <- read.csv("summarydatPRO.csv")
datPro$study <- as.factor(c("PRO"))


PROstealerPer <- (nrow(subset(summDatPro, summDatPro$group == "stealer")))/nrow(summDatPro)
PROearnerPer <- (nrow(subset(summDatPro, summDatPro$group == "earner")))/nrow(summDatPro)
PROvaluerPer <- (nrow(subset(summDatPro, summDatPro$group == "valuer")))/nrow(summDatPro)

#PROA
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROA/")

datProA <- read.csv("alltrialsdatPROA.csv")
summDatProA <- read.csv("summarydatPROA.csv")
datProA$study <- as.factor(c("PROA"))

PROAstealerPer <- (nrow(subset(summDatProA, summDatProA$group == "stealer")))/nrow(summDatProA)
PROAearnerPer <- (nrow(subset(summDatProA, summDatProA$group == "earner")))/nrow(summDatProA)
PROAvaluerPer <- (nrow(subset(summDatProA, summDatProA$group == "valuer")))/nrow(summDatProA)


#PROC
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROC/")

datProC <- read.csv("alltrialsdatPROC.csv")
summDatProC <- read.csv("summarydatPROC.csv")
summDatProC$missed <- 0
datProC$study <- as.factor(c("PROC"))

PROCstealerPer <- (nrow(subset(summDatProC, summDatProC$group == "stealer")))/nrow(summDatProC)
PROCearnerPer <- (nrow(subset(summDatProC, summDatProC$group == "earner")))/nrow(summDatProC)
PROCvaluerPer <- (nrow(subset(summDatProC, summDatProC$group == "valuer")))/nrow(summDatProC)


#PROCS
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROCS/")

datProCS <- read.csv("alltrialsdatPROCS.csv")
summDatProCS <- read.csv("summarydatPROCS.csv")
datProCS$study <- as.factor(c("PROCS"))

PROCSstealerPer <- (nrow(subset(summDatProCS, summDatProCS$group == "stealer")))/nrow(summDatProCS)
PROCSearnerPer <- (nrow(subset(summDatProCS, summDatProCS$group == "earner")))/nrow(summDatProCS)
PROCSvaluerPer <- (nrow(subset(summDatProCS, summDatProCS$group == "valuer")))/nrow(summDatProCS)


#PRO SWITCH
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/SWITCH/")

datProSW <- read.csv("alltrialsdatSWITCH.csv")
summDatProSW <- read.csv("summarydatSWITCH.csv")

## Combine into a STUDY DF ##
blag <- rbind(PROearnerPer,PROAearnerPer,PROCearnerPer,PROCSearnerPer)

blag2 <- rbind(PROstealerPer,PROAstealerPer,PROCstealerPer,PROCSstealerPer)
blag3 <- rbind(PROvaluerPer,PROAvaluerPer,PROCvaluerPer,PROCSvaluerPer)

study <- c("PRO", "PROA", "PROC", "PROCS")
summStudy <- data.frame(study)
summStudy$earnerPer <- blag[,1]
summStudy$stealerPer <- blag2[,1]
summStudy$valuerPer <- blag3[,1]

#Combine summary DFs

#setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/")

combdf <- rbind(summDatProCS,summDatProC,summDatProA,summDatPro)


#Combine alltrials DFs
datPro1 <- datPro[,c(2,3,17,18,19,22,26,30,109:114)]
dim(datPro1)

datProA1 <- datProA[,c(2,3,16,17,18,21,25,29,91:96)]
dim(datProA1)

datProC1 <- datProC[,c(2,3,16,17,18,21,25,29,101:106)]
dim(datProC1)

datProCS1 <- datProCS[,c(2,3,16,17,18,21,27,31,91:96)]
dim(datProCS1)


totalAlltrials <- rbind(datProCS1,datProC1,datProA1,datPro1)
dim(totalAlltrials)
totalAlltrials <- totalAlltrials[-which(totalAlltrials$choiceType == "missed"),] #remove missed level

## Write out new combined all trials and summary csvs

setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/")
write.csv(totalAlltrials, "totalAlltrials.csv")
write.csv(combdf, "combSummary.csv")


---------------- ### CLUSTER ANALYSES ### ------------------------------------


## Cluster with MIXTOOLS ##
# library(mixtools)
# 
# # 
# # model <- normalmixEM(x=combdf$steals, k=3)
# # index.lower <- which.min(model$mu)  # Index of component with lower mean
# # 
# # find.cutoff <- function(proba=0.5, i=index.lower) {
# #   ## Cutoff such that Pr[drawn from bad component] == proba
# #   f <- function(x) {
# #     proba - (model$lambda[i]*dnorm(x, model$mu[i], model$sigma[i]) /
# #                (model$lambda[1]*dnorm(x, model$mu[1], model$sigma[1]) + model$lambda[2]*dnorm(x, model$mu[2], model$sigma[2])))
# #   }
# #   return(uniroot(f=f, lower=-10, upper=10)$root)  # Careful with division by zero if changing lower and upper
# # }
# # 
# # cutoffs <- c(find.cutoff(proba=0.5), find.cutoff(proba=0.75))  # Around c(1.8, 1.5)
# # 
# # hist(x)
# # abline(v=cutoffs, col=c("red", "blue"), lty=2)
# 
# #With MIX TOOLS #
# wait = combdf$stealPer
# mixmdl = normalmixEM(wait)
# plot(mixmdl,which=2)
# lines(density(wait), lty=2, lwd=2)
# 
# #WITH CLUES#
# 
# res.sil <- clues(combdf$stealPer, strengthMethod = "sil", disMethod = "Euclidean")
# summary(res.sil)
# plotClusters(combdf, res.sil$mem, plot.dim = c(1, 4, 9, 10),cex.points = 0.01)
# 
# ## MCLUST
# mixfit <- Mclust(combdf[,9])
# mixfit$G(2:10)
# summary(mixfit)
# plot(combdf$stealPer,xlab='Participants',ylab="Proportion of Steal Choices",col=c("red","orange","yellow","blue","green")[mixfit$classification])
# plot(mixfit,what = c("BIC", "classification"))
# 
# #G=2:10; a quick start to mclust; pull out mclust BIC
# #or hierarchical cluster analysis with hclust
# #flexmix - specify no predictor (~1)
# 
# #FLEXMIX
# set.seed(210485)
# fmstep <-  flexmix(combdf$stealPer ~ 1, k = 5, model = FLXMCmvnorm())
# fmstep
# plot(1:5, BIC(fmstep), type = "b", pch = 19, xaxt = "n", xlab = "Number of Clusters", ylab = "BIC", main = "BIC Cluster Evaluation")
# axis(1, at = 1:5, labels = 1:5)
# fm <- getModel(fmstep, "BIC")               ## minimum BIC
# fm                                          ## cluster solution
# round(modeltools::posterior(fm), 2)         ## posterior probabilities
# fm@cluster                                  ## cluster assignments
# 
# #BETAMIX
# 
# set.seed(0)
# m <- betamix(stealPer ~ 1 | 1, data = combdf, k = 1:5)
# 
# 
# ##HCLUST
# 
# 
# png("hclust.png", width = 12, height = 8, units = 'in', res = 300)
# plot(hclust(dist(combdf["stealPer"])))
# dev.off()
# 
# #PVCLUST#
# library(pvclust)
# 
# fit <- pvclust(t(combdf), method.hclust="ward.D2",
#                method.dist="euclidean")
# 
# ## plotting the dendrogram
# x11()
# plot(fitpv, cex = 0.8)
# pvrect(fitpv, alpha = 0.95)  ## automatically detects the number of clusters for which p > .95
# 
# ## cluster memberships
# memb <- pvpick(fitpv, alpha = 0.95)$clusters
# memb


---------------------------------- ### ------------------------------------------------------


## PLOT CHOICE FREQUENCY ##

install.packages("wesanderson")

#Import Libraries
library(ggplot2)
library(gridExtra)
library(reshape2)
library(wesanderson)

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")

totalAlltrials <- read.csv("totalAlltrials.csv")
choices <- table(totalAlltrials$choiceType,totalAlltrials$participant)
#prop <- prop.table(choices)
choices <- as.data.frame(choices)



#unmelt the df
choices <- dcast(choices, Var2 ~ Var1)

#names for your variables
colnames(choices)[1] <- "Subject"
#sort the df so that biggest earners come first
#if you'd like it to be biggest thieves, order by 'bank' insted of "steal"
choices <- choices[order(choices$steal),] 


rownames(choices) <- NULL #delete row.names

### If you want this graph to be proportions##--------------------------------
#Create proportional columns 
choices$propBank <- (choices$bank*100)/(choices$bank+choices$missed+choices$steal)
#choices$propMissed <- (choices$missed*100)/(choices$bank+choices$missed+choices$steal)
choices$propSteal <- (choices$steal*100)/(choices$bank+choices$missed+choices$steal)


#create a df with prop choices

propChoices <- choices[,c('Subject','propBank', 'propSteal')]

#make sure the order is ok

propChoices <- propChoices[order(propChoices$propSteal),]
###----------------------------------------#####







#now that we have an ordered df, let's remelt it
choices <- melt(choices, id=(c("Subject")))


choices2 <- subset(choices, choices$variable != "missed")
#add a levels variable that we'll use to order a list
choices2$levels <-rep(seq(1:89),2)





#Get some sane names for your variables
colnames(choices2)[2:3] <- c("Choice","Frequency")
#Order the Choice as Factor so it plots pretty
choices2$Choice = factor(choices2$Choice,levels=c("bank", "steal"),ordered=TRUE)


#Let's set a Theme

mytheme = theme(
  panel.border = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,axis.line = element_line(color ='black')
  ,axis.text.y = element_text(size=24, family="Helvetica") #you can change fonts here
  ,axis.text.x =element_text(size=24, vjust=0.5, family="Helvetica")
  ,axis.title = element_text(size= 24, family="Helvetica")
  ,title = element_text(size =24, color = "#3F4042", face="bold", family="Helvetica" )
  ,panel.background = element_rect(fill = "white")
  ,legend.key.size=unit(2, "cm")
  ,legend.text = element_text(size=16,family="Helvetica")
  ,legend.title = element_text(size=18,family="Helvetica")
  
)



#choiceFreqPlot <- ggplot((as.data.frame(choices))+geom_bar(aes(y = (..count..)/sum(..count..))) +
                           #scale_y_continuous(labels=percent)


choiceFreqPlot <- ggplot((as.data.frame(choices2)), aes(x=levels, y = Frequency, fill=Choice)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("Trials")+ggtitle("Subject Choices")+
  scale_x_discrete(breaks=seq(1:465))+
  scale_fill_manual(values = wes_palette("Royal1"),
                    breaks=c("bank", "steal"),
                    labels=c("No Steal", "Steal"))

#Preview looks wonky, just save it as an image with 1200 X 790px
png("choiceFreqPROV.png", width = 14, height = 8, units = 'in', res = 300)
choiceFreqPlot+mytheme
dev.off()

# # Read in agg param
# agg<- read.csv("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/hyper_model3_agg_comb_model_summary.csv")
# dim(agg)
# aggDat <- merge(agg,combdf, by = "participant")
# #Hist 1
# ggplot(data=aggDat, aes(aggDat$best.agg)) + 
#   geom_histogram(breaks=seq(0, 2, by =.01), 
#                  col="blue", 
#                  aes(fill=..count..))
# #Hist
# ggplot(data=aggDat, aes(x=aggDat$best.agg, y = ..density..)) + 
#   geom_histogram(bins = 25,
#                  col="blue")+mytheme
# 
# #Hist3 - USE?
# hist3 <- ggplot(data=aggDat, aes(x=aggDat$best.agg), colour = wes_palette("Royal1")) + 
#   geom_histogram(bins = 40, col="red")+ xlab("Aggression Parameter")+ylab("Frequency")+ggtitle("Model Fit")
# 
# png("agg_param_hist3.png", width = 12, height = 8, units = 'in', res = 300)
# hist3
# dev.off()
# 
# #Bar plot for agg param
# tableDat <- table(aggDat$best.agg, aggDat$participant)
# ggplot(as.data.frame(tableDat), aes(x=Var1, y=Freq)) + geom_bar(stat = "identity")+mytheme
# 
# ggplot(aggDat, aes(x=aggDat$best.agg)) + stat_bin(geom="bar", binwidth = 0.20)+mytheme
# 
# ggplot(aggDat, aes(x=aggDat$best.agg)) + geom_freqpoly(binwidth = 0.08)+mytheme
# 
# ggplot(data=aggDat, aes(x=aggDat$best.agg)) + geom_bar(binwidth = .005)
# 
# aggDat$Reorder <- reorder(aggDat$participant, aggDat$best.agg)
# ggplot(aggDat, aes(y=best.agg))+geom_bar(aes(x=Reorder), data=aggDat, stat="identity")
# 
# 
# #ggplot(aggDat, aes(y=best.agg))+geom_bar(aes(x=participant), data=aggDat, stat="identity")
# 
# 
# #ggplot(as.data.frame(tableDat), aes(x=Var1, y=Freq) + geom_bar(stat = "identity"))
# 
# setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/")
# write.csv(aggDat, "aggDatOrder.csv")
# aggDatRe <- read.csv("aggDatreOrder.csv")
# 
# aggDatRe2 <- table(aggDatRe$best.agg,aggDatRe$participant)
# aggDatRe2 <- as.data.frame((aggDatRe2))
# 
# #Works but need to bin 
# hist4 <- ggplot(aggDatRe2, aes(y=Freq))+geom_bar(aes(x=Var1), data=aggDatRe2, stat = "identity")+
#   xlab("Aggression Parameter")+ylab("Frequency")+ggtitle("Parameter Distribution")+
#   scale_x_discrete(breaks=seq(1:60))+mytheme
# 
# png("agg_param_hist_final.png", width = 12, height = 8, units = 'in', res = 400)
# hist4
# dev.off()
# 
# 
# ggplot(aggDatRe2, aes(y=Freq))+geom_bar(aes(x=Var2), fill = Var1, data=aggDatRe2, stat = "identity")+
#   xlab("Aggression Parameter")+ylab("Frequency")+ggtitle("Parameter Distribution")+
#   scale_x_discrete(breaks=seq(1:60))+mytheme
# 
# 
# 
# 
# 
# #Add study variable
# 
# ###to all trials
# totalAlltrials$study <- ifelse((grepl("PRO0",totalAlltrials$participant)),"PRO",ifelse((grepl("PROC0",totalAlltrials$participant)),"PROC",ifelse((grepl("PROCS0",totalAlltrials$participant)),"PROCS",ifelse((grepl("PROA",totalAlltrials$participant)),"PROA",NA))))
# 
# ###to summary dat
# ###to all trials
# combdf$study <- ifelse((grepl("PRO0",combdf$participant)),"PRO",ifelse((grepl("PROC0",combdf$participant)),"PROC",ifelse((grepl("PROCS0",combdf$participant)),"PROCS",ifelse((grepl("PROA",combdf$participant)),"PROA",NA))))
# 
# 
# #Plot number of steals & earns by study - using all trials dat
# choiceFreqPlot <- ggplot(totalAlltrials, aes(x=study, fill=choiceType)) + geom_bar(stat="count")+
#   xlab("Study")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
#                                                        name="Choice",
#                                                        breaks=c("bank", "steal", "missed"),
#                                                        labels=c("Earn", "Steal", "Missed"))
# choiceFreqPlot
# 
# #Plot stealers and earners by study - using summary dat
# choiceFreqPlot <- ggplot(combdf, aes(x=study, fill=group)) + geom_bar(stat="count")+
#   xlab("Study")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
#                                                        name="Choice",
#                                                        breaks=c("stealer", "earner", "valuer"),
#                                                        labels=c("Stealer", "Earner", "Valuer"))
# choiceFreqPlot
# 
# ## Regression ##
# reg1 <- lm(stealPer ~ meanKey, data = combdf)
# summary(reg1) #NOT SIG
# 
# reg1b <- glm(stealPer ~ ppi_sf_mach + sub_perseverance + stab_subscore_social_aggression +
#                stab_subscore_physical_aggression  + dospert_perceived_risk_soc, data = corrDat4,
#              family = poisson())
# summary(reg1b)
# 
# reg2 <- glm(stealPer ~ meanChoiceRT, data = combdf)
# reg2b <- glm(stealPer ~ meanChoiceRT, data = combdf, family = poisson())
# hist(predict(reg2, type = "response"))
# 
# summary(reg2) #NOT SIG
# 
# reg3 <- glm(dum_group ~ meanlogChoiceRT, data = combdf, family = binomial)
# summary(reg3)
# 
# reg4 <- glm(stealPer ~ meanlogChoiceRT, data = combdf, family = binomial)
# summary(reg4)
# 
# reg4 <- glm(stealPer ~ meanlogChoiceRT, data = combdf, family = binomial(link="logit"))
# summary(reg4)
# 
# reg4 <- glm(stealPer ~ meanKey, data = combdf, family = binomial(link="logit"))
# summary(reg4)
# 
# reg5 <- glm(stealPer ~ meanNkey, data = combdf, family = binomial(link="logit"))
# summary(reg5)
# 
# #BETA REGRESSION#
# #Can't use because I have true zero's in my data and must be >0
# if (!require("pscl")) {install.packages("pscl"); require("pscl")}           ## count regression tools
# if (!require("betareg")) {install.packages("betareg"); require("betareg")}  ## beta regression package
# 
# ## let's check out the beta distribution
# bpar <- fitdistr(combdf$stealPer, densfun = "beta", start = list(shape1 = 0.7*11, shape2 = 0.3*11))
# xvals <- seq(0, 1, 0.01)
# densvals <- dbeta(xvals, shape1 = bpar$estimate[1], shape2 = bpar$estimate[2])
# densvals
# hist(accuracy, breaks = 10, freq = FALSE)
# lines(xvals, densvals, col = "red")
# 
# ## Let's fit the beta regression
# fitBeta1 <- betareg(combdf$stealPer ~ combdf$meanlogChoiceRT)
# summary(fitBeta1)
# #---------------------------------------------------------------#
# 
# #Plot proportion of types of people in each study
# summStudy1 <- summStudy[c(1:4),]
# 
# dfm <- melt(summStudy1[,c('study','earnerPer','stealerPer', 'valuerPer')],id.vars = 1)
# 
# choiceFreqPlot <- ggplot(dfm, aes(x=study, y =value, fill = variable)) + geom_bar(stat="identity")+
#   xlab("Study Version")+ylab("Proportion")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
#                                                  name="Group",
#                                                  breaks=c("valuerPer","stealerPer" ,"earnerPer"),
#                                                  labels=c("Valuer","Stealer", "Earner")) + scale_x_discrete(labels = c("1\n(n=35)", "2\n(n=19)", "3\n(n=15)", "4\n(n=24)"))
# 
# png("prop_study_group.png", width = 12, height = 8, units = 'in', res = 300)
# choiceFreqPlot+mytheme
# dev.off()
# 
# ## Group differences between study versions
# 
# boxplot(combdf$stealPer~combdf$study)
# 
# fit1 <- lm(formula = combdf$stealPer~combdf$study)
# anova(fit1) #NO SIG DIFFERENCE BETWEEN STUDIES
# 
# fit1a <- lm(formula = combdf$earnPer~combdf$study)
# anova(fit1a) #NO SIG DIFFERENCE BETWEEN STUDIES
# 
# library(plyr)
# library(dplyr)
# #Visualize ANOVA
# graph_summary <- ddply(combdf, c("study"), summarize,
#                        AVERAGE=mean(earnPer),
#                        SE=sqrt(var(earnPer)/length(earnPer)))
# 
# earnPlot <- ggplot(data = graph_summary, aes(x = study, y = AVERAGE, colour = study))+
#   geom_point()+
#   geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(angle = 90, size=14))+
#   scale_x_discrete("Study Version")+
#   scale_y_continuous("Proportion Earn Choices")
# 
# png("earnPlotAOV.png", width = 12, height = 8, units = 'in', res = 300)
# earnPlot
# dev.off()
# 
# graph_summary <- ddply(combdf, c("study"), summarize,
#                        AVERAGE=mean(stealPer),
#                        SE=sqrt(var(stealPer)/length(stealPer)))
# stealPlot <- ggplot(data = graph_summary, aes(x = study, y = AVERAGE, colour = study))+
#   geom_point()+
#   geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(angle = 90, size=14))+
#   scale_x_discrete("Study Version")+
#   scale_y_continuous("Proportion Earn Choices")
# 
# png("stealPlotAOV.png", width = 12, height = 8, units = 'in', res = 300)
# stealPlot
# dev.off()
# 
# #Correlation between mean choice RT and choices
# corr1 <- cor.test(combdf$meanChoiceRT,combdf$steals)
# corr1 #STEALS - NOT SIGNIFICANT
# 
# corr2 <- cor.test(combdf$meanChoiceRT,combdf$earns)
# corr2 #EARNS - NOT SIGNIFICANT
# 
# #Regression with mean choice RT
# reg1 <- glm(combdf$stealPer ~ combdf$meanChoiceRT)
# summary(reg1) #NOT SIGNIFICANT
# 
# 
# ## Dummy code group var ##
# combdf <- within(combdf, {
#   dum_group <- C(group, treatment,2)
#   print(attributes(group))
# })
# 
# ml2 <- mlogit.data(combdf, choice = "dum_group", shape = "wide", varying = NULL)  ## requires the data in wide format
# ml2
# fit.mlogit <- mlogit:::mlogit(prog2 ~ 0 | ses + write, reflevel = "academic", data = ml2)  ## note: conflict with VGAM - there are two mlogit functions in different packages, so this is a way to force R to specify
# summary(fit.mlogit)   ## same as above
# 
# exp(cbind(as.vector(coef(fit.mlogit)), confint(fit.mlogit)))  ## OR parameterization
# 
# ### PLOT
# p <- ggplot(combdf, aes(factor(group), logChoiceRT))+geom_boxplot()
# p+mytheme
# 
# #Differences between study versions
# library(lme4)
# ## random-intecept for subjects
# fit2 <- glmer(choiceType ~ study + (1|participant), data = totalAlltrials, family = binomial)
# 
# with(combdf, boxplot(stealPer ~ study, col = c("white", "lightgray"), main = "Steal Percentage by Study Version"))
# with(combdf, boxplot(earnPer ~ study, col = c("white", "lightgray"), main = "Steal Percentage by Study Version"))
# 
# fit3 <- glmer(stealPer ~ study + (1|participant), data - combdf)
# 

## RT ANALYSES ##
corrRT1 <- cor.test(combdf$meanKey,combdf$steals)
corrRT1

corrRT2 <- cor.test(totalAlltrials$choiceValue,totalAlltrials$nKeyPress)
corrRT2
# Read in MCQ k's
mcq<- read.csv("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/mcq_k.csv")
dim(mcq)
aggDat$mcq <- mcq$k[match(aggDat$participant,mcq$participant)]

# Read in agg param
#agg<- read.csv("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/hyper_model3_agg_comb_model_summary.csv")
#dim(agg)
#aggDat <- merge(agg,combdf, by = "participant")

## Corr with trait measures ##
library(psych)
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/")
corrDat <- read.csv("PSAP_inclusion_2ndyr_survey.csv")

#age <- read.csv("age.csv")
corrDat$participant <- corrDat$study_id

corrDat <- merge(corrDat, aggDat, by = "participant")

corrDat$participant %in% combdf$participant
setdiff(corrDat$study_id,combdf$participant)
setdiff(combdf$participant,corrDat$study_id)


#corrDat1 <- corrDat[-which(corrDat$study_id == "PROC012"),]
#corrDat1 <- corrDat1[-which(corrDat1$study_id == "PROC014"),]
#corrDat1 <- corrDat1[-which(corrDat1$study_id == "PROC030"),]
combdf1 <- combdf[-which(combdf$participant == "PROA013"),]
corrDat$participant <- corrDat$study_id

corrDat3 <- merge(corrDat,combdf1,by = "participant")
names(corrDat3)
corrDat4 <- corrDat3[,c(23:66,79:80,82:86,88:92)]

#aggDat <- aggDat[-which(aggDat$participant == "PROA013"),]

#corrDat4$bestAgg <- aggDat$best.agg

#corr1 <- corr.test(corrDat4, corrDat4, use = "pairwise", adjust = "hochberg")
#write.csv(corr1$r, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_r.csv", sep = ""), row.names = T)
#write.csv(corr1$p, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_p.csv", sep = ""), row.names = T)

#corr2 <- corr.test(corrDat4, corrDat4, use = "pairwise", adjust = "none")
#write.csv(corr2$r, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_uncorr_r.csv", sep = ""), row.names = T)
#write.csv(corr2$p, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_uncorr_p.csv", sep = ""), row.names = T)

corr3 <- corr.test(corrDat4, corrDat4, method = "spearman", use = "pairwise", adjust = "none")
write.csv(corr3$r, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_uncorr_all_spear_r.csv", sep = ""), row.names = T)
write.csv(corr3$p, file=paste("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/Combined/", format(Sys.time(), "%a_%b_%d_%H:%M:%S_%Y"), "_allCorrs_PSAP_uncorr_all_spear_p.csv", sep = ""), row.names = T)

plot(corrDat4$total_buss_perry,corrDat4$best.agg)
plot(corrDat4$stab_subscore_physical_aggression,corrDat4$best.agg)
plot(corrDat4$ppi_sf_mach,corrDat4$best.agg)
plot(corrDat4$stab_subscore_social_aggression,corrDat4$best.agg)
plot(corrDat4$sub_perseverance,corrDat4$best.agg)


##ANOVA for group differences and trait measures ###

corrDat$ppi_sf_mach <- as.numeric(corrDat$ppi_sf_mach)

fitA <- lm(formula = corrDat$ppi_sf_mach~corrDat$group)
anova(fitA) #SIG DIFFERENCE BETWEEN Groups

fitB <- lm(formula = corrDat$sub_physical_aggression~corrDat$group)
anova(fitB) #NO SIG DIFFERENCE BETWEEN Groups

fitC <- lm(formula = corrDat$sub_verbal_aggression~corrDat$group)
anova(fitC) #NO SIG DIFFERENCE BETWEEN Groups

fitD <- lm(formula = corrDat$stab_subscore_social_aggression~corrDat$group)
anova(fitD) #NO SIG DIFFERENCE BETWEEN Groups

fitE <- lm(formula = corrDat$stab_subscore_physical_aggression~corrDat$group)
anova(fitE) #NO SIG DIFFERENCE BETWEEN Groups

fitF <- lm(formula = corrDat$total_buss_perry~corrDat$group)
anova(fitF) #NO SIG DIFFERENCE BETWEEN Groups

corrDatCom <- corrDat[complete.cases(corrDat),]

corrDatEarners <- corrDat[-(corrDat$group=="earner"),]  #no earners - s&v
corrDatEarners$groupBin <- ifelse(corrDatEarners$group=="stealer",1,0)

corrDatStealers <- corrDat[-(corrDat$group=="stealer"),]  #no stealers - v&e
corrDatStealers$groupBin <- ifelse(corrDatStealers$group=="earner",1,0)

corrDatValuers <- corrDat[-(corrDat$group=="valuer"),]  #no valuers - s&e
corrDatValuers$groupBin <- ifelse(corrDatValuers$group=="earner",1,0)




corrDatEarners$sub_physical_aggression <- as.numeric(corrDatEarners$sub_physical_aggression)

t1 <- t.test(corrDatEarners$stab_subscore_physical_aggression ~ corrDatEarners$groupBin, paired = FALSE, var.equal = TRUE)
t1 #NOT SIG between stealers and valuers

t2 <- t.test(corrDatStealers$stab_subscore_physical_aggression ~ corrDatStealers$groupBin, na.action = na.exclude, paired = FALSE, var.equal = TRUE)
t2 #NOT SIG between earners and valuers


t3 <- t.test(corrDatValuers$stab_subscore_physical_aggression ~ corrDatValuers$groupBin, na.action = na.omit, paired = FALSE, var.equal = TRUE)
t3 #SIG between stealers and earners



t4 <- t.test(corrDatValuers$total_buss_perry ~ corrDatValuers$groupBin, na.action = na.omit, paired = FALSE)
t4 #SIG between stealers and earners

t5 <- t.test(corrDatValuers$total_buss_perry ~ corrDatValuers$groupBin, na.action = na.omit, paired = FALSE)
t5 #SIG between stealers and earners





#Visualize ANOVA - PPI Mach
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(ppi_sf_mach),
                       SE=sqrt(var(ppi_sf_mach)/length(ppi_sf_mach)))

ppiPlot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("PPI MachEgo")

png("PPImachAOV.png", width = 12, height = 8, units = 'in', res = 300)
ppiPlot
dev.off()

#Visualize ANOVA - BPAQ Phys
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(sub_physical_aggression),
                       SE=sqrt(var(sub_physical_aggression)/length(sub_physical_aggression)))

BPAQ1Plot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("BPAQ Phys")

png("BPAQphysAOV.png", width = 12, height = 8, units = 'in', res = 300)
BPAQ1Plot
dev.off()


#Visualize ANOVA - BPAQ Verb
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(sub_verbal_aggression),
                       SE=sqrt(var(sub_verbal_aggression)/length(sub_verbal_aggression)))

BPAQ2Plot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("BPAQ Verb")

png("BPAQverbAOV.png", width = 12, height = 8, units = 'in', res = 300)
BPAQ2Plot
dev.off()

#Visualize ANOVA - stab_subscore_social_aggression
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(stab_subscore_social_aggression),
                       SE=sqrt(var(stab_subscore_social_aggression)/length(stab_subscore_social_aggression)))

STAB1Plot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("stab_subscore_social_aggression")

png("STAB1AOV.png", width = 12, height = 8, units = 'in', res = 300)
STAB1Plot
dev.off()


#Visualize ANOVA - stab_subscore_physical_aggression
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(stab_subscore_physical_aggression),
                       SE=sqrt(var(stab_subscore_physical_aggression)/length(stab_subscore_physical_aggression)))

STAB2Plot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("stab_subscore_physical_aggression")

png("STAB2AOV.png", width = 12, height = 8, units = 'in', res = 300)
STAB2Plot
dev.off()

#Visualize ANOVA -buss_perry_total
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(buss_perry_total),
                       SE=sqrt(var(buss_perry_total)/length(buss_perry_total)))

BPAQtotPlot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("BPAQ Total")

png("BPAQtotAOV.png", width = 12, height = 8, units = 'in', res = 300)
BPAQtotPlot
dev.off()


#steal <- subset(totalAlltrials,choiceType == 'steal')
#earn <- subset(totalAlltrials,choiceType == 'bank') 

#totalAlltrials$stealOrder <- factor(totalAlltrials$participant, levels = totalAlltrials$participant[order(totalAlltrials$choiceType)])
#totalAlltrials$earnOrder <- factor(totalAlltrials$participant, levels = totalAlltrials$participant[order(earn)])
# choices <- table(totalAlltrials$choiceType,totalAlltrials$participant)
# choicesDF <- as.data.frame((choices))
# 
# bankDF <- choicesDF[choicesDF$Var1=="bank",]
# stealDF <- choicesDF[choicesDF$Var1=="steal",]
# 
# 
# stealDF2 <- stealDF[order(stealDF[,3]),] #Reorder by steal frequency
# 
# #Overlapping histogram for all subjs
# all <- rbind(bankDF,stealDF2)
# 
# #ggplot(all,aes(x=Var2), stat = "identity") + 
#  # geom_bar(data = bankDF,fill = "red", alpha = 0.2) +
#   #geom_bar(data = stealDF,fill = "blue", alpha = 0.2)
# 
# ############################################################
# #Overlap barplot for choices for all subjs, for steal and bank
# 
# 
# barplot(sort(bankDF$Freq),col = c("steelblue2"), legend = c("Steal", "Earn"), 
#         args.legend = list(title = "Choice Type", x = "bottomright", cex = .5), ylim = c(0, 90))
# text(bp, 0, round(femaleses, 1),cex=.8,pos=2))
# barplot(sort(stealDF$Freq, decreasing = T), add = T,
#         col = c("snow4"),
#         xlab = "Participant", 
#         ylab = "Trial")
# 
# ############################################################
# #ggplot(totalAlltrials,aes(x=participant, y = ), stat = "identity") + 
# #  geom_bar(fill = totalAlltrials$choiceType)
# 
# # x$name <- factor(x$name, levels = x$name[order(x$val)])
# # x$name  # notice the changed order of factor levels
# # 
# # #choicesDF$Var1 <- factor(choicesDF$Var1, levels = choicesDF$Var1[order(choicesDF$Freq)])
# # choices <- write.csv(choicesDF,"choices_annie.csv")
# # 
# choiceFreqPlot <- ggplot(test1, aes(x=Var2, y = Freq, fill=Var1)) + geom_bar(stat="identity")+
#    xlab("Participant")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
#                                                         name="Choice",
#                                                         breaks=c("bank", "steal", "missed"),
#                                                        labels=c("Earn", "Steal", "Missed"))
# choiceFreqPlot
# 
# choiceFreqPlot <- ggplot(test1, aes(x=Var2, y = Freq, fill=Var1)) + geom_histogram(position="fill", stat = "identity")+
#   xlab("Participant")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "snow3", "steelblue3"), 
#                                                        name="Choice",
#                                                        breaks=c("bank", "steal", "missed"),
#                                                        labels=c("Earn", "Steal", "Missed"))
# choiceFreqPlot
# # 
# # 
# # 
# # 
# # choiceFreqPlot <- ggplot(test2, aes(reorder(value, Var2), value)) + geom_bar(stat="identity")+
# #   xlab("Participant")+ylab("Trials")+scale_fill_manual(values=c("steelblue4", "steelblue3", "blue"), 
# #                                                        name="Choice",
# #                                                        breaks=c("bank", "steal", "missed"),
# #                                                        labels=c("Earn", "Steal", "missed"))
# # choiceFreqPlot
# # 
# 
# 
# library(reshape2)
# 
# data_wide <- dcast(choicesDF, Var2 ~ Var1, value.var="Freq")
# data_wide
# write.csv(data_wide, "data_wide_annie.csv")
# data_ordered <- read.csv("data_wide_ordered.csv")

