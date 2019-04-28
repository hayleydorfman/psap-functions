
if (!require(wesanderson)) {install.packages("wesanderson"); require(wesanderson)}

if (!require(plyr)) {install.packages("plyr"); require(plyr)}

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")

## PLOT CHOICE FREQUENCY ##

# install.packages("wesanderson")
# 
# #Import Libraries
# library(ggplot2)
# library(gridExtra)
# library(reshape2)
# library(wesanderson)

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
#choices <- melt(choices, id=(c("Subject"))) #for counts
propChoices <- melt(propChoices, id=(c("Subject"))) #for proportions

#choices2 <- subset(choices, choices$variable != "missed")
#propChoices2 <- subset(propChoices, propChoices$variable != "missed")

#add a levels variable that we'll use to order a list
#choices2$levels <-rep(seq(1:89),2)
propChoices2$levels <-rep(seq(1:89),2)





#Get some sane names for your variables
#colnames(choices2)[2:3] <- c("Choice","Frequency")
colnames(propChoices2)[2:3] <- c("Choice","Frequency")


#Order the Choice as Factor so it plots pretty
#choices2$Choice = factor(choices2$Choice,levels=c("bank", "steal"),ordered=TRUE)
propChoices2$Choice = factor(propChoices2$Choice,levels=c("propBank", "propSteal"),ordered=TRUE)


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


# choiceFreqPlot <- ggplot((as.data.frame(choices2)), aes(x=levels, y = Frequency, fill=Choice)) + geom_bar(stat="identity")+
#   xlab("Participant")+ylab("Trials")+ggtitle("Subject Choices")+
#   scale_x_discrete(breaks=seq(1:465))+
#   scale_fill_manual(values = wes_palette("Royal1"),
#                     breaks=c("bank", "steal"),
#                     labels=c("No Steal", "Steal"))


choicePropPlot <- ggplot((as.data.frame(propChoices2)), aes(x=levels, y = Frequency, fill=Choice)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("% Trials")+ggtitle("Subject Choices")+
  scale_x_discrete(breaks=seq(1:465))+
  scale_fill_manual(values = wes_palette("Royal1"),
                    breaks=c("propBank", "propSteal"),
                    labels=c("No Steal", "Steal"))


#Preview looks wonky, just save it as an image with 1200 X 790px
# png("choiceFreqPROV.png", width = 14, height = 8, units = 'in', res = 300)
# choiceFreqPlot+mytheme
# dev.off()

png("choicePropPROV.png", width = 14, height = 8, units = 'in', res = 300)
choicePropPlot+mytheme
dev.off()


## Plot model distribution choice frequency

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")
temp = list.files(pattern="*harmparam_model_fit.csv")

df <- do.call(rbind.fill,lapply(temp,read.csv)) #combine into all trials, all subjects
head(df)
dim(df)

df <- df[!is.na(df$modelChoice),] #Remove empty rows
dim(df)


library(ggplot2)
library(gridExtra)
library(reshape2)
library(wesanderson)


##Plot distriubtion of model steal and earn choices##------------------------------

choices <- table(df$modelChoice, df$participant)
choices <- as.data.frame(choices)
#unmelt the df
choices <- dcast(choices, Var2 ~ Var1)

#Get some sane names for your variables
colnames(choices)[1] <- "Subject"
#sort the df so that biggest earners come frist
#if you'd like it to be biggest thieves, order by 'bank' insted of "steal"
#choices <- choices[order(choices$steal),] 


rownames(choices) <- NULL #delete row.names

#Create proportional columns 
choices$propBank <- (choices$bank*100)/(choices$bank+choices$steal)
#choices$propMissed <- (choices$missed*100)/(choices$bank+choices$steal)
choices$propSteal <- (choices$steal*100)/(choices$bank+choices$steal)


#create a df with prop choices

propChoices <- choices[,c('Subject','propBank', 'propSteal')]

#make sure the order is ok

propChoices <- propChoices[order(propChoices$propSteal),]

#now that we have an ordered df, let's remelt it
#choices <- melt(choices, id=(c("Subject")))

#remelt the propChoices as well
propChoices <- melt(propChoices, id=(c('Subject')))

#add a levels variable that we'll use to order a list
#choices$levels <-rep(seq(1:89),3)

#add a levels variable that we'll use to order a list
propChoices$levels <-rep(seq(1:89),2)

#Get some sane names for your variables
#colnames(choices)[2:3] <- c("Choice","Frequency")

#Get some sane names for your variables
colnames(propChoices)[2:3] <- c("Choice","Proportion")

#Order the Choice as Factor so it plots pretty
#choices$Choice = factor(choices$Choice,levels=c("bank", "steal", "missed"),ordered=TRUE)



#Order the Choice as Factor so it plots pretty
propChoices$Choice = factor(propChoices$Choice,levels=c("propBank", "propSteal"),ordered=TRUE)

#Let's set a Theme

mytheme = theme(
  panel.border = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,axis.line = element_line(color ='black')
  ,axis.text.y = element_text(size=24, family="Helvetica") #you can change fonts here, but some didn't work for me 
  ,axis.text.x =element_text(size=24, angle=90, vjust=0.5, family="Helvetica")
  ,axis.title = element_text(size= 24, family="Helvetica")
  ,title = element_text(size =24, color = "#3F4042", face="bold", family="Helvetica" )
  ,panel.background = element_rect(fill = "white")
  ,legend.key.size=unit(2, "cm")
  ,legend.text = element_text(size=16,family="Helvetica")
  ,legend.title = element_text(size=18,family="Helvetica")
  
)






# choiceFreqPlot <- ggplot((as.data.frame(choices)), aes(x=levels, y = Frequency, fill=Choice)) + geom_bar(stat="identity")+
#   xlab("Participant")+ylab("Trials")+
#   scale_x_discrete(breaks=seq(1:89), labels=choices$Subject)+
#   scale_fill_manual(values = wes_palette("Royal1"), 
#                    breaks=c("bank", "steal", "missed"),
#                    labels=c("Earn", "Steal", "Missed"))

modelPropPlot <- ggplot((as.data.frame(propChoices)), aes(x=levels, y = Proportion, fill=Choice)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("% Trials")+ggtitle("Model Choice")+
  scale_x_discrete(breaks=seq(1:178), labels=propChoices$Subject)+
  scale_fill_manual(values = wes_palette("Royal1"), breaks=c("propBank", "propSteal"),labels=c("No Steal", "Steal"))

#Preview looks wonky, just save it as an image with 1200 X 790px

#choiceFreqPlot+mytheme
png("modelchoiceProp.png", width = 14, height = 8, units = 'in', res = 400)
modelPropPlot+mytheme
dev.off()


## Correlation plots ##

g1 <- ggplot(combdf1, aes(best.agg,ppi_sf_total)) + geom_point(color = "black", size = 2)+stat_smooth(color = "red3")+
  labs(x = "Aggression Parameter", y = "Machiavellian Egocentricity")
png("PPIALL.png", width = 12, height = 8, units = 'in', res = 400)
g1+mytheme
dev.off()


g1 <- ggplot(corrDat4, aes(best.agg,ppi)) + geom_point(color = "black", size = 2)+stat_smooth(color = "red3")+
  labs(x = "Aggression Parameter", y = "Physical Aggression")
png("physaggCorrALL.png", width = 12, height = 8, units = 'in', res = 400)
g1+mytheme
dev.off()


corrDat4$logAgg <- log(corrDat4$best.agg)

#corrDat4$jitter_agg <- jitter(corrDat4$best.agg,5)
#corrDat4$jitter_stab_phys <- jitter(corrDat4$stab_subscore_physical_aggression,1)

g2 <- ggplot(corrDat4, aes(best.agg,stab_subscore_physical_aggression)) + geom_point(color = "black", size = 2)+stat_smooth(color = "red3", se = FALSE)+
  labs(x = "Aggression Parameter", y = "Physical Aggression")
png("physaggCorrALL.png", width = 12, height = 8, units = 'in', res = 400)
g2
dev.off()

#with jitter
g3 <- ggplot(combdf1, aes(best.agg,ppi_sf_total)) + geom_point(fill = "firebrick",size = 3, position = position_jitter(w=0.4, h = 0.4), alpha = 0.5, color = "firebrick")+
  labs(x = "Aggression Parameter", y = "Psychopathy")
#+scale_x_discrete(limits=c("0", "2"))
png("PPITotal.png", width = 12, height = 8, units = 'in', res = 400)
g3+mytheme

g3 <- ggplot(combdf1, aes(best.agg,iri_total_score))+geom_point(fill = "#3F4042",size = 4, position = position_jitter(w=0.2, h = 0.2), alpha = 0.8, color = "#3F4042")+labs(x = "Aggression Parameter", y = "Empathy")
png("IRI.png", width = 12, height = 8, units = 'in', res = 400)
g3+mytheme2
dev.off()


g4 <- ggplot(combdf1, aes(best.agg,sub_verbal_aggression))+geom_point(fill = "#3F4042",size = 4, position = position_jitter(w=0.2, h = 0.2), alpha = 0.8, color = "#3F4042")+labs(x = "Aggression Parameter", y = "Empathy")
png("IRI.png", width = 12, height = 8, units = 'in', res = 400)
g4+mytheme2
dev.off()

cor.test(combdf1$log, combdf1$ppi_sf_total, method=c("spearman"))

#Group the agg param
combdf1$group <- ifelse(combdf1$best.agg >= 1.5, "A3",(ifelse(combdf1$best.agg >= 0.75, "A2", "A1")))
combdf1$group <- as.factor(combdf1$group)


#Valuers only

g3 <- ggplot(corrDat4, aes(best.agg,ppi_sf_mach))+geom_point(fill = "#3F4042",size = 4, position = position_jitter(w=0.2, h = 0.2), alpha = 0.8, color = "#3F4042")+stat_smooth(method = lm,color = "firebrick",se = FALSE)+labs(x = "Aggression Parameter", y = "Psychopathy")
#+scale_x_discrete(limits=c("0", "2"))
png("ppi_finalplot.png", width = 12, height = 8, units = 'in', res = 400)
g3+mytheme2
dev.off()

#Visualize ANOVA - PPI Mach
graph_summary <- ddply(combdf1, c("group"), summarize,
                       AVERAGE=mean(ppi_sf_total),
                       SE=sqrt(var(ppi_sf_total)/length(ppi_sf_total)))

ppiPlot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(angle = 90, size=14))+
  scale_x_discrete("Group")+
  scale_y_continuous("PPI MachEgo")

png("PPImachAOV.png", width = 12, height = 8, units = 'in', res = 300)
ppiPlot+mytheme2
dev.off()


paramGroup <- qplot(combdf1$group, ylim=c(0,80), xlab="Aggression Parameter Group", ylab= "# of Participants")
png("paramGroup.png", width = 12, height = 8, units = 'in', res = 300)
paramGroup+mytheme2
dev.off()


paramCont <- qplot(combdf1$best.agg, geom= "histogram",xlab="Aggression Parameter", ylab= "# of Participants", binwidth=1)
png("paramCont.png", width = 12, height = 8, units = 'in', res = 300)
paramCont+mytheme2
dev.off()

png("paramCont.png", width = 12, height = 8, units = 'in', res = 300)
hist(combdf1$best.agg, xlab="Aggression Parameter", col = "gray90", main = "Distribution of Aggression Parameters")+mytheme2
dev.off()

#My theme 2
mytheme2 = theme(
  panel.border = element_rect(colour = "gray", fill = NA)
  #,panel.grid.major = element_line(colour ='gray')
  #,panel.background = element_rect(fill = 'gray90')
  #,panel.grid.minor = element_line(colour ='white')
  ,axis.line = element_line(color ='black')
  ,axis.text.y = element_text(size=24,vjust = 0.8, family="Helvetica") #you can change fonts here
  ,axis.text.x =element_text(size=24, vjust=0.8, family="Helvetica")
  ,axis.title = element_text(size= 24, family="Helvetica")
  ,title = element_text(size =24, color = "#3F4042", face="bold", family="Helvetica")
  ,panel.background = element_rect(fill = "white")
  #,legend.key.size=unit(2, "cm")
  #,legend.text = element_text(size=14,family="Helvetica")
  #,legend.title = element_text(size=16,family="Helvetica")
  
)





#Boxplots with error bars for all subjs physical aggression

ggplot(corrDat,aes(group, stab_subscore_physical_aggression))+geom_boxplot()

ggplot(corrDatCom,aes(group, stab_subscore_physical_aggression))+geom_point()

#Visualize ANOVA - stab_subscore_social_aggression
graph_summary <- ddply(corrDatCom, c("group"), summarize,
                       AVERAGE=mean(stab_subscore_physical_aggression),
                       SE=sqrt(var(stab_subscore_physical_aggression)/length(stab_subscore_physical_aggression)))

STAB1Plot <- ggplot(data = graph_summary, aes(x = group, y = AVERAGE, colour = group))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  scale_x_discrete("Group")+
  scale_y_continuous("stab_subscore_social_aggression")

png("STAB1AOV.png", width = 12, height = 8, units = 'in', res = 300)
STAB1Plot
dev.off()


