## Plot model distribution

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
propChoices$levels <-rep(seq(1:93),2)

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

choicePropPlot <- ggplot((as.data.frame(propChoices)), aes(x=levels, y = Proportion, fill=Choice)) + geom_bar(stat="identity")+
  xlab("Participant")+ylab("% Trials")+ggtitle("Model Choice")+
  scale_x_discrete(breaks=seq(1:186), labels=propChoices$Subject)+
  scale_fill_manual(values = wes_palette("Royal1"), breaks=c("propBank", "propSteal"),labels=c("No Steal", "Steal"))

#Preview looks wonky, just save it as an image with 1200 X 790px

#choiceFreqPlot+mytheme
png("modelchoiceProp.png", width = 12, height = 8, units = 'in', res = 400)
choicePropPlot+mytheme
dev.off()

## Plot probability frequency of steal and earn choices##----------------------------------------------------

# choices2 <- df[,c(2,9,10)]
# choices2$probSteal <- round(choices2$probSteal, digits = 2)
# choices2$probBank <- round(choices2$probBank, digits = 2)
# 
# #unmelt the df
# #choices <- dcast(choices, Var2 ~ Var1)
# 
# #Get some sane names for your variables
# #colnames(choices)[1] <- "Subject"
# #sort the df so that biggest earners come frist
# #if you'd like it to be biggest thieves, order by 'bank' insted of "steal"
# #choices <- choices[order(choices$steal),] 
# 
# 
# #rownames(choices) <- NULL #delete row.names
# 
# #Create proportional columns 
# #choices$propBank <- (choices$bank*100)/(choices$bank+choices$steal)
# #choices$propMissed <- (choices$missed*100)/(choices$bank+choices$steal)
# #choices$propSteal <- (choices$steal*100)/(choices$bank+choices$steal)
# 
# 
# #create a df with prop choices
# 
# #propChoices <- choices[,c('Subject','propBank', 'propSteal')]
# 
# #make sure the order is ok
# 
# freqChoices <- choices2[order(choices2$propSteal),]
# 
# #now that we have an ordered df, let's remelt it
# #choices <- melt(choices, id=(c("Subject")))
# 
# #remelt the propChoices as well
# propChoices <- melt(propChoices, id=(c('Subject')))
# 
# #add a levels variable that we'll use to order a list
# #choices$levels <-rep(seq(1:89),3)
# 
# #add a levels variable that we'll use to order a list
# propChoices$levels <-rep(seq(1:93),2)
# 
# #Get some sane names for your variables
# #colnames(choices)[2:3] <- c("Choice","Frequency")
# 
# #Get some sane names for your variables
# colnames(propChoices)[2:3] <- c("Choice","Proportion")
# 
# #Order the Choice as Factor so it plots pretty
# #choices$Choice = factor(choices$Choice,levels=c("bank", "steal", "missed"),ordered=TRUE)
# 
# 
# 
# #Order the Choice as Factor so it plots pretty
# propChoices$Choice = factor(propChoices$Choice,levels=c("propBank", "propSteal"),ordered=TRUE)
# 
# #Let's set a Theme
# 
# mytheme = theme(
#   panel.border = element_blank()
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
#   ,axis.line = element_line(color ='black')
#   ,axis.text.y = element_text(size=13, family="Helvetica") #you can change fonts here, but some didn't work for me 
#   ,axis.text.x =element_text(size=13, angle=90, vjust=0.5, family="Helvetica")
#   ,axis.title = element_text(size= 18, family="Helvetica")
#   ,title = element_text(size =18, color = "#3F4042", face="bold", family="Helvetica" )
#   ,panel.background = element_rect(fill = "white")
#   ,legend.key.size=unit(2, "cm")
#   ,legend.text = element_text(size=14,family="Helvetica")
#   ,legend.title = element_text(size=16,family="Helvetica")
#   
# )
# 
# 
# 
# 
# 
# 
# # choiceFreqPlot <- ggplot((as.data.frame(choices)), aes(x=levels, y = Frequency, fill=Choice)) + geom_bar(stat="identity")+
# #   xlab("Participant")+ylab("Trials")+
# #   scale_x_discrete(breaks=seq(1:89), labels=choices$Subject)+
# #   scale_fill_manual(values = wes_palette("Royal1"), 
# #                    breaks=c("bank", "steal", "missed"),
# #                    labels=c("Earn", "Steal", "Missed"))
# 
# choicePropPlot <- ggplot((choices2, aes(x=participant, y = Proportion, fill=Choice)) + geom_bar(stat="identity")+
#   xlab("Participant")+ylab("% Trials")+title("Model")+
#   scale_x_discrete(breaks=seq(1:186), labels=propChoices$Subject)+
#   scale_fill_manual(values = wes_palette("Royal1"), breaks=c("propBank", "propSteal"),labels=c("No Steal", "Steal"))
# 
# #Preview looks wonky, just save it as an image with 1200 X 790px
# 
# #choiceFreqPlot+mytheme
# png("modelchoiceProp.png", width = 12, height = 8, units = 'in', res = 400)
# choicePropPlot+mytheme
# dev.off()
