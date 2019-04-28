require(plyr)

#Setwd - task data files
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Experimental Files/PSAP_PRO_A_CONTROL_Task/data/")

temp1 = list.files(pattern ="*.csv")  #PROC
temp <- temp1[grep("PROC008|PROC009|PROC010|PROC011|PROC012|PROC013|PROC014|PROC015|PROC016|PROC017|PROC018|PROC019|PROC020|PROC021|PROC022|PROC024|PROC025|PROC026|PROC027|PROC028|PROC029|PROC030|PROC031|PROC032|PROC033", temp1)]


#temp2 = list.files(pattern = "*.csv")  #PRO double condition version
#temp <- temp2[grep("PRO027|PRO028|PRO029|PRO030|PRO031|PRO032|PRO033|PRO034|PRO035|PRO036|PRO037|PRO039|PRO040|PRO041|PRO042|PRO043|PRO044|PRO045|PRO047|PRO048|PRO049|PRO050|PRO053|PRO055|PRO056|PRO057|PRO058", temp2)]
# exclude 54, 38, 26, 46, 51, 52 because of RA fuckup/not believe deception - this needs to be fixed. excel creates extra columns for some csvs


alltrialsdat <- do.call(rbind.fill,lapply(temp,read.csv)) #combine into all trials, all subjects


alltrialsdat <- alltrialsdat[!is.na(alltrialsdat$stealTrial),] #Remove empty rows
alltrialsdat$choiceType <- as.character(alltrialsdat$choiceType)


# #Look at catch trials
# alltrialsdat$catchType <- ifelse(alltrialsdat$earnTrial >= alltrialsdat$stealTrial,"catch","reg")
# alltrialsdat$catchFlag <- ifelse(alltrialsdat$choiceType == "steal" && alltrialsdat$catchType =="catch", "flag", "fine")
# excludeSubjs <- length(which(alltrialsdat$catchFlag == "flag")) #Check to make sure you don't need to exclude these subjs for not doing task!
# 
# #Remove catch trials from alltrialsdat data frame#
# alltrialsdat <- alltrialsdat[-which(alltrialsdat$catchType=="catch"),]


#setwd - where models will be output
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A_CONTROL/Active/Analyses/")


alltrialsCSV <- write.csv(alltrialsdat, file="alltrials.csv")



