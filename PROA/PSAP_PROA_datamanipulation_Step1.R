require(plyr)

#Setwd - task data files
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_A/Task/Active/Experimental Files/PSAP_PRO_A_Task/data/")

temp1 = list.files(pattern ="*.csv")  #PROA single condition version


#temp2 = list.files(pattern = "*.csv")  #PRO double condition version
#temp <- temp2[grep("PRO027|PRO028|PRO029|PRO030|PRO031|PRO032|PRO033|PRO034|PRO035|PRO036|PRO037|PRO039|PRO040|PRO041|PRO042|PRO043|PRO044|PRO045|PRO047|PRO048|PRO049|PRO050|PRO053|PRO055|PRO056|PRO057|PRO058", temp2)]
# exclude 54, 38, 26, 46, 51, 52 because of RA fuckup/not believe deception - this needs to be fixed. excel creates extra columns for some csvs


alltrialsdat <- do.call(rbind.fill,lapply(temp1,read.csv)) #combine into all trials, all subjects


alltrialsdat <- alltrialsdat[!is.na(alltrialsdat$stealTrial),] #Remove empty rows
alltrialsdat$choiceType <- as.character(alltrialsdat$choiceType)



#setwd - where models will be output
setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Model Tests/Softmax/PRO_A/")


alltrialsCSV <- write.csv(alltrialsdat, file="alltrials.csv")



