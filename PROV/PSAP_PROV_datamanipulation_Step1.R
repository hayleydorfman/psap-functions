#PROV

require(plyr)

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Experimental Files/PSAP_PRO_V_TASK/data/")
temp1 = list.files(pattern ="*.csv") #grab csv files
temp1a <- temp1[grep("PROV", temp1)] 

temp <- temp1a[-c(2,4,6,11,12,18,22,23,25,26,27,31,32,34,36,37,40,45,46,48,59,60,61,62,76,79,85,88,89,90,97,103,106,110,116,120,122,123,124,125,127,128,129,131,133,134)] 
temp<- temp[-c(54,64,74)]
#Exclude for not believing deception:
#PROV002, PROV003, PROV007, PROV009, PROV011, PROV017,
#PROV018, PROV024, PROV028, PROV029, PROV031, PROV032,
#PROV033, PROV037, PROV038, PROV040, PROV042, PROV043,
#PROV046, PROV051, PROV052, PROV054, PROV065,
#PROV067, PROV068, PROV069, PROV083, PROV086, PROV091,
#PROV094, PROV095, PROV096, PROV101, PROV107, PROV109,
#PROV111, PROV114, PROV120, PROV124, PROV126, PROV127,
#PROV128, PROV129, PROV131, PROV132, PROV133, PROV135,
#PROV137, PROV138



alltrialsdat <- do.call(rbind.fill,lapply(temp,read.csv)) #combine into all trials, all subjects
alltrialsdat 

alltrialsdat <- alltrialsdat[!is.na(alltrialsdat$earnTrial),] #Remove empty rows
alltrialsdat$choiceType <- as.character(alltrialsdat$choiceType)



#setwd - where models will be output
setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROV/")

write.csv(alltrialsdat, file="alltrials.csv") #read out csv

pressTimes <- read.csv("pressTimes.csv") #run pressTimes in python notebook before reading in this file

alltrials <- cbind(alltrialsdat, pressTimes)

write.csv(alltrials, file="alltrials.csv") #read out csv




