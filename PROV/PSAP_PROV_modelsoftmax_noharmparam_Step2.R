
if (!require(matrixStats)) {install.packages("matrixStats"); require(matrixStats)}
library(matrixStats)
library(matlab)
library(plyr)
library(ggplot2)

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/Code/Final Code/PROV/")

alltrialsdat <- read.csv("alltrialsdatPROV.csv", row.names = 1, stringsAsFactors = FALSE)

#alltrialsdat <- alltrialsdat[alltrialsdat$participant != "", ]
#str(alltrialsdat)


# --------------------------------------------------------------------------------------------------
# plots 

# disaggregated
# plot_disagg <- ggplot(alltrialsdat, aes(x = ddpd, y = rewardAmount)) +
#   geom_point(aes(color = choiceType)) +
#   facet_wrap(~ participant) +
#   theme_bw()
# ggsave(plot_disagg, file = "plot_disagg.pdf", height = 9, width = 11)
# 
# # aggregated
# alltrial_agg <- ddply(alltrialsdat, .(participant, ddpd, choiceType), summarise, 
#                       rewardAmount_avg = mean(rewardAmount),
#                       ddpd_20 = 20/mean(ddpd))
# 
# plot_agg <- ggplot(alltrial_agg, aes(x = ddpd, y = rewardAmount_avg)) +
#   geom_line(aes(x = ddpd, y = ddpd_20), color = "grey30") +
#   geom_point(aes(color = choiceType)) + 
#   facet_wrap(~ participant) +
#   theme_bw()
# ggsave(plot_agg, file = "plot_agg.pdf", height = 9, width = 11)
# 
# 
# 
# setwd("/Users/hayleydorfman/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO/Development/Experimental Files/PSAP_PRO_Task/model_tests/")
# 
# #remove missed trials from data frame - to do!

# --------------------------------------------------------------------------------------------------        
# aggression function

aggFunc <- function(a, betaT, participant, data = alltrialsdat) { 
  singsub <- data[data$participant==participant, ]
  singsub <- singsub[!is.na(singsub$choiceResponse.rt), ]
  
  valSteal <- singsub$stealTrial*a # value of steal is reward amt x a param
  valBank <- singsub$earnTrial    
  loglike <- ifelse(singsub$choiceType=="steal",
                    logSumExp(log(1/(1+exp(-betaT*(valSteal-valBank))))) , 
                    logSumExp(log(1-1/(1+exp(-betaT*(valSteal-valBank))))))
  loglike <- sum(-loglike)     
  probSteal <- 1/(1+exp(-betaT*(valSteal-valBank))) # softmax (vSteal-vBank = value diff between choices)
  probBank <- 1-probSteal
  dat <- data.frame(
    participant = singsub$participant,
    stealTrial = singsub$stealTrial, 
    actualChoice = singsub$choiceType, 
    actualChoicebin = ifelse(singsub$choiceType=="steal", 1, 0), 
    valSteal = valSteal, 
    valBank = valBank, 
    valDiff = valSteal-valBank, 
    probSteal = probSteal, 
    probBank = probBank, 
    modelChoice = ifelse(probSteal>probBank, "steal", "bank"), 
    modelChoicebin = ifelse(probSteal>probBank, 1, 0), 
    LL = loglike, 
    tempr = betaT, 
    agg = a,
    #thresholded = sum(abs(actualChoicebin - modelChoicebin)),
    #betterThanChance = sum(pbinom(0:thresholded, 80, 0.5)),
    stringsAsFactors = FALSE)
  #print(dat)  
  return(dat)
} 


# --------------------------------------------------------------------------------------------------  
# loop
mainDat <- data.frame()
df <- nrow
u.ppl <- unique(alltrialsdat$participant)

for (participant in u.ppl[1:25]) {
  
  fr <- function(x) {
    am <- x[1]
    agg <- x[2]
    tempError<-aggFunc(a = agg, betaT = am, participant = participant)
    #print(tempError)
    result<-unique(tempError$LL)
    min(result,10^10)
    #return(tempError)
  }
  
  output <- optim( par=c(0.5,0.5), fn=fr, gr=NULL, method="L-BFGS-B", lower=c(0.001,0.01), upper=c(5,2))
  
  cat("participant:", participant, "best agg:", output$par[2], "best am:", output$par[1], "\n")
  
  data <- list(participant, output$par[2], output$par[1])
  
  df <- as.data.frame(do.call("cbind", data))
  
  mainDat <- rbind(mainDat, df)  
  
  
  names(df)<- c("participant", "best agg", "best tempr")
}

names(mainDat) <- c("participant", "best agg", "best tempr")

setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")
write.csv(mainDat, file = paste(c("PROV_model_summary_noharmparam", ".csv"), collapse = ""))


# a
# plot_agg_param <- ggplot(alltrialsdat, aes(x = stealTrial, y = a)) +
#   geom_point(aes(color = choiceType)) +
#   facet_wrap(~ participant) +
#   theme_bw()
# ggsave(plot_disagg, file = "plot_aggParam.pdf", height = 9, width = 11)
# plot_agg_param




