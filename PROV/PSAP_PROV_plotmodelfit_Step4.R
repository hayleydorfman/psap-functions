setwd("/Users/hayley/Dropbox (Harvard SNP Lab)/PSAP ITC/PSAP_PRO_V/Analyses/")

x11()
par(mfrow=c(2,2))

df <- read.csv("1PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df[,1]),1),df$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 1 - No harm")
lines(df$modelChoicebin)

df2 <- read.csv("1PROV_harmparam_model_fit.csv")

plot(seq(1,length(df2[,1]),1),df2$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 1 - harm")
lines(df2$modelChoicebin)

df3 <- read.csv("2PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df3[,1]),1),df3$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 2 - No harm")
lines(df3$modelChoicebin)

df4 <- read.csv("2PROV_harmparam_model_fit.csv")

plot(seq(1,length(df4[,1]),1),df4$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 2 - harm")
lines(df4$modelChoicebin)

#------------------------------------------------------------#
x11()
par(mfrow=c(2,2))

df5 <- read.csv("5PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df5[,1]),1),df5$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 5 - No harm")
lines(df5$modelChoicebin)

df6 <- read.csv("5PROV_harmparam_model_fit.csv")

plot(seq(1,length(df6[,1]),1),df6$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 5 - harm")
lines(df6$modelChoicebin)

df7 <- read.csv("6PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df7[,1]),1),df7$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 6 - No harm")
lines(df7$modelChoicebin)

df8 <- read.csv("6PROV_harmparam_model_fit.csv")

plot(seq(1,length(df8[,1]),1),df8$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 6 - harm")
lines(df8$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df9 <- read.csv("7PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df9[,1]),1),df9$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 7 - No harm")
lines(df9$modelChoicebin)

df10 <- read.csv("7PROV_harmparam_model_fit.csv")

plot(seq(1,length(df10[,1]),1),df10$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 7 - harm")
lines(df10$modelChoicebin)

df11 <- read.csv("8PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df11[,1]),1),df11$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 8 - No harm")
lines(df11$modelChoicebin)

df12 <- read.csv("8PROV_harmparam_model_fit.csv")

plot(seq(1,length(df12[,1]),1),df12$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 8 - harm")
lines(df12$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df13 <- read.csv("9PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df13[,1]),1),df13$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 9 - No harm")
lines(df13$modelChoicebin)

df14 <- read.csv("9PROV_harmparam_model_fit.csv")

plot(seq(1,length(df14[,1]),1),df14$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 9 - harm")
lines(df14$modelChoicebin)

df15 <- read.csv("10PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df15[,1]),1),df15$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 10 - No harm")
lines(df15$modelChoicebin)

df16 <- read.csv("10PROV_harmparam_model_fit.csv")

plot(seq(1,length(df16[,1]),1),df16$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 10 - harm")
lines(df16$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df17 <- read.csv("11PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df17[,1]),1),df17$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 11 - No harm")
lines(df17$modelChoicebin)

df18 <- read.csv("11PROV_harmparam_model_fit.csv")

plot(seq(1,length(df18[,1]),1),df18$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 11 - harm")
lines(df18$modelChoicebin)

df19 <- read.csv("12PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df19[,1]),1),df19$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 12 - No harm")
lines(df19$modelChoicebin)

df20 <- read.csv("12PROV_harmparam_model_fit.csv")

plot(seq(1,length(df20[,1]),1),df20$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 12 - harm")
lines(df20$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df21 <- read.csv("13PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df21[,1]),1),df21$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 13 - No harm")
lines(df21$modelChoicebin)

df22 <- read.csv("13PROV_harmparam_model_fit.csv")

plot(seq(1,length(df22[,1]),1),df22$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 13 - harm")
lines(df22$modelChoicebin)

df23 <- read.csv("14PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df23[,1]),1),df23$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 14 - No harm")
lines(df23$modelChoicebin)

df24 <- read.csv("14PROV_harmparam_model_fit.csv")

plot(seq(1,length(df24[,1]),1),df24$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 14 - harm")
lines(df24$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df25 <- read.csv("15PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df25[,1]),1),df25$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 15 - No harm")
lines(df25$modelChoicebin)

df26 <- read.csv("15PROV_harmparam_model_fit.csv")

plot(seq(1,length(df26[,1]),1),df26$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 15 - harm")
lines(df26$modelChoicebin)

df27 <- read.csv("16PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df27[,1]),1),df27$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 16 - No harm")
lines(df27$modelChoicebin)

df28 <- read.csv("16PROV_harmparam_model_fit.csv")

plot(seq(1,length(df28[,1]),1),df28$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 16 - harm")
lines(df28$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df29 <- read.csv("17PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df29[,1]),1),df29$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 17 - No harm")
lines(df29$modelChoicebin)

df30 <- read.csv("17PROV_harmparam_model_fit.csv")

plot(seq(1,length(df30[,1]),1),df30$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 17 - harm")
lines(df30$modelChoicebin)

df31 <- read.csv("18PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df31[,1]),1),df31$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 18 - No harm")
lines(df31$modelChoicebin)

df32 <- read.csv("18PROV_harmparam_model_fit.csv")

plot(seq(1,length(df32[,1]),1),df32$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 18 - harm")
lines(df32$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df33 <- read.csv("19PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df33[,1]),1),df33$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 19 - No harm")
lines(df33$modelChoicebin)

df34 <- read.csv("19PROV_harmparam_model_fit.csv")

plot(seq(1,length(df34[,1]),1),df34$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 19 - harm")
lines(df34$modelChoicebin)

df35 <- read.csv("20PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df35[,1]),1),df35$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 20 - No harm")
lines(df35$modelChoicebin)

df36 <- read.csv("20PROV_harmparam_model_fit.csv")

plot(seq(1,length(df36[,1]),1),df36$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 20 - harm")
lines(df36$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df37 <- read.csv("21PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df37[,1]),1),df37$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 21 - No harm")
lines(df37$modelChoicebin)

df38 <- read.csv("21PROV_harmparam_model_fit.csv")

plot(seq(1,length(df38[,1]),1),df38$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 21 - harm")
lines(df38$modelChoicebin)

df39 <- read.csv("22PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df39[,1]),1),df39$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 22 - No harm")
lines(df39$modelChoicebin)

df40 <- read.csv("22PROV_harmparam_model_fit.csv")

plot(seq(1,length(df40[,1]),1),df40$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 22 - harm")
lines(df40$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df41 <- read.csv("23PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df41[,1]),1),df41$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 23 - No harm")
lines(df41$modelChoicebin)

df42 <- read.csv("23PROV_harmparam_model_fit.csv")

plot(seq(1,length(df42[,1]),1),df42$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 23 - harm")
lines(df42$modelChoicebin)

df43 <- read.csv("24PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df43[,1]),1),df43$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 24 - No harm")
lines(df43$modelChoicebin)

df44 <- read.csv("24PROV_harmparam_model_fit.csv")

plot(seq(1,length(df44[,1]),1),df44$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 24 - harm")
lines(df44$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df45 <- read.csv("25PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df45[,1]),1),df45$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 25 - No harm")
lines(df45$modelChoicebin)

df46 <- read.csv("25PROV_harmparam_model_fit.csv")

plot(seq(1,length(df46[,1]),1),df46$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 25 - harm")
lines(df46$modelChoicebin)

df47 <- read.csv("26PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df47[,1]),1),df47$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 26 - No harm")
lines(df47$modelChoicebin)

df48 <- read.csv("26PROV_harmparam_model_fit.csv")

plot(seq(1,length(df48[,1]),1),df48$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 26 - harm")
lines(df48$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df49 <- read.csv("27PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df49[,1]),1),df49$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 27 - No harm")
lines(df49$modelChoicebin)

df50 <- read.csv("27PROV_harmparam_model_fit.csv")

plot(seq(1,length(df50[,1]),1),df50$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 27 - harm")
lines(df50$modelChoicebin)

df51 <- read.csv("28PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df51[,1]),1),df51$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 28 - No harm")
lines(df51$modelChoicebin)

df52 <- read.csv("28PROV_harmparam_model_fit.csv")

plot(seq(1,length(df52[,1]),1),df52$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 28 - harm")
lines(df52$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df53 <- read.csv("29PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df53[,1]),1),df53$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 29 - No harm")
lines(df53$modelChoicebin)

df54 <- read.csv("29PROV_harmparam_model_fit.csv")

plot(seq(1,length(df54[,1]),1),df54$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 29 - harm")
lines(df54$modelChoicebin)

df55 <- read.csv("30PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df55[,1]),1),df55$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 30 - No harm")
lines(df55$modelChoicebin)

df56 <- read.csv("30PROV_harmparam_model_fit.csv")

plot(seq(1,length(df56[,1]),1),df56$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 30 - harm")
lines(df56$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df57 <- read.csv("31PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df57[,1]),1),df57$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 31 - No harm")
lines(df57$modelChoicebin)

df58 <- read.csv("31PROV_harmparam_model_fit.csv")

plot(seq(1,length(df58[,1]),1),df58$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 31 - harm")
lines(df58$modelChoicebin)

df59 <- read.csv("32PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df59[,1]),1),df59$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 32 - No harm")
lines(df59$modelChoicebin)

df60 <- read.csv("32PROV_harmparam_model_fit.csv")

plot(seq(1,length(df60[,1]),1),df60$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 32 - harm")
lines(df60$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df61 <- read.csv("33PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df61[,1]),1),df61$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 33 - No harm")
lines(df61$modelChoicebin)

df62 <- read.csv("33PROV_harmparam_model_fit.csv")

plot(seq(1,length(df62[,1]),1),df62$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 33 - harm")
lines(df62$modelChoicebin)

df63 <- read.csv("34PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df63[,1]),1),df63$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 34 - No harm")
lines(df63$modelChoicebin)

df64 <- read.csv("34PROV_harmparam_model_fit.csv")

plot(seq(1,length(df64[,1]),1),df64$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 34 - harm")
lines(df64$modelChoicebin)

#------------------------------------------------------------#

x11()
par(mfrow=c(2,2))

df65 <- read.csv("35PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df65[,1]),1),df65$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 35 - No harm")
lines(df65$modelChoicebin)

df66 <- read.csv("35PROV_harmparam_model_fit.csv")

plot(seq(1,length(df66[,1]),1),df66$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 35 - harm")
lines(df66$modelChoicebin)

df67 <- read.csv("36PROV_noharmparam_model_fit.csv")

plot(seq(1,length(df67[,1]),1),df67$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 36 - No harm")
lines(df67$modelChoicebin)

df68 <- read.csv("36PROV_harmparam_model_fit.csv")

plot(seq(1,length(df68[,1]),1),df68$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Model Fit - Subj 36 - harm")
lines(df68$modelChoicebin)

#------------------------------------------------------------#