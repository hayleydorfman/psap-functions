par(mfrow=c(2,2))

df <- read.csv("1hyper_model3_agg_comb_model_fit.csv")

x11()
plot.new()
v <- ggplot(df19,aes(x= seq(1,length(df19[,1]),1), y = df19$actualChoicebin))+geom_point(aes(size = 4, colour = "gray"))+
  labs(x="Trial", y="Earn                                                        Steal")+title("Model Fit - Subject 19")+
  geom_line(data = df19, aes(x= seq(1,length(df19[,1]),1), y =df19$modelChoicebin), size = 2, colour = "#3F4042", alpha = 0.8)+
  coord_flip()+
  mytheme3
png("example_model.png", width = 12, height = 8, units = 'in', res = 400)
v
dev.off()

#My theme 3
mytheme3 = theme(
  panel.border = element_rect(colour = "gray", fill = NA)
  #,panel.grid.major = element_line(colour ='gray')
  #,panel.background = element_rect(fill = 'gray90')
  #,panel.grid.minor = element_line(colour ='white')
  ,axis.line = element_line(color ='black')
  ,axis.text.y = element_text(size=24,vjust = 0.8, family="Helvetica") #you can change fonts here
  ,axis.text.x =element_blank()
  ,axis.ticks.x = element_blank()
  ,axis.title = element_text(size= 24,family="Helvetica")
  ,title = element_text(size =24, color = "#3F4042", face="bold", family="Helvetica")
  ,panel.background = element_rect(fill = "white")
  #,legend.key.size=unit(2, "cm")
  #,legend.text = element_text(size=14,family="Helvetica")
  #,legend.title = element_text(size=16,family="Helvetica")
  
)


df2 <- read.csv("2hyper_model3_agg_comb_model_fit.csv")

plot(seq(1,length(df2[,1]),1),df2$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 2")
lines(df2$modelChoicebin)

df3 <- read.csv("3hyper_model3_agg_comb_model_fit.csv")

plot(seq(1,length(df3[,1]),1),df3$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 3")
lines(df3$modelChoicebin)

df4 <- read.csv("4hyper_model3_agg_comb_model_fit.csv")

plot(seq(1,length(df4[,1]),1),df4$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 4")
lines(df4$modelChoicebin)

par(mfrow=c(2,2))

df5 <- read.csv("5agg_comb_model_fit.csv")

plot(seq(1,length(df5[,1]),1),df5$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 5")
lines(df5$modelChoicebin)

df6 <- read.csv("6agg_comb_model_fit.csv")

plot(seq(1,length(df6[,1]),1),df6$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 6")
lines(df6$modelChoicebin)

df7 <- read.csv("7agg_comb_model_fit.csv")

plot(seq(1,length(df7[,1]),1),df7$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 7")
lines(df7$modelChoicebin)

df8 <- read.csv("8agg_comb_model_fit.csv")

plot(seq(1,length(df8[,1]),1),df8$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 8")
lines(df8$modelChoicebin)

df9 <- read.csv("9agg_comb_model_fit.csv")

plot(seq(1,length(df9[,1]),1),df9$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 9")
lines(df9$modelChoicebin)

x11()
plot.new()
par(mfrow=c(2,2))

df10 <- read.csv("10agg_comb_model_fit.csv")

plot(seq(1,length(df10[,1]),1),df10$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 10")
lines(df10$modelChoicebin)

df11 <- read.csv("11agg_comb_model_fit.csv")

plot(seq(1,length(df11[,1]),1),df11$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 11")
lines(df11$modelChoicebin)

df12 <- read.csv("12agg_comb_model_fit.csv")

plot(seq(1,length(df12[,1]),1),df12$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 12")
lines(df12$modelChoicebin)

df13 <- read.csv("13agg_comb_model_fit.csv")

plot(seq(1,length(df13[,1]),1),df13$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 13")
lines(df13$modelChoicebin)

x11()
plot.new()

par(mfrow=c(2,2))

df18 <- read.csv("18agg_comb_model_fit.csv")

plot(seq(1,length(df18[,1]),1),df18$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 18")
lines(df18$modelChoicebin)

df19 <- read.csv("19agg_comb_model_fit.csv")

plot(seq(1,length(df19[,1]),1),df19$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 19")
lines(df19$modelChoicebin)

df20 <- read.csv("20agg_comb_model_fit.csv")

plot(seq(1,length(df20[,1]),1),df20$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 20")
lines(df20$modelChoicebin)

df21 <- read.csv("21agg_comb_model_fit.csv")

plot(seq(1,length(df21[,1]),1),df21$actualChoicebin,xlab="Trial", ylab="Steal or Earn", main="Optim Model Fit - Subj 21")
lines(df21$modelChoicebin)

# Look at descriptives for model fit




