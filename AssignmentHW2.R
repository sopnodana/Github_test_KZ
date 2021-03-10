
Temp <- c(26,32,30,35,35,30,33,38,30,36,29,30,27,41,18,28,17,13,36,27,23,27,39)
Discomfort <- c(0,0,1,0,1,1,rep(0,8),1,0,1,1,0,0,1,0,0)
temp_discomfort <- data.frame(Temp,Discomfort)
dat <- temp_discomfort[order(temp_discomfort$Temp),]





res_logit <- glm(Discomfort~Temp, data=dat, family= binomial(link="logit") )
summary(res_logit)



res_probit <- glm(Discomfort~Temp, data=dat, family= binomial(link="probit") )
summary(res_probit)


#####################====================####################
predict.logit <- predict.glm(res_logit, type="response")
predict.probit <- predict.glm(res_probit, type="response")
plot(dat$Temp, dat$Discomfort, xlab="Temp", ylab="Discomfort")
lines(dat$Temp, predict.logit, col="red")
lines(dat$Temp, predict.probit, col="blue")
legend(36, 0.99, legend=c("Fitted Logit", "Fitted Probit"),
            col=c("red", "blue"), lty=1, cex=0.8)




pdf("jeny1.pdf",width=15,height=10) 
plot(dat$Temp, dat$Discomfort, xlab="Temp", ylab="Discomfort")
lines(dat$Temp, predict.logit, col="red")
lines(dat$Temp, predict.probit, col="blue")
legend(36, 0.99, legend=c("Fitted Logit", "Fitted Probit"),col=c("red", "blue"), lty=1, cex=0.8)
dev.off()




