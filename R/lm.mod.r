###
### this function will be called by BANOVA()
###
lm.mod<-function(target, inputdata){
cat("\n")
target.lm<- lm(target ~ seq + subj:seq + prd + drug ,data=inputdata)
###
### residual plot/diagnosis?
###
#   par(ask=FALSE)
#   target.lm.res=resid(target.lm)
#   plot(inputdata$drug, target.lm.res, ylab="Residuals", xlab="Treatment/Formulaiton",
#        main="histogram plot of ln(target)");abline(a=0,b=0,col=2);dev.new()
#   plot(target.lm.res, ylab="Residuals", xlab="Fitted",
#        main="residual plot of ln(target)");abline(a=0,b=0,col=2);dev.new()
#   qqnorm(target.lm.res, main="QQ plot of ln(target)");qqline(target.lm.res,col=2);dev.new()
### hist(target.lm.res);dev.new()
#   par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
#   plot(target.lm, which=1:4)  ### most useful one!  -YJ
#   par(mfrow = c(1, 1))
#   cat(" AIC =",AIC(target.lm),";","BIC =",BIC(target.lm),"for lm().\n\n")
### residuals(target.lm)
### df.residual(target.lm)
#   readline(" pause here...")
###
BearAnova=anova(target.lm)
row.names(BearAnova)[4]="subj(seq)"
show(BearAnova[2:5,])
cat("\n")
colnames(BearAnova)<- c(" DF","   Type I SS"," Mean Square"," F Value"," Pr > F")
show(BearAnova[2:4,])
cat("\n")
colnames(BearAnova)<- c(" DF"," Type III SS"," Mean Square"," F Value"," Pr > F")
show(BearAnova[2:4,])
cat("-----------------------------------------------------------\n")
#
# to adpat to SAS output
#
cat("\n")
cat("Tests of Hypothesis using the Type I MS for \n")
cat("SUBJECT(SEQUENCE) as an error term\n\n")
show(summary(aov(target ~ seq,data=inputdata)))
cat("\n")
cat("Tests of Hypothesis using the Type III MS for\n")
cat("SUBJECT(SEQUENCE) as an error term\n\n")
show(summary(aov(target ~ seq,data=inputdata)))
cat("---\n")
cat("Sum Sq. = Type III SS\n")
}