#Adjusted R squared (ARS) method and ANOVA
AIC_BANOVA<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
aic(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Demo=FALSE, BANOVA=TRUE)
}