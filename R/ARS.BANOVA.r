#Adjusted R squared (ARS) method and ANOVA
ARS.BANOVA<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
ARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
   Demo=FALSE, BANOVA=TRUE)
}