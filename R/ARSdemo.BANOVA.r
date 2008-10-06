#Adjusted R squared (ARS) method and ANOVA for demmo
ARSdemo.BANOVA<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
ARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
   Demo=TRUE, BANOVA=TRUE)
}