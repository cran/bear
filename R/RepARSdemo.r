#Adjusted R squared (ARS) method for demo
RepARSdemo<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
ARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
    Demo=TRUE, BANOVA=FALSE, replicated=TRUE)
}