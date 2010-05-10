#Adjusted R squared (ARS) method for demo
ParaARSdemo<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
ARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
    Demo=TRUE, BANOVA=FALSE, replicated=FALSE, parallel=TRUE)
}