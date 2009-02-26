ParaTTTAICdemo<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
TTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
    Demo=TRUE, BANOVA=FALSE,replicated=FALSE,parallel=TRUE)
}