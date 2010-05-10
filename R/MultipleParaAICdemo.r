MultipleParaAICdemo<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)
{
aic(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau, TlastD,SingleRdata0,SingleTdata0,
              separateWindows=TRUE,Demo=TRUE, BANOVA=FALSE,replicated=FALSE, MIX=FALSE, parallel=TRUE, multiple=TRUE)
}