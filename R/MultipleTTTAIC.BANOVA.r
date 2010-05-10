MultipleTTTAIC.BANOVA<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
 {
TTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0,
              separateWindows=TRUE,Demo=FALSE, BANOVA=TRUE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=TRUE)
 }