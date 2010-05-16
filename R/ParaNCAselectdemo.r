ParaNCAselectdemo<-function(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis){
NCAselect(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata, xaxis, yaxis,
          Demo=TRUE, BANOVA=FALSE, replicated=FALSE, parallel=TRUE)
}