MultipleParaNCAselectdemo<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)
{
NCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0,
                    Demo=TRUE, BANOVA=FALSE, replicated=FALSE, MIX=FALSE, parallel=TRUE, multiple=TRUE)
}