MultipleParaNCAselect.MIX<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)
{
NCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0,
                    Demo=FALSE, BANOVA=FALSE, replicated=FALSE, MIX=TRUE, parallel=TRUE, multiple=TRUE)
}