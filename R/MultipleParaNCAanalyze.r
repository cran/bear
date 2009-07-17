MultipleParaNCAanalyze<-function(TotalSingledata,  Dose,  Tau, TlastD, xaxis,yaxis, separateWindows=TRUE)
{
NCAanalyze(TotalSingledata, Dose,  Tau, TlastD, xaxis,yaxis, separateWindows=TRUE,
                     parallel=TRUE, MIX=FALSE, multiple=TRUE)
}