MultipleNCAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis, Tau, TlastD, separateWindows=TRUE)
{
NCAanalyze(TotalSingledata, Dose,  Tau, TlastD, xaxis,yaxis, separateWindows=TRUE,
                     parallel=FALSE, MIX=FALSE, multiple=TRUE)
}