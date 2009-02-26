#data analyze for NCA for parallel study
ParaNCAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE)
{
NCAanalyze(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE,
           parallel=TRUE, MIX=FALSE)
}