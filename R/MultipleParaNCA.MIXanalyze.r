#data analyze NCA ->lme for parallel study
MultipleParaNCA.MIXanalyze<-function(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE)
{
NCAanalyze(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE,
           parallel=TRUE, MIX=TRUE, multiple=TRUE)
}