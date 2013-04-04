MultipleNCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis, TlastD)
{
NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis, TlastD,
                  replicated=FALSE,parallel=FALSE,multiple=TRUE )
}