MultipleParaNCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD)
{
NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD,
                  replicated=FALSE,parallel=TRUE,multiple=TRUE )
}