#data input for Multiple dose study
MultipleBANOVAdata<-function(TotalData)
{
BANOVAdata(TotalData,replicated=FALSE,parallel=FALSE, multiple=TRUE)
}