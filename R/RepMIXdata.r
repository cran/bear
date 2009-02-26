#replicated study (lme) for data entry
RepMIXdata<-function(TotalData)
{
BANOVAdata(TotalData,replicated=TRUE, parallel=FALSE)
}