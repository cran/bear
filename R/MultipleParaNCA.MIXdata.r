#edit data for NCA -> lme  for replicated data
MultipleParaNCA.MIXdata<-function()
{
NCA.BANOVAdata(replicated=FALSE, parallel=TRUE, multiple=TRUE)
}