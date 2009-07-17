MultipleNCA.BANOVA<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
{
NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,
               Demo=FALSE,BANOVA=TRUE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=TRUE)
}