MultipleParaNCAselectsave<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
       Tau, TlastD,SingleRdata0,SingleTdata0)
{
NCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                        Tau, TlastD,SingleRdata0,SingleTdata0, BANOVA=FALSE, replicated=FALSE, MIX=FALSE, parallel=TRUE, multiple=TRUE)
}