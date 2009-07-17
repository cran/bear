MultipleParaNCA<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                         Tau, TlastD,SingleRdata0,SingleTdata0, separateWindows=TRUE)
 {
NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,Demo=FALSE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE, parallel=TRUE, multiple=TRUE)
 }