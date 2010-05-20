# NCAselectsave()
ParaNCA.MIX<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split){

NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,
    SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Demo=FALSE, BANOVA=FALSE,replicated=FALSE,MIX=TRUE, parallel=TRUE)
}