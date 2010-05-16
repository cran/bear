RepNCAselectsave.MIX<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,
SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split){
NCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
            ,rdata.split,tdata.split,BANOVA=FALSE, replicated=TRUE, MIX=TRUE)
}