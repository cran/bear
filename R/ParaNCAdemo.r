#NCA demo for parallel study
ParaNCAdemo<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split){
NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
    Demo=TRUE, BANOVA=FALSE,replicated=FALSE, MIX=FALSE, parallel=TRUE)
}