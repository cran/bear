#select 3 points, then save the data (NCA) for parallel study
ParaNCAselectsave<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
                        ,rdata.split,tdata.split) {
NCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
                        ,rdata.split,tdata.split,BANOVA=FALSE, replicated=FALSE,  MIX=FALSE, parallel=TRUE)
}