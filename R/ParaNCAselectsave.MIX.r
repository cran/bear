#select 3 points, then save the data (NCA -> lme) for parallel study
ParaNCAselectsave.MIX<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
                        ,rdata.split,tdata.split) {
NCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
                        ,rdata.split,tdata.split,BANOVA=FALSE, replicated=FALSE,  MIX=TRUE, parallel=TRUE)
}