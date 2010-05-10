#output for ARS for multiple dose
MultipleARSoutput<-function(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, Tau, TlastD,rdata.split,tdata.split){
NCAoutput(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split, Tau, TlastD,NCA=FALSE,ARS=TRUE,TTT=FALSE,aic=FALSE,TTTARS=FALSE,TTTAIC=FALSE,replicated=FALSE, multiple=TRUE)
}