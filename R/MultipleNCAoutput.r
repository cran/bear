#output for NCA for multiple dose
MultipleNCAoutput<-function(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, Tau, TlastD,rdata.split,tdata.split){
NCAoutput(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split, Tau, TlastD,NCA=TRUE,ARS=FALSE,TTT=FALSE,aic=FALSE,TTTARS=FALSE,TTTAIC=FALSE,replicated=FALSE, multiple=TRUE)
}