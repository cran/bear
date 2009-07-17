#output for AIC for multiple dose
MultipleAICoutput<-function(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, Tau, TlastD, rdata.split,tdata.split){
NCAoutput(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split, Tau, TlastD,NCA=FALSE,ARS=FALSE,TTT=FALSE,aic=TRUE,TTTARS=FALSE,TTTAIC=FALSE,replicated=FALSE, multiple=TRUE)
}