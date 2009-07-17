MultipleParaNCAoutput<-function(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, rdata.split,tdata.split, Tau, TlastD)
{
NCAoutput(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split, Tau, TlastD,
                    NCA=TRUE,ARS=FALSE,TTT=FALSE,aic=FALSE,TTTARS=FALSE,TTTAIC=FALSE,replicated=FALSE,parallel=TRUE,multiple=TRUE)
 }