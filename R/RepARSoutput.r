#output for ARS
RepARSoutput<-function(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData){
NCAoutput(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData,
          NCA=FALSE,ARS=TRUE,TTT=FALSE,aic=FALSE,TTTARS=FALSE,TTTAIC=FALSE, replicated=TRUE)
}