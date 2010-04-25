RepTTTAICoutput<-function(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData){
NCAoutput(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData,
          NCA=FALSE,ARS=FALSE,TTT=FALSE,aic=FALSE,TTTARS=FALSE,TTTAIC=TRUE, replicated=TRUE, parallel=FALSE)
}