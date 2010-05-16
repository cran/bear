#output for AIC (replicated)
RepAICoutput<-function(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData){
NCAoutput(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData,
          NCA=FALSE,ARS=FALSE,TTT=FALSE,aic=TRUE,TTTARS=FALSE,TTTAIC=FALSE, replicated=TRUE, parallel=FALSE)
}