ParaNCAoutput<-function(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split){
NCAoutput(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split,
                    NCA=TRUE,
                    ARS=FALSE,
                    TTT=FALSE,
                    aic=FALSE,
                    TTTARS=FALSE,
                    TTTAIC=FALSE,
                    replicated=FALSE,
                    parallel=TRUE)
  }