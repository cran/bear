#TTT -> lme for parallel study
ParaTTT.MIX<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1){
 TTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,
              separateWindows=TRUE,Demo=FALSE, BANOVA=FALSE,replicated=FALSE, MIX=TRUE, parallel=TRUE)
}