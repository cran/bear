### diaplay BE criteria set; assumed all BE acceptance is the same. --YJ
description_BE_criteria<-function(BE_LL,BE_UL){
BE_LL<-BE_LL
BE_UL<-BE_UL

 theta1<-BE_LL
 theta2<-BE_UL

cat(paste("*** BE acceptance criterion is set within the range:",formatC(theta1*100,format="f",digits=3),"% - ",
     formatC(theta2*100,format="f",digits=3),"%.",sep=""))
cat("\n\n")
}