### diaplay BE criteria set; assumed all BE acceptance is the same. --YJ
description_BE_criteria<-function(lnCmax_theta1,lnCmax_theta2){

 theta1<-lnCmax_theta1
 theta2<-lnCmax_theta2

cat("*** BE acceptance criterion is set within the range:\n","   ",formatC(theta1*100,format="f",digits=3),"% -",
     formatC(theta2*100,format="f",digits=3),"%.\n")
cat("\n")
}