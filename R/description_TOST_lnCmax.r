#TOST or TL>0.05 or TU>0.05
description_TOST_lnCmax<-function(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2, 
                                  lnCmax=TRUE, lnAUC0t=FALSE, lnAUC0INF=FALSE ){

if(lnCmax){
 theta1<-lnCmax_theta1
 theta2<-lnCmax_theta2
 }
if(lnAUC0t){
 theta1<-lnAUC0t_theta1
 theta2<-lnAUC0t_theta2
 } 
if(lnAUC0INF){
 theta1<-lnAUC0INF_theta1
 theta2<-lnAUC0INF_theta2
 }  
cat("**Interpretation:\n")
cat("Ho: Theta < ",formatC(theta1,format="f",digits=5)," or  Theta > ",formatC(theta2,format="f",digits=5), "\n")
cat("Ha: ",formatC(theta1,format="f",digits=5)," < or = Theta < or = ",formatC(theta2,format="f",digits=5),"\n")
cat("where Theta = Mean_test/Mean_ref.\n")
cat("Because at least one of P values is greater than 0.05, we thus can not reject\n")
cat("the null hypothesis (Ho). \n")
cat("BE acceptance criterion is set within the range of ",formatC(theta1*100,format="f",digits=3)," - ",formatC(theta2*100,format="f",digits=3),"% .\n")
cat("\n")
}