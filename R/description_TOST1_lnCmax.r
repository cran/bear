#TOST except TL>0.05 or TU>0.05
description_TOST1_lnCmax<-function(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
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
cat(paste("Ho: Theta < ",formatC(theta1,format="f",digits=3)," or  Theta > ",formatC(theta2,format="f",digits=5),"\n",sep=""))
cat(paste("Ha: ",formatC(theta1,format="f",digits=3)," < or = Theta < or = ",formatC(theta2,format="f",digits=5),"\n",sep=""))
cat("where Theta = Mean_Test/Mean_Ref.\n")
cat("Because all P values are less than 0.05,\nwe will reject the null hypothesis (Ho).\n")
cat(paste("BE acceptance criterion is set within the range of ",formatC(theta1*100,format="f",digits=3),"% - ",
     formatC(theta2*100,format="f",digits=3),"%.\n",sep=""))
cat("\n")
}