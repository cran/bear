MultipleParaMIX<-function(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,test_lnCmax,test_lnAUC0t,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
                          ref_lnAUC0INF,test_lnAUC0INF,lnAUC0INF_theta1,lnAUC0INF_theta2)
 {
RepMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,parallel=TRUE, multiple=TRUE)
 }