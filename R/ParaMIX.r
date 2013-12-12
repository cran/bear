##for parallel study
ParaMIX<-function(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,
    lnpAUC_theta2)
{
RepMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
    lnpAUC_theta1,lnpAUC_theta2,parallel=TRUE)
}