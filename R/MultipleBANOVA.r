MultipleBANOVA<-function(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnpAUC00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnpAUC_theta1,lnpAUC_theta2)
{
pAUC<-pAUC
if(pAUC){
BANOVA(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter=0,lnAUC0INF_MSintra=0,lnAUC0INF_SSinter=0,lnAUC0INF_SSintra=0,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00=0,
       IntraInterlnpAUC00,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnAUC0INF_theta1=0,lnAUC0INF_theta2=0,lnpAUC_theta1,lnpAUC_theta2,multiple=TRUE)
}
else{
BANOVA(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter=0,lnAUC0INF_MSintra=0,lnAUC0INF_SSinter=0,lnAUC0INF_SSintra=0,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00=0,
       IntraInterlnpAUC00=0,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnAUC0INF_theta1=0,lnAUC0INF_theta2=0,lnpAUC_theta1=0,lnpAUC_theta2=0,multiple=TRUE)
}
       
### BANOVA(RefData,TestData,TotalData,L1,L2,                                        ### this is wrong one. It will cause error in
###        lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,             ### multiple-dose 2x2x2...  -YJ
###        lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
###        lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
###        IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnpAUC00,
###        lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
###        lnpAUC_theta1,lnpAUC_theta2,multiple=TRUE)
}