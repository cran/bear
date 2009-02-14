##########.txt output
options(warn=-1)
BANOVAoutput<-function(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
       IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
       IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
{
filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" Files have been output to the directory of                                 \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" ANOVA_stat.txt                                                             \n")
cat("    -->ANOVA: Cmax, AUC0t, AUC0inf, ln(Cmax), ln(AUC0t), ln(AUC0inf)        \n")
cat("    -->90%CI: ln(Cmax), ln(AUC0t), and ln(AUC0inf)                          \n")
cat(" ODplots.pdf                                                                \n")
cat("    -->Normal Probability Plot of ln(Cmax), ln(AUC0t), ln(AUC0inf)(intrasubj)\n")
cat("    -->Normal Probability Plot of ln(Cmax), ln(AUC0t), ln(AUC0inf)(intersubj)\n")
cat("    -->lnCmax(expected value) vs studentized residuals(intrasubj)           \n") 
cat("    -->ln(AUC0t)(expected value) vs studentized residuals(intrasubj)        \n")
cat("    -->ln(AUC0inf)(expected value) vs studentized residuals(intrasubj)       \n")
cat("    -->lnCmax(expected value) vs studentized residuals(intersubj)           \n") 
cat("    -->ln(AUC0t)(expected value) vs studentized residuals(intersubj)        \n")
cat("    -->ln(AUC0inf)(expected value) vs studentized residuals(intersubj)       \n")
cat("    -->Boxplots(intrasubj): ln(Cmax), ln(AUC0t), and ln(AUC0inf)             \n") 
cat("    -->Boxplots(intersubj): ln(Cmax), ln(AUC0t), and ln(AUC0inf)             \n") 
cat("****************************************************************************\n")
cat("\n")


#ANOVA.txt
zz <- file("ANOVA_stat.txt", open="wt")
sink(zz)
description_version()
cat("\n")
BANOVA(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00)
cat("\n")
sink()



options(warn=-1)
pdf("ODplots.pdf")
description_plot()
BANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
dev.off()
}