##########.txt output
options(warn=-1)
BANOVAoutput<-function(RefData, TestData,TotalData)
{
filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" Files have been output to the directory of                                 \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" ANOVA_stat.txt                                                             \n")
cat("    -->ANOVA:Cmax, AUC0t, AUC0inf, ln(Cmax), ln(AUC0t), ln(AUC0inf)         \n")
cat("    -->90%CI: ln(Cmax), ln(AUC0t), and ln(AUC0inf)                          \n")
cat("****************************************************************************\n")
cat("\n")


#GLM_Cmax.txt
zz <- file("ANOVA_stat.txt", open="wt")
sink(zz)
description_version()
cat("\n")
cat("\n")
cat("\n")
BANOVA(RefData, TestData,TotalData)
cat("\n")
cat("\n")
cat("\n")
cat("\n")
sink()

}