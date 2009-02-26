##########.txt output
options(warn=-1)
BANOVAoutput<-function(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
       IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
       IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
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
#class level
values <- lapply(TotalData, unique)
level <-sapply(values, length)
#divide by seq
TotalData1<-data.frame (subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF, 
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
                    
rsubj<-as.data.frame(sapply(TotalData1, tapply, TotalData1$subj, mean))
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
rseq<-as.data.frame(sapply(TotalData1, tapply, TotalData1$seq, mean))
rprd<-as.data.frame(sapply(TotalData1, tapply, TotalData1$prd, mean))

#ANOVA.txt
zz <- file("ANOVA_stat.txt", open="wt")
sink(zz)
description_version()
cat("\n")
cat("  Data Summary of BA measurement                  \n")
cat("--------------------------------------------------------------------------\n")
show(TotalData)
cat("\n")
cat("\n")
cat("\n")
cat("  Class Level Information                  \n")
cat("--------------------------------------------------------------------------\n")
cat("  Class       Levels      Values\n")  
cat("  SUBJECT     ",level[[1]],"        ",sort(as.numeric(levels(values$subj))) ,"\n")
cat("  DRUG         ",level[[2]],"        ",sort(as.numeric(levels(values$drug))),"\n")
cat("  SEQUENCE     ",level[[3]],"        ",sort(as.numeric(levels(values$seq))),"\n")
cat("  PERIOD       ",level[[4]],"        ",sort(as.numeric(levels(values$prd))),"\n")
cat("-----------\n")
cat("DRUG 1: the Ref. product; DRUG 2: the Test product\n")
cat("SEQUENCE 1:  Ref. -> Test, and SEQUENCE 2: Test -> Ref.\n")
cat("-------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat(" Means                  \n")
cat("--------------------------------------------------------------------------\n")
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=rseq$Cmax, AUC0t=rseq$AUC0t, AUC0INF=rseq$AUC0INF, 
                    lnCmax=rseq$lnCmax,lnAUC0t=rseq$lnAUC0t,lnAUC0INF=rseq$lnAUC0INF)
show(seq_mean)
cat("\n")
cat("\n")
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=rsubj$Cmax, AUC0t=rsubj$AUC0t, AUC0INF=rsubj$AUC0INF, 
                      lnCmax=rsubj$lnCmax,lnAUC0t=rsubj$lnAUC0t,lnAUC0INF=rsubj$lnAUC0INF)                      
show(subj_mean)
cat("\n")
cat("\n")
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=rprd$Cmax, AUC0t=rprd$AUC0t, AUC0INF=rprd$AUC0INF, 
                      lnCmax=rprd$lnCmax,lnAUC0t=rprd$lnAUC0t,lnAUC0INF=rprd$lnAUC0INF)                      
show(prd_mean)
cat("\n")
cat("\n")
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF, 
                      lnCmax=rdrug$lnCmax,lnAUC0t=rdrug$lnAUC0t,lnAUC0INF=rdrug$lnAUC0INF)                      
show(drug_mean)
cat("\n")
cat("\n")
cat("\n")
BANOVA(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
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