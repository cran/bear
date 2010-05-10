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
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2, multiple=FALSE)
{
filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" The following output files can be found at the directory of                \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" ANOVA_stat.txt                                                             \n")
if(multiple){
cat("    -->ANOVA: Cmax_ss, AUC(tau)ss, lnCmax_ss, lnAUC(tau)ss                 \n")
cat("    -->90%CI: lnCmax_ss, lnAUC(tau)ss                                      \n")
cat(" ODplots.pdf                                                               \n")
cat("    -->Normal Probability Plot of lnCmax_ss, lnAUC(tau)ss(intrasubj)       \n")
cat("    -->Normal Probability Plot of lnCmax_ss, lnAUC(tau)ss(intersubj)       \n")
cat("    -->lnCmax_ss expected value vs studentized residuals(intrasubj)        \n") 
cat("    -->lnAUC(tau)ss expected value vs studentized residuals(intrasubj)     \n")
cat("    -->lnCmax_ss expected value vs studentized residuals(intersubj)        \n") 
cat("    -->lnAUC(tau)ss expected value vs studentized residuals(intersubj)     \n")
cat("    -->Boxplots(intrasubj): lnCmax_ss, lnAUC(tau)ss                        \n") 
cat("    -->Boxplots(intersubj): lnCmax_ss, lnAUC(tau)ss                        \n") 
cat("    -->Cook's distance plots: lnCmax_ss, lnAUC(tau)ss                     \n")  
}
else{
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
cat("    -->Cook's distance plots: ln(Cmax), ln(AUC0t), and ln(AUC0inf)          \n")  
}
cat("****************************************************************************\n")
cat("\n")
#class level
values <- lapply(TotalData, unique)
level <-sapply(values, length)
#divide by seq
if(multiple){
TotalData1<-data.frame (subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t)
}
else{
TotalData1<-data.frame (subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF)
}                    
rsubj<-as.data.frame(sapply(TotalData1, tapply, TotalData1$subj, mean))
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
rseq<-as.data.frame(sapply(TotalData1, tapply, TotalData1$seq, mean))
rprd<-as.data.frame(sapply(TotalData1, tapply, TotalData1$prd, mean))

#ANOVA.txt
zz <- file("ANOVA_stat.txt", open="wt")
sink(zz)
description_version()
cat("\n")
cat("  List of Input Data Obtained from NCA         \n")
cat("-----------------------------------------------\n")
if(multiple){
TotalData2<-data.frame (subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax_ss=TotalData$Cmax, AUCtau_ss=TotalData$AUC0t)
colnames(TotalData2)<- c("subj","drug","seq","prd","Cmax_ss","AUC(tau)ss")
show(TotalData2)
}
else{
show(TotalData1)
}

cat("\n")
cat("\n")
cat("  Class Level Information                  \n")
cat("-------------------------------------------------------\n")
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
cat("------------------------------------------------\n")
if(multiple){
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=rseq$Cmax, AUC0t=rseq$AUC0t)
colnames(seq_mean)<- c("SEQUENCE","Cmax_ss","AUCtau_ss" )
show(seq_mean)
cat("\n")
cat("\n")
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=rsubj$Cmax, AUC0t=rsubj$AUC0t)                      
colnames(subj_mean)<- c("SUBJECT","SEQUENCE","Cmax_ss","AUCtau_ss")
show(subj_mean)
cat("\n")
cat("\n")
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=rprd$Cmax, AUC0t=rprd$AUC0t)                      
colnames(prd_mean)<- c("PERIOD","Cmax_ss","AUCtau_ss" )
show(prd_mean)
cat("\n")
cat("\n")
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t)                      
colnames(drug_mean)<- c("DRUG","Cmax_ss","AUCtau_ss" )
show(drug_mean)
cat("\n")
cat("\n")
cat("\n")
MultipleBANOVA(RefData, TestData, TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,            
       IntraInterlnCmax00,IntraInterlnAUC0t00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
else{
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=rseq$Cmax, AUC0t=rseq$AUC0t, AUC0INF=rseq$AUC0INF)
show(seq_mean)
cat("\n")
cat("\n")
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=rsubj$Cmax, AUC0t=rsubj$AUC0t, AUC0INF=rsubj$AUC0INF)                      
show(subj_mean)
cat("\n")
cat("\n")
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=rprd$Cmax, AUC0t=rprd$AUC0t, AUC0INF=rprd$AUC0INF)                      
show(prd_mean)
cat("\n")
cat("\n")
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF)                      
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
}
cat("\n")
sink()

options(warn=-1)
pdf("ODplots.pdf", paper = "a4", bg = "white")
description_plot()
if(multiple){
MultipleBANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,
                   IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                   IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,TotalData)
}
else{
BANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData)
}
dev.off()
}