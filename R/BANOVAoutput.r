##########.txt output
### not only ANOVA.pAUC output, but also ODA plots (with BANOVAplot()). -YJ
###
BANOVAoutput<-function(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,IntraInterlnpAUC00,
       IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,IntraInterlnAUC0tseq11,
       IntraInterlnAUC0tseq22,IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,
       IntraInterlnpAUCseq11,IntraInterlnpAUCseq22,lnCmax_theta1,lnCmax_theta2,
       lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
       lnpAUC_theta1,lnpAUC_theta2,multiple=FALSE)
{
options(warn=-1)
options(width=100)
## to avoid "not visible binding..." error message with codetool
pAUC<-pAUC               ### for pAUC
anova_output_xfile<- anova_output_xfile
ODplot_output_xfile<- ODplot_output_xfile
ODAnalysis<-ODAnalysis
##

filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" The following output files can be found at the directory of                \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" xxx_anova.txt                                                              \n")
if(multiple){
cat("    -->ANOVA: Cmax_ss, AUC(tau)ss, lnCmax_ss, lnAUC(tau)ss                 \n")
if(pAUC){
cat("    -->90%CI: lnCmax_ss, lnAUC(tau)ss and ln(pAUC)                       \n\n")
 }
else{
cat("    -->90%CI: lnCmax_ss, lnAUC(tau)ss                                    \n\n")}
if(ODAnalysis){
cat(" xxx_odaplots.pdf                                                          \n")
cat("    -->Normal Probability Plot of lnCmax_ss, lnAUC(tau)ss(intrasubj)       \n")
cat("    -->Normal Probability Plot of lnCmax_ss, lnAUC(tau)ss(intersubj)       \n")
cat("    -->lnCmax_ss expected value vs studentized residuals(intrasubj)        \n") 
cat("    -->lnAUC(tau)ss expected value vs studentized residuals(intrasubj)     \n")
cat("    -->lnCmax_ss expected value vs studentized residuals(intersubj)        \n") 
cat("    -->lnAUC(tau)ss expected value vs studentized residuals(intersubj)     \n")
cat("    -->Boxplots(intrasubj): lnCmax_ss, lnAUC(tau)ss                        \n") 
cat("    -->Boxplots(intersubj): lnCmax_ss, lnAUC(tau)ss                        \n") 
cat("    -->Cook's distance plots: lnCmax_ss, lnAUC(tau)ss                     \n")} 
}
else{
cat("    -->ANOVA: Cmax, AUC0t, AUC0inf, ln(Cmax), ln(AUC0t), ln(AUC0inf)        \n")
if(pAUC){
cat("    -->90%CI: ln(Cmax), ln(AUC0t), ln(AUC0inf) and ln(pAUC)               \n\n")
}
else{
cat("    -->90%CI: ln(Cmax), ln(AUC0t), and ln(AUC0inf)                       \n\n")}
if(ODAnalysis){
cat(" xxx_odplots.pdf                                                            \n")
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
cat("    -->Cook's distance plots: ln(Cmax), ln(AUC0t), and ln(AUC0inf)          \n")} 
}
cat("****************************************************************************\n")
cat("\n")
#class level
values <- lapply(TotalData, unique)
level <-sapply(values, length)
#divide by seq
if(multiple){
if(pAUC){
TotalData1<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,partAUC=TotalData$partAUC)
}
else{
TotalData1<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t)
 }
}
else{
if(pAUC){
TotalData1<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=round(TotalData$Cmax,digits=2),AUC0t=round(TotalData$AUC0t,
                      digits=2),AUC0INF=round(TotalData$AUC0INF,digits=2),partAUC=round(TotalData$partAUC,digits=2))
}
else{
TotalData1<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=round(TotalData$Cmax,digits=2),AUC0t=round(TotalData$AUC0t,
                      digits=2),AUC0INF=round(TotalData$AUC0INF,digits=2))
 }
}                    
rsubj<-as.data.frame(sapply(TotalData1, tapply, TotalData1$subj, mean))
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
rseq<-as.data.frame(sapply(TotalData1, tapply, TotalData1$seq, mean))
rprd<-as.data.frame(sapply(TotalData1, tapply, TotalData1$prd, mean))

#ANOVA.txt
cat("\n\n Generate anova output now...\n");readline(" Press Enter to proceed...");cat("\n\n")
zz <- file(anova_output_xfile, open="wt")
sink(zz, split=TRUE)   ### for debugging... -YJ
description_version()
cat("  List of Input Data Obtained from NCA         \n")
cat("-----------------------------------------------\n")
if(multiple){
if(pAUC){
TotalData2<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                        prd=as.numeric(TotalData$prd),Cmax_ss=round(TotalData$Cmax,digits=2),AUCtau_ss=round(TotalData$AUC0t,digits=2),
                        partAUC=round(TotalData$partAUC,digits=2))
colnames(TotalData2)<- c("subj","drug","seq","prd","Cmax_ss","AUC(tau)ss","partAUC")
show(TotalData2)
}
else{
TotalData2<-data.frame (subj=as.factor(TotalData$subj),drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax_ss=round(TotalData$Cmax,digits=2),AUCtau_ss=round(TotalData$AUC0t,digits=2))
colnames(TotalData2)<- c("subj","drug","seq","prd","Cmax_ss","AUC(tau)ss")
show(TotalData2)
}
}
else{
show(TotalData1)
}

cat("\n\n")
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
cat("\n\n")
cat(" Means                  \n")
cat("------------------------------------------------\n")
if(multiple){
if(pAUC){
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=round(rseq$Cmax,digits=2),AUC0t=round(rseq$AUC0t,digits=2),
                     partAUC=round(rseq$partAUC,digits=2))
colnames(seq_mean)<- c("SEQUENCE","Cmax_ss","AUCtau_ss","partAUC" )
}
else{
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=round(rseq$Cmax,digits=2),AUC0t=round(rseq$AUC0t,digits=2))
colnames(seq_mean)<- c("SEQUENCE","Cmax_ss","AUCtau_ss")
}
show(seq_mean)
cat("\n\n")
if(pAUC){
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=round(rsubj$Cmax,digits=2),
                      AUC0t=round(rsubj$AUC0t,digits=2),partAUC=round(rsubj$partAUC,digits=2))                      
colnames(subj_mean)<- c("SUBJECT","SEQUENCE","Cmax_ss","AUCtau_ss","partAUC")
}
else{
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=round(rsubj$Cmax,digits=2),
                      AUC0t=round(rsubj$AUC0t,digits=2))                      
colnames(subj_mean)<- c("SUBJECT","SEQUENCE","Cmax_ss","AUCtau_ss")
}
show(subj_mean)
cat("\n")
cat("\n")
if(pAUC){
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=round(rprd$Cmax,digits=2),AUC0t=round(rprd$AUC0t,digits=2),
                     partAUC=round(rprd$partAUC,digits=2))                      
colnames(prd_mean)<- c("PERIOD","Cmax_ss","AUCtau_ss","partAUC")
}
else{
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=round(rprd$Cmax,digits=2),AUC0t=round(rprd$AUC0t,digits=2))
colnames(prd_mean)<- c("PERIOD","Cmax_ss","AUCtau_ss")
}
show(prd_mean)
cat("\n")
cat("\n")
if(pAUC){
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=round(rdrug$Cmax,digits=2),AUC0t=round(rdrug$AUC0t,digits=2),
                      partAUC=round(rdrug$partAUC,digits=2))                      
colnames(drug_mean)<- c("DRUG","Cmax_ss","AUCtau_ss","partAUC")
}
else{
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=round(rdrug$Cmax,digits=2),AUC0t=round(rdrug$AUC0t,digits=2))                      
colnames(drug_mean)<- c("DRUG","Cmax_ss","AUCtau_ss")
}
show(drug_mean)
cat("\n")
cat("\n")
MultipleBANOVA(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnpAUC00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnpAUC_theta1,lnpAUC_theta2)
}
else{
if(pAUC){
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=round(rseq$Cmax,digits=2),AUC0t=round(rseq$AUC0t,digits=2),
                     AUC0INF=round(rseq$AUC0INF,digits=2),partAUC=round(rseq$partAUC,digits=2))
}
else{
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=round(rseq$Cmax,digits=2),AUC0t=round(rseq$AUC0t,digits=2),AUC0INF=round(rseq$AUC0INF,digits=2))
}
show(seq_mean)
cat("\n")
cat("\n")
if(pAUC){
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=round(rsubj$Cmax,digits=2), 
                      AUC0t=round(rsubj$AUC0t,digits=2),AUC0INF=round(rsubj$AUC0INF,digits=2),partAUC=round(rsubj$partAUC,digits=2))
}
else{
subj_mean<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=round(rsubj$Cmax,digits=2), 
                      AUC0t=round(rsubj$AUC0t,digits=2),AUC0INF=round(rsubj$AUC0INF,digits=2))
}
show(subj_mean)
cat("\n")
cat("\n")
if(pAUC){
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=round(rprd$Cmax,digits=2),AUC0t=round(rprd$AUC0t,digits=2),AUC0INF=round(rprd$AUC0INF,digits=2),
                     partAUC=round(rprd$partAUC,digits=2))
}
else{
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=round(rprd$Cmax,digits=2),AUC0t=round(rprd$AUC0t,digits=2),AUC0INF=round(rprd$AUC0INF,digits=2))
}                      
show(prd_mean)
cat("\n")
cat("\n")
if(pAUC){
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=round(rdrug$Cmax,digits=2),AUC0t=round(rdrug$AUC0t,digits=2),AUC0INF=round(rdrug$AUC0INF,digits=2),
                      partAUC=round(rdrug$partAUC,digits=2))
}
else{
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=round(rdrug$Cmax,digits=2),AUC0t=round(rdrug$AUC0t,digits=2),AUC0INF=round(rdrug$AUC0INF,digits=2))
}                      
show(drug_mean)
cat("\n")
cat("\n")
BANOVA(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       IntraInterlnpAUC00,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,lnpAUC_theta2)
}
cat("\n")
sink()
close(zz)

options(warn=-1)
##
## close OD analysis by default
##
ODAnalysis<-ODAnalysis
##
if(ODAnalysis){
cat("\n\n Save ODA plots now...\n");readline(" Press Enter to proceed...");cat("\n\n")
pdf(ODplot_output_xfile, paper = "a4", bg = "white")
logo_plot_desc()
if(multiple){
if(pAUC){
MultipleBANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00=0,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11=0,IntraInterlnAUC0INFseq22=0,TotalData,
           IntraInterlnpAUC00,IntraInterlnpAUCseq11,IntraInterlnpAUCseq22)
}
else{
MultipleBANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00=0,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11=0,IntraInterlnAUC0INFseq22=0,TotalData,
           IntraInterlnpAUC00=0,IntraInterlnpAUCseq11=0,IntraInterlnpAUCseq22=0)
  }
}
else{
if(pAUC){
BANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
           IntraInterlnpAUC00,IntraInterlnpAUCseq11,IntraInterlnpAUCseq22)
}
else{
BANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
           IntraInterlnpAUC00=0,IntraInterlnpAUCseq11=0,IntraInterlnpAUCseq22=0)
}
}
    dev.off()
  }
}
