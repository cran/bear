RepMIXoutput<-function(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
                       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
                       parallel=FALSE, multiple=FALSE)
{
options(width=100)
## to avoid "not visible binding..." error message with codetool
lm_stat_xfile<- lm_stat_xfile 
lme_stat_xfile<- lme_stat_xfile
##

filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" The following output files can be found at the directory of                \n")
cat(" ",filepath,"                                                              \n")
cat("----------------------------------------------------------------------------\n")
if(parallel){
   cat(" xxx_lm_stat.txt \n")}
 else{
   cat(" xxx_lme_stat.txt \n")
}
if(multiple){
cat("    -->lme/lm: Cmax_ss, AUC(tau)ss, lnCmax_ss, lnAUC(tau)ss               \n")
cat("    -->90%CI: lnCmax_ss, lnAUC(tau)ss                                     \n")
}
else{
cat("    --> lme/lm: Cmax, AUC0t, AUC0inf, ln(Cmax), ln(AUC0t), ln(AUC0inf)    \n")
cat("    --> 90%CI: ln(Cmax), ln(AUC0t), and ln(AUC0inf)                       \n")
}
cat("****************************************************************************\n")
cat("\n")
values <- lapply(TotalData, unique)
level <-sapply(values, length)

if(parallel){

  if(multiple){
  TotalData1<-data.frame (subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t)
 
  rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
  drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t)       
  colnames(TotalData1)<- c("subj","drug","Cmax_ss","AUCtau_ss")
  colnames(drug_mean)<- c("drug","Cmax_ss","AUCtau_ss")
  }
  else{
  TotalData1<-data.frame(subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),
                      Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF)
  rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
  drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF)                
  }
}
else {
TotalData1<-data.frame (subj=TotalData$subj, drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF) 
rsubj<-as.data.frame(sapply(TotalData1, tapply, TotalData1$subj, mean))
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
rseq<-as.data.frame(sapply(TotalData1, tapply, TotalData1$seq, mean))
rprd<-as.data.frame(sapply(TotalData1, tapply, TotalData1$prd, mean))

subj_mean11<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=rsubj$Cmax, AUC0t=rsubj$AUC0t, AUC0INF=rsubj$AUC0INF)                      
subj_mean1<-subj_mean11[ do.call(order, subj_mean11) ,]
subj_mean<-na.omit(subj_mean1)
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=rseq$Cmax, AUC0t=rseq$AUC0t, AUC0INF=rseq$AUC0INF)
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=rprd$Cmax, AUC0t=rprd$AUC0t, AUC0INF=rprd$AUC0INF)            
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF)   
                      
}
if(parallel){
   zz <- file(lm_stat_xfile, open="wt")}
 else{
   zz <- file(lme_stat_xfile, open="wt")}
cat("\n\n Generate lm (linear model) or lme (linear mixed effect) output now...\n");readline(" Press Enter to proceed...");cat("\n\n")
sink(zz)
description_version()
cat("  List of Input Data from NCA                  \n")
cat("-----------------------------------------------\n")
show(TotalData1)
cat("\n")
cat("\n")
cat("  Class Levels Information                     \n")
cat("-----------------------------------------------\n")
cat("  Class       Levels      Values\n")  
if(parallel){
cat("  SUBJECT     ",level[[1]],"        ",sort(as.numeric(levels(values$subj))) ,"\n")
cat("  DRUG         ",level[[2]],"        ",sort(as.numeric(levels(values$drug))),"\n")
cat("-----------\n")
cat(" DRUG 1: the Ref. product; DRUG 2: the Test product \n")
cat("----------------------------------------------------\n")
cat("\n")
cat("\n")

cat(" Means                  \n")
cat("--------------------------------------------------\n") 
      
show(drug_mean)
cat("\n")
cat("\n")                     
   if(multiple){
   MultipleParaMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,test_lnCmax,test_lnAUC0t,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
   }
   else{
   ParaMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
         lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
   }
 }
else{ 
cat("  SUBJECT     ",level[[1]],"        ",subj_mean$SUBJECT,"\n")
cat("  DRUG/TRT     ",level[[2]],"        ",sort(as.numeric(levels(values$drug))),"\n")
cat("  SEQUENCE     ",level[[3]],"        ",sort(as.numeric(levels(values$seq))),"\n")
cat("  PERIOD       ",level[[4]],"        ",sort(as.numeric(levels(values$prd))),"\n")
cat("--------\n")
cat(" DRUG 1: the Ref. product; DRUG 2: the Test product \n")
cat("----------------------------------------------------\n")
cat("\n")

cat(" Means                  \n")
cat("----------------------------------------------------\n")

show(seq_mean)
cat("\n")
cat("\n")

show(subj_mean)
cat("\n")
cat("\n")
          
show(prd_mean)
cat("\n")
cat("\n")
                   
show(drug_mean)
cat("\n")
cat("\n")
RepMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
        lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
} 
cat("\n\n")
sink()
close(zz)
} 