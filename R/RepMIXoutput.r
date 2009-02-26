RepMIXoutput<-function(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF
                       ,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2
                       ,parallel=FALSE)
{
filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" Files have been output to the directory of                                 \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" lme_stat.txt                                                               \n")
cat("    --> lme: Cmax, AUC0t, AUC0inf, ln(Cmax), ln(AUC0t), ln(AUC0inf)           \n")
cat("    --> 90%CI: ln(Cmax), ln(AUC0t), and ln(AUC0inf)                          \n")
cat("****************************************************************************\n")
cat("\n")
values <- lapply(TotalData, unique)
level <-sapply(values, length)

if(parallel){
TotalData1<-data.frame(subj=as.factor(TotalData$subj), drug=as.numeric(TotalData$drug),
                      Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF,
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF, 
                      lnCmax=rdrug$lnCmax,lnAUC0t=rdrug$lnAUC0t,lnAUC0INF=rdrug$lnAUC0INF)                
}
else {
TotalData1<-data.frame (subj=TotalData$subj, drug=as.numeric(TotalData$drug),seq=as.numeric(TotalData$seq),
                      prd=as.numeric(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF, 
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF) 
rsubj<-as.data.frame(sapply(TotalData1, tapply, TotalData1$subj, mean))
rdrug<-as.data.frame(sapply(TotalData1, tapply, TotalData1$drug, mean))
rseq<-as.data.frame(sapply(TotalData1, tapply, TotalData1$seq, mean))
rprd<-as.data.frame(sapply(TotalData1, tapply, TotalData1$prd, mean))

subj_mean11<-data.frame(SUBJECT=as.numeric(levels(values$subj)),SEQUENCE=rsubj$seq,Cmax=rsubj$Cmax, AUC0t=rsubj$AUC0t, AUC0INF=rsubj$AUC0INF, 
                      lnCmax=rsubj$lnCmax,lnAUC0t=rsubj$lnAUC0t,lnAUC0INF=rsubj$lnAUC0INF)                      
subj_mean1<-subj_mean11[ do.call(order, subj_mean11) ,]
subj_mean<-na.omit(subj_mean1)
seq_mean<-data.frame(SEQUENCE=rseq$seq,Cmax=rseq$Cmax, AUC0t=rseq$AUC0t, AUC0INF=rseq$AUC0INF, 
                    lnCmax=rseq$lnCmax,lnAUC0t=rseq$lnAUC0t,lnAUC0INF=rseq$lnAUC0INF)
prd_mean<-data.frame(PERIOD=rprd$prd,Cmax=rprd$Cmax, AUC0t=rprd$AUC0t, AUC0INF=rprd$AUC0INF, 
                      lnCmax=rprd$lnCmax,lnAUC0t=rprd$lnAUC0t,lnAUC0INF=rprd$lnAUC0INF)            
drug_mean<-data.frame(DRUG=rdrug$drug,Cmax=rdrug$Cmax, AUC0t=rdrug$AUC0t, AUC0INF=rdrug$AUC0INF, 
                      lnCmax=rdrug$lnCmax,lnAUC0t=rdrug$lnAUC0t,lnAUC0INF=rdrug$lnAUC0INF)   
                      
}
zz <- file("lme_stat.txt", open="wt")
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
if(parallel){
cat("  SUBJECT     ",level[[1]],"        ",sort(as.numeric(levels(values$subj))) ,"\n")
cat("  DRUG         ",level[[2]],"        ",sort(as.numeric(levels(values$drug))),"\n")
cat("-----------\n")
cat("DRUG 1: the Ref. product; DRUG 2: the Test product\n")
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

cat(" Means                  \n")
cat("--------------------------------------------------------------------------\n") 
      
show(drug_mean)
cat("\n")
cat("\n")
cat("\n")                     
 ParaMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
         lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
 }
else{ 
cat("  SUBJECT     ",level[[1]],"        ",subj_mean$SUBJECT,"\n")
cat("  DRUG         ",level[[2]],"        ",sort(as.numeric(levels(values$drug))),"\n")
cat("  SEQUENCE     ",level[[3]],"        ",sort(as.numeric(levels(values$seq))),"\n")
cat("  PERIOD       ",level[[4]],"        ",sort(as.numeric(levels(values$prd))),"\n")
cat("--------\n")
cat("DRUG 1: the Ref. product; DRUG 2: the Test product\n")
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

cat(" Means                  \n")
cat("--------------------------------------------------------------------------\n") 

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
cat("\n")
RepMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
        lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
} 
cat("\n")
sink()
 
} 