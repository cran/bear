#data split for replicated study 
RepMIXanalyze<-function(TotalData, separateWindows=TRUE,
                        parallel=FALSE)
{
if(parallel){
description_ParaMIX()
TotalData<-data.frame(subj=as.factor(TotalData$subj), drug=as.factor(TotalData$drug),
                      Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF,
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
}
else{ 
description_RepMIX()
TotalData<-data.frame (subj=as.factor(TotalData$subj), drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF,
                   lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
} 
Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]


cat("\n")
cat("Enter lower acceptance limit (%) for lnCmax\n")
cat("(or press Enter to use default value: 80 )\n")
Lm<-readline()
if (substr(Lm, 1, 1) == ""|| Lm<=0)  Lm<-80  else Lm<-as.numeric(Lm)
lnCmax_theta1 <- Lm/100      # theta1: lower acceptance limit
lnCmax_theta2 <- 1/lnCmax_theta1

cat("\n")
cat("Enter lower acceptance limit (%) for lnAUC0t\n")
cat("(or press Enter to use default value: 80 )\n")
Lm<-readline()
if (substr(Lm, 1, 1) == ""|| Lm<=0)  Lm<-80  else Lm<-as.numeric(Lm)
lnAUC0t_theta1 <- Lm/100      # theta1: lower acceptance limit
lnAUC0t_theta2 <- 1/lnAUC0t_theta1

cat("\n")
cat("Enter lower acceptance limit (%) for lnAUC0INF\n")
cat("(or press Enter to use default value: 80 )\n")
Lm<-readline()
if (substr(Lm, 1, 1) == ""|| Lm<=0)  Lm<-80  else Lm<-as.numeric(Lm)
lnAUC0INF_theta1 <- Lm/100      # theta1: lower acceptance limit
lnAUC0INF_theta2 <- 1/lnAUC0INF_theta1


if(parallel){
L1<-length(RefData$subj)
L2<-length(TestData$subj)
 }
else{ 
SeqLeg<-split(RefData, list(RefData$seq))
SeqLeg1 <- reshape(SeqLeg[[1]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")
SeqLeg2 <- reshape(SeqLeg[[2]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")  
L1<-length(SeqLeg1$subj)
L2<-length(SeqLeg2$subj)
} 
ref_lnCmax<-mean(RefData$lnCmax)
ref_lnAUC0t<-mean(RefData$lnAUC0t)
ref_lnAUC0INF<-mean(RefData$lnAUC0INF)

test_lnCmax<-mean(TestData$lnCmax)
test_lnAUC0t<-mean(TestData$lnAUC0t)
test_lnAUC0INF<-mean(TestData$lnAUC0INF)

if(parallel){
ParaMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
ParaMIXoutput(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{ 
RepMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
RepMIXoutput(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
  } 
}
