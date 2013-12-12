###
### this is for replicated study; if non-replicate --> BANOVAanalyze()  --YJ
###
RepMIXanalyze<-function(TotalData, separateWindows=TRUE,
                        parallel=FALSE, multiple=FALSE)
{
lin.AUC<-lin.AUC
pAUC<-pAUC               ### for pAUC
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz

if(parallel){
description_ParaMIX()
  if(multiple){
  if(pAUC){
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),Cmax=TotalData$Cmax_ss,AUC0t=TotalData$AUCtau_ss, 
                        partAUC=TotalData$partAUC,lnCmax=TotalData$lnCmax_ss,lnAUC0t=TotalData$lnAUCtau_ss,lnpAUC=TotalData$lnpAUC)
  }
  else{
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),Cmax=TotalData$Cmax_ss, AUC0t=TotalData$AUCtau_ss, 
                        lnCmax=TotalData$lnCmax_ss,lnAUC0t=TotalData$lnAUCtau_ss)
   }
  }
  else{
  if(pAUC){
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),
                      Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,partAUC=TotalData$partAUC,
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF,lnpAUC=TotalData$lnpAUC)
  }
  else{                 
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),
                      Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,
                      lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
  }
  }
}
else{ 
description_RepMIX()
if(pAUC){
TotalData<-data.frame (subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,
                   partAUC=TotalData$partAUC,lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF,
                   lnpAUC=TotalData$lnpAUC)
}
else{
TotalData<-data.frame (subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,
                   lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)
 }
} 
Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

cat("\n")
if(multiple){
  lnCmax_theta1    <- BE_LL        # theta1: lower acceptance limit
  lnCmax_theta2    <- BE_UL
  lnAUC0t_theta1   <- BE_LL        # theta1: lower acceptance limit; here lnAUC0t = lnAUC(tau)ss  --YJ
  lnAUC0t_theta2   <- BE_UL
}
else{
  lnCmax_theta1    <- BE_LL        # theta1: lower acceptance limit
  lnCmax_theta2    <- BE_UL
  lnAUC0t_theta1   <- BE_LL        # theta1: lower acceptance limit
  lnAUC0t_theta2   <- BE_UL
  lnAUC0INF_theta1 <- BE_LL        # theta1: lower acceptance limit
  lnAUC0INF_theta2 <- BE_UL
}
if(pAUC){lnpAUC_theta1<- BE_LL;lnpAUC_theta2<- BE_UL}

ref_lnCmax<-mean(RefData$lnCmax)        ### if multiple-dose, lnCmax = lnCmax_ss
ref_lnAUC0t<-mean(RefData$lnAUC0t)      ### if multiple-dose, lnAUC0t = lnAUC(tau)ss
if(pAUC) ref_lnpAUC<-mean(RefData$lnpAUC)

test_lnCmax<-mean(TestData$lnCmax)      ### if multiple-dose, lnCmax = lnCmax_ss
test_lnAUC0t<-mean(TestData$lnAUC0t)    ### if multiple-dose, lnAUC0t = lnAUC(tau)ss
if(pAUC) test_lnpAUC<-mean(TestData$lnpAUC)

if(multiple){
}
else{                                   ### of course, lnAUC0INF -> only for single-dose BE here. -YJ
ref_lnAUC0INF<-mean(RefData$lnAUC0INF)
test_lnAUC0INF<-mean(TestData$lnAUC0INF)
}

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


if(parallel){
  if(multiple){
     if(pAUC){
     MultipleParaMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnpAUC,
     lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnpAUC_theta1,lnpAUC_theta2)
     MultipleParaMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnpAUC,test_lnCmax,test_lnAUC0t,
     test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnpAUC_theta1,lnpAUC_theta2)
     }
     else{
     if(pAUC){
     MultipleParaMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnpAUC,
     lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnpAUC_theta1,lnpAUC_theta2)
     MultipleParaMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnpAUC,
     lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnpAUC_theta1,lnpAUC_theta2)
     }
     else{
     MultipleParaMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,test_lnCmax,test_lnAUC0t,lnCmax_theta1,lnCmax_theta2,
     lnAUC0t_theta1,lnAUC0t_theta2)
     MultipleParaMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,test_lnCmax,test_lnAUC0t,lnCmax_theta1,
     lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
     }
     }
  }
  else{
    if(pAUC){
    ParaMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,
    lnpAUC_theta2)
    ParaMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,
    lnpAUC_theta2)
    }
    else{
    ParaMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
    ParaMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
    }
  }
}
else{ 
    if(pAUC){
    RepMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
    lnpAUC_theta1,lnpAUC_theta2)
    RepMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
    lnpAUC_theta1,lnpAUC_theta2)
    }
    else{
    RepMIX(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
    RepMIXoutput(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
    }
  } 
}
