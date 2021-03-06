### will be called by BANOVAAnalyze() once.
### This is for non-replicate,non-parallel only; parallel or replicated --> RepMIX() --YJ
##ANOVA (lm)
library(ICSNP)
BANOVA<-function(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       IntraInterlnpAUC00,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,lnpAUC_theta2,
       multiple=FALSE)
{
#theta1:  lower acceptance limit
#theta2:  higher acceptance limit
#represent GLM
##
options(width=100)
pAUC<-pAUC               ### for pAUC
oda_output_xfile<- oda_output_xfile
ODAnalysis<- ODAnalysis
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz


####
  lnCmax_theta1    <- BE_LL        # theta1: lower acceptance limit
  lnCmax_theta2    <- BE_UL
  lnAUC0t_theta1   <- BE_LL        # theta1: lower acceptance limit
  lnAUC0t_theta2   <- BE_UL
  lnAUC0INF_theta1 <- BE_LL        # theta1: lower acceptance limit
  lnAUC0INF_theta2 <- BE_UL

  lnCmax_ss<-NULL
  lnAUCtau_ss<-NULL
  
if(pAUC) {lnpAUC_theta1<-BE_LL; lnpAUC_theta2<-BE_UL}   ### something can be wrong if this line is req.  -YJ
####

##
if(multiple){
cat("*** A 2-trt,2-seq,and 2-period crossover multiple-dose design.\n\n")
description_BE_criteria(BE_LL,BE_UL)
if(pAUC){
Data<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                 prd=as.factor(TotalData$prd),Cmax_ss=TotalData$Cmax,AUCtau_ss=TotalData$AUC0t,
                 partAUC=TotalData$partAUC)
}
else{
Data<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                 prd=as.factor(TotalData$prd),Cmax_ss=TotalData$Cmax,AUCtau_ss=TotalData$AUC0t)
}
}
else{
cat("*** A 2-trt,2-seq,and 2-period crossover single-dose design.\n\n")
description_BE_criteria(BE_LL,BE_UL)
}
cat("\n")
cat("             Statistical analysis (ANOVA(lm))              \n")
cat("-----------------------------------------------------------\n")
if(multiple){
cat("   Dependent Variable: Cmax_ss                       \n")
lm.mod(Data$Cmax_ss, Data)  ### or can be written as 'lm.mod(TotalData$Cmax, TotalData)' shoud be the same.
###
### the following is the same as Cmax_ss<- lm(Cmax_ss ~ seq + subj:seq + prd + drug ,data=Data)!
### the reason to do this is that we don't have to re-write more codes (around lines 277- 408) for multiple-dose study;
### the data for multiple-dose was ported from TotalData with the same data column; Cmax of single-dose is
### replaced by Cmax_ss in the case of multiple-dose.
###
### Cmax<- lm(Cmax ~ seq + subj:seq + prd + drug ,data=TotalData)  ### not req. for later use; only for log(target)!
}
else{
cat("   Dependent Variable: Cmax                       \n")
lm.mod(TotalData$Cmax, TotalData)
### Cmax<- lm(Cmax ~ seq + subj:seq + prd + drug,data=TotalData)
}
cat("\n\n")

#GLM_AUC0t.txt
cat("            Statistical analysis (ANOVA(lm))               \n")
cat("-----------------------------------------------------------\n")
if(multiple){
cat("   Dependent Variable: AUC(tau)ss                 \n")
lm.mod(Data$AUCtau_ss, Data)
### AUC0t<- lm(AUC0t ~ seq + subj:seq + prd + drug ,data=TotalData)
}
else{
cat("   Dependent Variable: AUC0t                      \n")
lm.mod(TotalData$AUC0t, TotalData)
### AUC0t<- lm(AUC0t ~ seq + subj:seq + prd + drug ,data=TotalData)
}
cat("\n\n")
###
### GLM for pAUC
###
if(pAUC){
#GLM_pAUC.txt
cat("             Statistical analysis (ANOVA(lm))              \n")
cat("-----------------------------------------------------------\n")
cat("   Dependent Variable: pAUC                                \n")
lm.mod(TotalData$partAUC, TotalData)
### AUC0INF<- lm(partAUC ~ seq + subj:seq + prd + drug ,data=TotalData)
}
else{
if(multiple){    ### do nohting for multiple-dose since there is no 'AUC0INF' at all.
}
else{
#GLM_AUC0INF.txt
cat("             Statistical analysis (ANOVA(lm))              \n")
cat("-----------------------------------------------------------\n")
cat("   Dependent Variable: AUC0INF                  \n") 
lm.mod(TotalData$AUC0INF, TotalData)
### AUC0INF<- lm(AUC0INF ~ seq + subj:seq + prd + drug ,data=TotalData)
}
}
cat("\n\n")
#lnCmax or lnCmax_ss
cat("              Statistical analysis (ANOVA(lm))             \n")
cat("-----------------------------------------------------------\n")
if(multiple){
cat("   Dependent Variable: log(Cmax_ss)    \n")
lm.mod(log(Data$Cmax_ss), Data)
### lnCmax_ss<- lm(log(Cmax) ~ seq + subj:seq + prd + drug ,data=TotalData)   ### yes! for later user. error in v2.6.2. -YJ
### lnCmax<- lm(log(Cmax) ~ seq + subj:seq + prd + drug ,data=TotalData)      ### here lnCmax ==lnCmax_ss
}
else{
cat("   Dependent Variable: log(Cmax)                           \n")
lm.mod(log(TotalData$Cmax), TotalData)
### lnCmax<- lm(log(Cmax)~ seq + subj:seq + prd + drug,data=TotalData)    ### yes! for later user
}
cat("\n")
lnCmax<- lm(log(Cmax)~ seq + subj:seq + prd + drug,data=TotalData)  ### yes! for later user; ; here lnCmax ==lnCmax_ss for md.
cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnCmax)[5,3])-1),format="f",digits=3),"%\n")
cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
if(anova(lnCmax)[4,3]<anova(lnCmax)[5,3]){
   cat("               =",0,"% (due to a negative variance component)\n")
   cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
   }
else{
   cat("               =",formatC(100*sqrt(exp((anova(lnCmax)[4,3]-anova(lnCmax)[5,3])/2)-1),format="f",digits=3),"%\n")
}
cat("    MSResidual =",anova(lnCmax)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnCmax)[4,3],"\n")
### re-marked the following
###
### if(multiple){
### cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnCmax_ss)[5,3])-1),format="f",digits=3),"%\n")
### cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
### if(anova(lnCmax_ss)[4,3]<anova(lnCmax_ss)[5,3]){
###    cat("               =",0,"% (due to a negative variance component)\n")
###    cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
###    }
###  else{
###    cat("               =",formatC(100*sqrt(exp((anova(lnCmax_ss)[4,3]-anova(lnCmax_ss)[5,3])/2)-1),format="f",digits=3),"%\n")
### }
### cat("    MSResidual =",anova(lnCmax_ss)[5,3],"\n")
### cat("MSSubject(seq) =",anova(lnCmax_ss)[4,3],"\n")
### }
### else{
### cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnCmax)[5,3])-1),format="f",digits=3),"%\n")
### cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
### if(anova(lnCmax)[4,3]<anova(lnCmax)[5,3]){
###    cat("               =",0,"% (due to a negative variance component)\n")
###    cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
###    }
###  else{
###    cat("               =",formatC(100*sqrt(exp((anova(lnCmax)[4,3]-anova(lnCmax)[5,3])/2)-1),format="f",digits=3),"%\n")
### }
### cat("    MSResidual =",anova(lnCmax)[5,3],"\n")
### cat("MSSubject(seq) =",anova(lnCmax)[4,3],"\n")
### }
cat("\n\n")

#lnAUC0t or lnAUCtau_ss

cat("              Statistical analysis (ANOVA(lm))             \n")
cat("-----------------------------------------------------------\n")
if(multiple){
cat("   Dependent Variable: log(AUCtau_ss) \n")
lm.mod(log(Data$AUCtau_ss), Data)
### lnAUCtau_ss<- lm(log(AUC0t) ~ seq + subj:seq + prd + drug ,data=TotalData)   ### yes! for later user. error in v2.6.2. -YJ
### lnAUC0t<- lm(log(AUC0t) ~ seq + subj:seq + prd + drug ,data=TotalData)       ### same as lnCmax_ss; should be simplified here.
}
else{
cat("   Dependent Variable: log(AUC0t)     \n")
lm.mod(log(TotalData$AUC0t), TotalData)
### lnAUC0t<- lm(log(AUC0t) ~ seq + subj:seq + prd + drug ,data=TotalData)       ### yes! for later user.
}
cat("\n")
lnAUC0t<- lm(log(AUC0t) ~ seq + subj:seq + prd + drug ,data=TotalData)  ### yes! for later user; here lnAUC0t == lnAUCtau_ss for md.
cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnAUC0t)[5,3])-1),format="f",digits=3),"%\n")
cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
if(anova(lnAUC0t)[4,3]<anova(lnAUC0t)[5,3]){
   cat("               =",0,"% (due to a negative variance component)\n")
   cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
   }
 else{
   cat("               =",formatC(100*sqrt(exp((anova(lnAUC0t)[4,3]-anova(lnAUC0t)[5,3])/2)-1),format="f",digits=3),"%\n")
}
cat("    MSResidual =",anova(lnAUC0t)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnAUC0t)[4,3],"\n")
### if(multiple){
### cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnAUCtau_ss)[5,3])-1),format="f",digits=3),"%\n")
### cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
### if(anova(lnAUCtau_ss)[4,3]<anova(lnAUCtau_ss)[5,3]){
###    cat("               =",0,"% (due to a negative variance component)\n")
###    cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
###    }
###  else{
###    cat("               =",formatC(100*sqrt(exp((anova(lnAUCtau_ss)[4,3]-anova(lnAUCtau_ss)[5,3])/2)-1),format="f",digits=3),"%\n")
### }
### cat("    MSResidual =",anova(lnAUCtau_ss)[5,3],"\n")
### cat("MSSubject(seq) =",anova(lnAUCtau_ss)[4,3],"\n")
### }
### else{
### cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnAUC0t)[5,3])-1),format="f",digits=3),"%\n")
### cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
### if(anova(lnAUC0t)[4,3]<anova(lnAUC0t)[5,3]){
###    cat("               =",0,"% (due to a negative variance component)\n")
###    cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
###    }
###  else{
###    cat("               =",formatC(100*sqrt(exp((anova(lnAUC0t)[4,3]-anova(lnAUC0t)[5,3])/2)-1),format="f",digits=3),"%\n")
### }
### cat("    MSResidual =",anova(lnAUC0t)[5,3],"\n")
### cat("MSSubject(seq) =",anova(lnAUC0t)[4,3],"\n")

### for negative variance component; run lme(); see Bebac Forum for more details. -YJ
## ctrl <- lmeControl(opt='optim')
## cat("\n\n  Statistical analysis (lme) - 2x2x2 BE study               \n")
## modlnAUC0t<-lme(log(AUC0t) ~ drug + seq + prd,
##                random=~1|subj/seq, ### control=ctrl,
##                ### weights=varIdent(form = ~ 1 |subj/seq), 
##                data=TotalData, method="REML")
## cat("--------------------------------------------------------------------------\n")
## cat("  Dependent Variable: log(AUC0t)                                 \n")       
## cat("\n")
## print(summary(modlnAUC0t))
## ### add two new lines here
## ### cat("\n Variance components for method=REML\n\n");VarCorr(modlnAUC0t);cat("\n\n")
## ### getVarCov(modlnAUC0t, type = "marginal");cat("\n\n")
## print(anova(modlnAUC0t))
## ###
## cat("\n")
## cat("Type I Tests of Fixed Effects\n")
## print(anova(modlnAUC0t)[2:4,])
## cat("\n")
## cat("Type III Tests of Fixed Effects\n")
## print(anova(modlnAUC0t, type="marginal")[2:4,])
## cat("\n\n")
## ### prdcount==2 here (2x2x2)
## upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][4,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][4,3])*summary(modlnAUC0t)[20][[1]][4,2])
## lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][4,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][4,3])*summary(modlnAUC0t)[20][[1]][4,2])
## SlnAUC0t<-(summary(modlnAUC0t)[20][[1]][4,1])
## SE_lnAUC0t<-summary(modlnAUC0t)[20][[1]][4,2]
## TL_lnAUC0t<-(SlnAUC0t-log(lnAUC0t_theta1))/SE_lnAUC0t
## TU_lnAUC0t<-(SlnAUC0t-log(lnAUC0t_theta2))/SE_lnAUC0t
## PTL_lnAUC0t<-pt(TL_lnAUC0t,L1+L2-2)
## PTU_lnAUC0t<-pt(TU_lnAUC0t,L1+L2-2)
## TAH_lnAUC0t<-SlnAUC0t/SE_lnAUC0t
## NP_lnAUC0t<-log(lnAUC0t_theta2)/SE_lnAUC0t
## EP_lnAUC0t<- pt((abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2) - pt((-abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2)
## cat("**************** Classical (Shortest) 90% C.I. for lnAUC0t ****************\n")
## cat("\n")
## output<-data.frame(Point_estimate=c( formatC(100*exp(SlnAUC0t),format="f",digits=3)),
##                    CI90_lower=c(formatC(lowerAUC0t,format="f",digits=3)),
##                    CI90_upper=c(formatC(upperAUC0t,format="f",digits=3)))
## dimnames(output) <- list("Ratio",
##                           c("  Point Estimate",
##                             "  CI90 lower",
##                             "  CI90 upper" ))
## show(output)
## Point_estimate <- 100*exp(SlnAUC0t)
## CI90_lower <- lowerAUC0t
## delta_CI <- log(Point_estimate)-log(CI90_lower)
## MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
## CVintra <- 100*sqrt(exp(MSE)-1)
## cat("\n")
## cat(" The estimated intra-subject CV for lnAUC0t =",formatC(CVintra, format="f", digits=5),"%\n")
## cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
## cat("---------------------------------------------------------------------------\n") 
### end of for negative variance component
## }
###
### GLM for ln(pAUC)
###
cat("\n\n")
if(pAUC){
cat("              Statistical analysis (ANOVA(lm))             \n")
cat("-----------------------------------------------------------\n")
cat("   Dependent Variable: log(pAUC)    \n")
lm.mod(log(TotalData$partAUC), TotalData)
lnpAUC<- lm(log(partAUC) ~ seq + subj:seq + prd + drug ,data=TotalData)   ### yes! for later user.
cat("\n")
cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnpAUC)[5,3])-1),format="f",digits=3),"%\n")
cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
if(anova(lnpAUC)[4,3]<anova(lnpAUC)[5,3]){
   cat("               =",0,"% (due to a negative variance component)\n")
   cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
   }
 else{
   cat("               =",formatC(100*sqrt(exp((anova(lnpAUC)[4,3]-anova(lnpAUC)[5,3])/2)-1),format="f",digits=3),"%\n")
}
cat("    MSResidual =",anova(lnpAUC)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnpAUC)[4,3],"\n")
cat("\n")
}

if(multiple){    ### do nothing for multiple-dose since there is no 'AUC0INF' at all.
 }
else{
cat("              Statistical analysis (ANOVA(lm))             \n")
cat("-----------------------------------------------------------\n")
cat("   Dependent Variable: log(AUC0INF)    \n")
lm.mod(log(TotalData$AUC0INF),TotalData)
lnAUC0INF<- lm(log(AUC0INF) ~ seq + subj:seq + prd + drug, data=TotalData)   ### yes! for later user.
cat("\n")
cat("Intra_subj. CV = 100*sqrt(exp(MSResidual)-1) =",formatC(100*sqrt(exp(anova(lnAUC0INF)[5,3])-1),format="f",digits=3),"%\n")
cat("Inter_subj. CV = 100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)\n")
if(anova(lnAUC0INF)[4,3]<anova(lnAUC0INF)[5,3]){
   cat("               =",0,"% (due to a negative variance component)\n")
   cat("*** the above CV_intra is estimated from lm() which may be different\n    from that obtained from lme().\n\n")
   }
 else{
   cat("               =",formatC(100*sqrt(exp((anova(lnAUC0INF)[4,3]-anova(lnAUC0INF)[5,3])/2)-1),format="f",digits=3),"%\n")
}
cat("    MSResidual =",anova(lnAUC0INF)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnAUC0INF)[4,3],"\n")     
cat("\n")
}
#####################################################################
###shortest confidence interval 
#L1(Reference-->Test),L2(Test-->Reference sequence)
Todata<-split(TotalData,list(TotalData$prd,TotalData$seq))
T<-qt(0.95,(L1+L2-2))

ref_Cmax<-mean(RefData$lnCmax)
ref_AUC0t<-mean(RefData$lnAUC0t)
if(pAUC) ref_pAUC<-mean(RefData$lnpAUC)

test_Cmax<-mean(TestData$lnCmax)
test_AUC0t<-mean(TestData$lnAUC0t)
if(pAUC) test_pAUC<-mean(TestData$lnpAUC)

SE_Cmax<-sqrt((anova(lnCmax)[5,3]/2) * (1/L1+1/L2))
SE_AUC0t<-sqrt((anova(lnAUC0t)[5,3]/2) * (1/L1+1/L2))
if(pAUC) SE_pAUC<-sqrt((anova(lnpAUC)[5,3]/2) * (1/L1+1/L2))

est_lnCmax<-lnCmax$coef[[4]] 
est_lnAUC0t<-lnAUC0t$coef[[4]] 
if(pAUC) est_lnpAUC<-lnpAUC$coef[[4]] 
####
#### with unbalanced dataset (L1=!L2),est_lnCmax will not equal to (test_Cmax-ref_Cmax)!!
#### They will be equal when in balanced dataset (L1 = L2)
#### statistical_summaries.txt will be in the file - NCAoutput.r
####
#### cat("*** est_lnCamx =",est_lnCmax,"\n\n")
#### cat("*** test_Cmax-ref_Cmax =",test_Cmax-ref_Cmax,"\n\n")

#### lowerCmax<-100*exp((test_Cmax-ref_Cmax)-(T*SE_Cmax))
#### upperCmax<-100*exp((test_Cmax-ref_Cmax)+(T*SE_Cmax))
#### lowerAUC0t<-100*exp((test_AUC0t-ref_AUC0t)-(T*SE_AUC0t))
#### upperAUC0t<-100*exp((test_AUC0t-ref_AUC0t)+(T*SE_AUC0t))

lowerCmax<-100*exp(est_lnCmax-(T*SE_Cmax))
upperCmax<-100*exp(est_lnCmax+(T*SE_Cmax))
lowerAUC0t<-100*exp(est_lnAUC0t-(T*SE_AUC0t))
upperAUC0t<-100*exp(est_lnAUC0t+(T*SE_AUC0t))
if(pAUC){
lowerpAUC<-100*exp(est_lnpAUC-(T*SE_pAUC))
upperpAUC<-100*exp(est_lnpAUC+(T*SE_pAUC))
}

###two-one side and Anderson and Hauck's test
#lnCmax or lnCmax_ss
TL_lnCmax<--((test_Cmax-ref_Cmax)-log(lnCmax_theta1))/SE_Cmax     ### there is a '-' (minus sign) at the front. -YJ
TU_lnCmax<-((test_Cmax-ref_Cmax)-log(lnCmax_theta2))/SE_Cmax
PTL_lnCmax<-pt(TL_lnCmax,L1+L2-2)
PTU_lnCmax<-pt(TU_lnCmax,L1+L2-2)
TAH_lnCmax<-(test_Cmax-ref_Cmax)/SE_Cmax
NP_lnCmax<-log(lnCmax_theta2)/SE_Cmax
EP_lnCmax<- pt((abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2) - pt((-abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2)

#lnAUC0t or lnAUCtau_ss
TL_lnAUC0t<--((test_AUC0t-ref_AUC0t)-log(lnAUC0t_theta1))/SE_AUC0t
TU_lnAUC0t<-((test_AUC0t-ref_AUC0t)-log(lnAUC0t_theta2))/SE_AUC0t
PTL_lnAUC0t<-pt(TL_lnAUC0t,L1+L2-2)
PTU_lnAUC0t<-pt(TU_lnAUC0t,L1+L2-2)
TAH_lnAUC0t<-(test_AUC0t-ref_AUC0t)/SE_AUC0t
NP_lnAUC0t<-log(lnAUC0t_theta2)/SE_AUC0t
EP_lnAUC0t<- pt((abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2) - pt((-abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2)

### ln(pAUC)
if(pAUC){
TL_lnpAUC<--((test_pAUC-ref_pAUC)-log(lnpAUC_theta1))/SE_pAUC
TU_lnpAUC<-((test_pAUC-ref_pAUC)-log(lnpAUC_theta2))/SE_pAUC
PTL_lnpAUC<-pt(TL_lnpAUC,L1+L2-2)
PTU_lnpAUC<-pt(TU_lnpAUC,L1+L2-2)
TAH_lnpAUC<-(test_pAUC-ref_pAUC)/SE_pAUC
NP_lnpAUC<-log(lnpAUC_theta2)/SE_AUC0t
EP_lnpAUC<- pt((abs(TAH_lnpAUC)-NP_lnpAUC),L1+L2-2) - pt((-abs(TAH_lnpAUC)-NP_lnpAUC),L1+L2-2)
}

if(multiple){
}
else{
ref_AUC0INF<-mean(RefData$lnAUC0INF)
test_AUC0INF<-mean(TestData$lnAUC0INF)
SE_AUC0INF<-sqrt((anova(lnAUC0INF)[5,3]/2) * (1/L1+1/L2))
est_lnAUC0INF<-lnAUC0INF$coef[[4]] 
#### read line# 389-395 (above)
LowerAUC0INF<-100*exp(est_lnAUC0INF-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp(est_lnAUC0INF+(T*SE_AUC0INF))           
#### LowerAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)-(T*SE_AUC0INF))
#### UpperAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)+(T*SE_AUC0INF))
#lnAUC0INF
TL_lnAUC0INF<--((test_AUC0INF-ref_AUC0INF)-log(lnAUC0INF_theta1))/SE_AUC0INF
TU_lnAUC0INF<-((test_AUC0INF-ref_AUC0INF)-log(lnAUC0INF_theta2))/SE_AUC0INF
PTL_lnAUC0INF<-pt(TL_lnAUC0INF,L1+L2-2)
PTU_lnAUC0INF<-pt(TU_lnAUC0INF,L1+L2-2)
TAH_lnAUC0INF<-(test_AUC0INF-ref_AUC0INF)/SE_AUC0INF
NP_lnAUC0INF<-log(lnAUC0INF_theta2)/SE_AUC0INF
EP_lnAUC0INF<- pt((abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2)-pt((-abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2)
}

###################################################
Prddata<-split(TotalData,list(TotalData$prd))
 
#chi-square distribution
chinv_1<-qchisq(0.025,df= (L1+L2-2))
chinv_2<-qchisq(0.975,df= (L1+L2-2))
#F distribution
F_1<-qf(0.025,L1+L2-2,L1+L2-2)
F_2<-qf(0.975,L1+L2-2,L1+L2-2)

Prddata1<-Prddata[[1]][ do.call(order,Prddata[[1]]) ,]
Prddata2<-Prddata[[2]][ do.call(order,Prddata[[2]]) ,]
seqdata1<-split(Prddata1,list(Prddata1$seq))
seqdata2<-split(Prddata2,list(Prddata2$seq))

################################################## 

#intra- and inter- subject variability
lnCmax_inter<-(lnCmax_MSinter - lnCmax_MSintra)/2
lnCmax_intraclass<- (lnCmax_MSinter - lnCmax_MSintra)/(lnCmax_MSinter + lnCmax_MSintra)

lnCmax_Fv<-lnCmax_MSinter/lnCmax_MSintra 

#95% CI
lnCmax_Le<-lnCmax_SSintra/chinv_2 
lnCmax_Ue<-lnCmax_SSintra/chinv_1

lnCmax_Lp<-(lnCmax_Fv/F_2-1)/(lnCmax_Fv/F_2+1) 
lnCmax_Up<-(lnCmax_Fv/F_1-1)/(lnCmax_Fv/F_1+1)
  
lnCmax_Ls<-(lnCmax_SSinter*(1-F_2/lnCmax_Fv))/(2*chinv_2) 
lnCmax_Us<-(lnCmax_SSinter*(1-F_1/lnCmax_Fv))/(2*chinv_1) 

lnCmax_PFv<-pf(1/lnCmax_Fv,L1+L2-2,L1+L2-2)  

#############################
lnCmax_Total<-Prddata1$lnCmax + Prddata2$lnCmax
lnCmax_Totalmean<-mean(lnCmax_Total)


lnCmax_CD1<-seqdata2[[1]]$lnCmax - seqdata1[[1]]$lnCmax
pre_CD1<-data.frame(subj=seqdata2[[1]]$subj,lnCmax_CD=lnCmax_CD1)
lnCmax_CD2<-seqdata1[[2]]$lnCmax - seqdata2[[2]]$lnCmax
pre_CD2<-data.frame(subj=seqdata2[[2]]$subj,lnCmax_CD=lnCmax_CD2)
prelnCmax_CD<-rbind(pre_CD1,pre_CD2)
lnCmax_CD<-prelnCmax_CD[ do.call(order,prelnCmax_CD) ,]
lnCmax_CDmean<- mean(lnCmax_CD$lnCmax_CD)


lnCmax_TotalData<-data.frame(subj=Prddata1$subj,Total=lnCmax_Total,U=lnCmax_Total-lnCmax_Totalmean,U2=(lnCmax_Total-lnCmax_Totalmean)^2,
                             CD=lnCmax_CD$lnCmax_CD,V=lnCmax_CD$lnCmax_CD-lnCmax_CDmean,V2=(lnCmax_CD$lnCmax_CD-lnCmax_CDmean)^2,
                             UV=(lnCmax_Total-lnCmax_Totalmean)*(lnCmax_CD$lnCmax_CD-lnCmax_CDmean)) 

lnCmax_Svv<-sum(lnCmax_TotalData$V2)*(1/(L1+L2-1))
lnCmax_Suu<-sum(lnCmax_TotalData$U2)*(1/(L1+L2-1))
lnCmax_Svu<-sum(lnCmax_TotalData$UV)*(1/(L1+L2-1))

lnCmax_Srr<-sum((RefData$lnCmax - ref_Cmax)^2 )*(1/(L1+L2-1))
lnCmax_Stt<-sum((TestData$lnCmax - test_Cmax)^2 )*(1/(L1+L2-1))
lnCmax_Srt<-sum((RefData$lnCmax - ref_Cmax)*(TestData$lnCmax - test_Cmax))*(1/(L1+L2-1))

lnCmax_pearson_P<-cor.test(lnCmax_TotalData$V,lnCmax_TotalData$U,method=c("pearson"))[[3]]
lnCmax_pearson_V<-cor.test(lnCmax_TotalData$V,lnCmax_TotalData$U,method=c("pearson"))[[4]][[1]]
lnCmax_spearman_P<-cor.test(lnCmax_TotalData$V,lnCmax_TotalData$U,method=c("spearman"))[[3]]
lnCmax_spearman_V<-cor.test(lnCmax_TotalData$V,lnCmax_TotalData$U,method=c("spearman"))[[4]][[1]]

lnCmax_Frt<-lnCmax_Stt/lnCmax_Srr
lnCmax_rrt<-lnCmax_Srt/sqrt(lnCmax_Srr*lnCmax_Stt)

lnCmax_Fpm<-((L1+L2-2)*(lnCmax_Frt-1)^2 )/(4*(lnCmax_Frt)*(1-(lnCmax_rrt)^2))
lnCmax_PFpm<-1-pf(lnCmax_Fpm,1,L1+L2-2)
           
#Report for lnCmax,lnAUC0t and lnAUC0inf

cat("  Pivotal Parameters of BE Study - Summary Report \n")
cat("--------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: log(Cmax_ss)                \n")
}
else{
cat("  Dependent Variable: log(Cmax)                   \n")
}
cat("--------------------------------------------------\n")
cat("        n1(R -> T) =",L1 ,"\n")
cat("        n2(T -> R) =",L2 ,"\n")
cat("          N(n1+n2) =",L1+L2 ,"\n")
cat("    Lower criteria =",formatC(lnCmax_theta1*100,format="f",digits=3),"%\n")
cat("    Upper criteria =",formatC(lnCmax_theta2*100,format="f",digits=3),"%\n")
cat("          MEAN-ref =",ref_Cmax,"\n")
cat("         MEAN-test =",test_Cmax,"\n")
cat("               MSE =",anova(lnCmax)[5,3],"\n")
cat("                SE =",SE_Cmax,"\n")
cat("Estimate(test-ref) =",est_lnCmax,"\n")
cat("\n")
if(multiple){
cat("*** Classical (Shortest) 90% C.I. for log(Cmax_ss) ***\n")
}
else{
cat("*** Classical (Shortest) 90% C.I. for log(Cmax) ***\n")
}
cat("\n")
output<-data.frame(Point_estimate=c(formatC(100*exp(est_lnCmax),format="f",digits=3)),
                   CI90_lower=c(formatC(lowerCmax,format="f",digits=3)),
                   CI90_upper=c(formatC(upperCmax,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)    
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnCmax<-data.frame(TOST=c("T_lower","T_upper"),
                 T_value=c(formatC(TL_lnCmax,format="f",digits=3),formatC(TU_lnCmax,format="f",digits=3)),
                 P_value=c(formatC(PTL_lnCmax,format="f",digits=3),formatC(PTU_lnCmax,format="f",digits=3))) 
colnames(TOST_lnCmax)<- c("TOST","   T value","  P value")
show(TOST_lnCmax) 
cat("\n")
if(PTL_lnCmax >= 0.05 || PTU_lnCmax >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnCmax,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnCmax >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("---------------------------------------------------------------------------\n\n")
cat("             *** Intra-subject and Inter-subject Residuals ***            \n")
cat("--------------------------------------------------------------------------\n")
II_lnCmax<-data.frame(subj=IntraInterlnCmax00$subj,
                      Obs=formatC(IntraInterlnCmax00$Obs,format="f",digits=6),
                      Exp=formatC(IntraInterlnCmax00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnCmax00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnCmax00$Stud_Intra,format="f",digits=6),
                      Inter=formatC(IntraInterlnCmax00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnCmax00$Stud_Inter,format="f",digits=6))   
show(II_lnCmax)
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("Obs: Observed lnCmax_ss\n")
cat("Exp: Expected lnCmax_ss\n")
}
else{
cat("Obs: Observed lnCmax\n")
cat("Exp: Expected lnCmax\n")
}
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")

#GLM_lnAUC0t.txt
#intra- and inter- subject variability
lnAUC0t_inter<-(lnAUC0t_MSinter - lnAUC0t_MSintra)/2
lnAUC0t_intraclass<- (lnAUC0t_MSinter - lnAUC0t_MSintra)/(lnAUC0t_MSinter + lnAUC0t_MSintra)

lnAUC0t_Fv<-lnAUC0t_MSinter/lnAUC0t_MSintra

#95% CI
lnAUC0t_Le<-lnAUC0t_SSintra/chinv_2
lnAUC0t_Ue<-lnAUC0t_SSintra/chinv_1

lnAUC0t_Lp<-(lnAUC0t_Fv/F_2-1)/(lnAUC0t_Fv/F_2+1)
lnAUC0t_Up<-(lnAUC0t_Fv/F_1-1)/(lnAUC0t_Fv/F_1+1)

lnAUC0t_Ls<-(lnAUC0t_SSinter*(1-F_2/lnAUC0t_Fv))/(2*chinv_2)
lnAUC0t_Us<-(lnAUC0t_SSinter*(1-F_1/lnAUC0t_Fv))/(2*chinv_1)

lnAUC0t_PFv<-pf(1/lnAUC0t_Fv,L1+L2-2,L1+L2-2)

#############################
lnAUC0t_Total<-Prddata1$lnAUC0t + Prddata2$lnAUC0t
lnAUC0t_Totalmean<-mean(lnAUC0t_Total)


lnAUC0t_CD1<-seqdata2[[1]]$lnAUC0t - seqdata1[[1]]$lnAUC0t
pre_CD1<-data.frame(subj=seqdata2[[1]]$subj,lnAUC0t_CD=lnAUC0t_CD1)
lnAUC0t_CD2<-seqdata1[[2]]$lnAUC0t - seqdata2[[2]]$lnAUC0t
pre_CD2<-data.frame(subj=seqdata2[[2]]$subj,lnAUC0t_CD=lnAUC0t_CD2)
prelnAUC0t_CD<-rbind(pre_CD1,pre_CD2)
lnAUC0t_CD<-prelnAUC0t_CD[ do.call(order,prelnAUC0t_CD) ,]
lnAUC0t_CDmean<- mean(lnAUC0t_CD$lnAUC0t_CD)


lnAUC0t_TotalData<-data.frame(subj=Prddata1$subj,Total=lnAUC0t_Total,U=lnAUC0t_Total-lnAUC0t_Totalmean,U2=(lnAUC0t_Total-lnAUC0t_Totalmean)^2,
                             CD=lnAUC0t_CD$lnAUC0t_CD,V=lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean,V2=(lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean)^2,
                             UV=(lnAUC0t_Total-lnAUC0t_Totalmean)*(lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean))

lnAUC0t_Svv<-sum(lnAUC0t_TotalData$V2)*(1/(L1+L2-1))
lnAUC0t_Suu<-sum(lnAUC0t_TotalData$U2)*(1/(L1+L2-1))
lnAUC0t_Svu<-sum(lnAUC0t_TotalData$UV)*(1/(L1+L2-1))

lnAUC0t_Srr<-sum((RefData$lnAUC0t - ref_AUC0t)^2 )*(1/(L1+L2-1))
lnAUC0t_Stt<-sum((TestData$lnAUC0t - test_AUC0t)^2 )*(1/(L1+L2-1))
lnAUC0t_Srt<-sum((RefData$lnAUC0t - ref_AUC0t)*(TestData$lnAUC0t - test_AUC0t))*(1/(L1+L2-1))

lnAUC0t_pearson_P<-cor.test(lnAUC0t_TotalData$V,lnAUC0t_TotalData$U,method=c("pearson"))[[3]]
lnAUC0t_pearson_V<-cor.test(lnAUC0t_TotalData$V,lnAUC0t_TotalData$U,method=c("pearson"))[[4]][[1]]
lnAUC0t_spearman_P<-cor.test(lnAUC0t_TotalData$V,lnAUC0t_TotalData$U,method=c("spearman"))[[3]]
lnAUC0t_spearman_V<-cor.test(lnAUC0t_TotalData$V,lnAUC0t_TotalData$U,method=c("spearman"))[[4]][[1]]

lnAUC0t_Frt<-lnAUC0t_Stt/lnAUC0t_Srr
lnAUC0t_rrt<-lnAUC0t_Srt/sqrt(lnAUC0t_Srr*lnAUC0t_Stt)

lnAUC0t_Fpm<-((L1+L2-2)*(lnAUC0t_Frt-1)^2 )/(4*(lnAUC0t_Frt)*(1-(lnAUC0t_rrt)^2))
lnAUC0t_PFpm<-1-pf(lnAUC0t_Fpm,1,L1+L2-2)

cat("            Pivotal Parameters of BE Study - Summary Report               \n")
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: log(AUCtau_ss)                                     \n")
}
else{
cat("  Dependent Variable: log(AUC0t)                                          \n")
}
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 ,"\n")
cat("        n2(T -> R) =",L2 ,"\n")
cat("          N(n1+n2) =",L1+L2 ,"\n")
cat("    Lower criteria =",formatC(lnAUC0t_theta1*100,format="f",digits=3),"%\n")
cat("    Upper criteria =",formatC(lnAUC0t_theta2*100,format="f",digits=3),"%\n")
cat("          MEAN-ref =",ref_AUC0t,"\n")
cat("         MEAN-test =",test_AUC0t,"\n")
cat("               MSE =",anova(lnAUC0t)[5,3],"\n")
cat("                SE =",SE_AUC0t,"\n")
cat("Estimate(test-ref) =",est_lnAUC0t,"\n")
cat("\n")
if(multiple){
cat("*********** Classical (Shortest) 90% C.I. for lnAUC(tau)ss **************\n")
}
else{
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0t ****************\n")
}
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(est_lnAUC0t),format="f",digits=3)),
                   CI90_lower=c(formatC(lowerAUC0t,format="f",digits=3)),
                   CI90_upper=c(formatC(upperAUC0t,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnAUC0t<-data.frame(TOST=c("T_lower","T_upper"),
                 T_value=c(formatC(TL_lnAUC0t,format="f",digits=3),formatC(TU_lnAUC0t,format="f",digits=3)),
                 P_value=c(formatC(PTL_lnAUC0t,format="f",digits=3),formatC(PTU_lnAUC0t,format="f",digits=3))) 
colnames(TOST_lnAUC0t)<- c("TOST","   T value","  P value")
show(TOST_lnAUC0t) 
cat("\n")
if(PTL_lnAUC0t >= 0.05 || PTU_lnAUC0t >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnAUC0t,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnAUC0t >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("---------------------------------------------------------------------------\n\n")
cat("             *** Intra-subject and Inter-subject Residuals ***            \n")
cat("--------------------------------------------------------------------------\n")
II_lnAUC0t<-data.frame(subj=IntraInterlnAUC0t00$subj,
                      Obs=formatC(IntraInterlnAUC0t00$Obs,format="f",digits=6),
                      Exp=formatC(IntraInterlnAUC0t00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnAUC0t00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnAUC0t00$Stud_Intra,format="f",digits=6),
                      Inter=formatC(IntraInterlnAUC0t00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnAUC0t00$Stud_Inter,format="f",digits=6))   
show(II_lnAUC0t)
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("Obs: Observed lnAUC(tau)ss\n")
cat("Exp: Expected lnAUC(tau)ss\n")
}
else{
cat("Obs: Observed lnAUC0t\n")
cat("Exp: Expected lnAUC0t\n")
}
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")

### for pAUC
###
if(pAUC){
#intra- and inter- subject variability
lnpAUC_inter<-(lnpAUC_MSinter - lnpAUC_MSintra)/2
lnpAUC_intraclass<- (lnpAUC_MSinter - lnpAUC_MSintra)/(lnpAUC_MSinter + lnpAUC_MSintra)

lnpAUC_Fv<-lnpAUC_MSinter/lnpAUC_MSintra

#95% CI
lnpAUC_Le<-lnpAUC_SSintra/chinv_2
lnpAUC_Ue<-lnpAUC_SSintra/chinv_1

lnpAUC_Lp<-(lnpAUC_Fv/F_2-1)/(lnpAUC_Fv/F_2+1)
lnpAUC_Up<-(lnpAUC_Fv/F_1-1)/(lnpAUC_Fv/F_1+1)

lnpAUC_Ls<-(lnpAUC_SSinter*(1-F_2/lnpAUC_Fv))/(2*chinv_2)
lnpAUC_Us<-(lnpAUC_SSinter*(1-F_1/lnpAUC_Fv))/(2*chinv_1)

lnpAUC_PFv<-pf(1/lnpAUC_Fv,L1+L2-2,L1+L2-2)

#############################
lnpAUC_Total<-Prddata1$lnpAUC + Prddata2$lnpAUC
lnpAUC_Totalmean<-mean(lnpAUC_Total)


lnpAUC_CD1<-seqdata2[[1]]$lnpAUC - seqdata1[[1]]$lnpAUC
pre_CD1<-data.frame(subj=seqdata2[[1]]$subj,lnpAUC_CD=lnpAUC_CD1)
lnpAUC_CD2<-seqdata1[[2]]$lnpAUC - seqdata2[[2]]$lnpAUC
pre_CD2<-data.frame(subj=seqdata2[[2]]$subj,lnpAUC_CD=lnpAUC_CD2)
prelnpAUC_CD<-rbind(pre_CD1,pre_CD2)
lnpAUC_CD<-prelnpAUC_CD[ do.call(order,prelnpAUC_CD) ,]
lnpAUC_CDmean<- mean(lnpAUC_CD$lnpAUC_CD)


lnpAUC_TotalData<-data.frame(subj=Prddata1$subj,Total=lnpAUC_Total,U=lnpAUC_Total-lnpAUC_Totalmean,U2=(lnpAUC_Total-lnpAUC_Totalmean)^2,
                             CD=lnpAUC_CD$lnpAUC_CD,V=lnpAUC_CD$lnpAUC_CD-lnpAUC_CDmean,V2=(lnpAUC_CD$lnpAUC_CD-lnpAUC_CDmean)^2,
                             UV=(lnpAUC_Total-lnpAUC_Totalmean)*(lnpAUC_CD$lnpAUC_CD-lnpAUC_CDmean))

lnpAUC_Svv<-sum(lnpAUC_TotalData$V2)*(1/(L1+L2-1))
lnpAUC_Suu<-sum(lnpAUC_TotalData$U2)*(1/(L1+L2-1))
lnpAUC_Svu<-sum(lnpAUC_TotalData$UV)*(1/(L1+L2-1))

lnpAUC_Srr<-sum((RefData$lnpAUC - ref_pAUC)^2 )*(1/(L1+L2-1))
lnpAUC_Stt<-sum((TestData$lnpAUC - test_pAUC)^2 )*(1/(L1+L2-1))
lnpAUC_Srt<-sum((RefData$lnpAUC - ref_pAUC)*(TestData$lnpAUC - test_pAUC))*(1/(L1+L2-1))

lnpAUC_pearson_P<-cor.test(lnpAUC_TotalData$V,lnpAUC_TotalData$U,method=c("pearson"))[[3]]
lnpAUC_pearson_V<-cor.test(lnpAUC_TotalData$V,lnpAUC_TotalData$U,method=c("pearson"))[[4]][[1]]
lnpAUC_spearman_P<-cor.test(lnpAUC_TotalData$V,lnpAUC_TotalData$U,method=c("spearman"))[[3]]
lnpAUC_spearman_V<-cor.test(lnpAUC_TotalData$V,lnpAUC_TotalData$U,method=c("spearman"))[[4]][[1]]

lnpAUC_Frt<-lnpAUC_Stt/lnpAUC_Srr
lnpAUC_rrt<-lnpAUC_Srt/sqrt(lnpAUC_Srr*lnpAUC_Stt)

lnpAUC_Fpm<-((L1+L2-2)*(lnpAUC_Frt-1)^2 )/(4*(lnpAUC_Frt)*(1-(lnpAUC_rrt)^2))
lnpAUC_PFpm<-1-pf(lnpAUC_Fpm,1,L1+L2-2)

cat("            Pivotal Parameters of BE Study - Summary Report               \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: log(pAUC)                                          \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 ,"\n")
cat("        n2(T -> R) =",L2 ,"\n")
cat("          N(n1+n2) =",L1+L2 ,"\n")
cat("    Lower criteria =",formatC(lnpAUC_theta1*100,format="f",digits=3),"%\n")
cat("    Upper criteria =",formatC(lnpAUC_theta2*100,format="f",digits=3),"%\n")
cat("          MEAN-ref =",ref_pAUC,"\n")
cat("         MEAN-test =",test_pAUC,"\n")
cat("               MSE =",anova(lnpAUC)[5,3],"\n")
cat("                SE =",SE_pAUC,"\n")
cat("Estimate(test-ref) =",est_lnpAUC,"\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnpAUC ****************\n")
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(est_lnpAUC),format="f",digits=3)),
                   CI90_lower=c(formatC(lowerpAUC,format="f",digits=3)),
                   CI90_upper=c(formatC(upperpAUC,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnpAUC<-data.frame(TOST=c("T_lower","T_upper"),
                 T_value=c(formatC(TL_lnpAUC,format="f",digits=3),formatC(TU_lnpAUC,format="f",digits=3)),
                 P_value=c(formatC(PTL_lnpAUC,format="f",digits=3),formatC(PTU_lnpAUC,format="f",digits=3))) 
colnames(TOST_lnpAUC)<- c("TOST","   T value","  P value")
show(TOST_lnpAUC) 
cat("\n")
if(PTL_lnpAUC >= 0.05 || PTU_lnpAUC >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnpAUC,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnpAUC >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("---------------------------------------------------------------------------\n\n")
cat("             *** Intra-subject and Inter-subject Residuals ***            \n")
cat("--------------------------------------------------------------------------\n")
II_lnpAUC<-data.frame(subj=IntraInterlnpAUC00$subj,
                      Obs=formatC(IntraInterlnpAUC00$Obs,format="f",digits=6),
                      Exp=formatC(IntraInterlnpAUC00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnpAUC00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnpAUC00$Stud_Intra,format="f",digits=6),
                      Inter=formatC(IntraInterlnpAUC00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnpAUC00$Stud_Inter,format="f",digits=6))   
show(II_lnpAUC)
cat("--------------------------------------------------------------------------\n")
cat("Obs: Observed lnpAUC\n")
cat("Exp: Expected lnpAUC\n")
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")
}
##### end of lnpAUC stat.
if(multiple){
}
else{
#Report_AUC0INF.txt
#GLM_lnAUC0INF.txt
#intra- and inter- subject variability
lnAUC0INF_inter<-(lnAUC0INF_MSinter - lnAUC0INF_MSintra)/2
lnAUC0INF_intraclass<- (lnAUC0INF_MSinter - lnAUC0INF_MSintra)/(lnAUC0INF_MSinter + lnAUC0INF_MSintra)

lnAUC0INF_Fv<-lnAUC0INF_MSinter/lnAUC0INF_MSintra

#95% CI
lnAUC0INF_Le<-lnAUC0INF_SSintra/chinv_2
lnAUC0INF_Ue<-lnAUC0INF_SSintra/chinv_1

lnAUC0INF_Lp<-(lnAUC0INF_Fv/F_2-1)/(lnAUC0INF_Fv/F_2+1)
lnAUC0INF_Up<-(lnAUC0INF_Fv/F_1-1)/(lnAUC0INF_Fv/F_1+1)

lnAUC0INF_Ls<-(lnAUC0INF_SSinter*(1-F_2/lnAUC0INF_Fv))/(2*chinv_2)
lnAUC0INF_Us<-(lnAUC0INF_SSinter*(1-F_1/lnAUC0INF_Fv))/(2*chinv_1)

lnAUC0INF_PFv<-pf(1/lnAUC0INF_Fv,L1+L2-2,L1+L2-2)

#############################
lnAUC0INF_Total<-Prddata1$lnAUC0INF + Prddata2$lnAUC0INF
lnAUC0INF_Totalmean<-mean(lnAUC0INF_Total)


lnAUC0INF_CD1<-seqdata2[[1]]$lnAUC0INF - seqdata1[[1]]$lnAUC0INF
pre_CD1<-data.frame(subj=seqdata2[[1]]$subj,lnAUC0INF_CD=lnAUC0INF_CD1)
lnAUC0INF_CD2<-seqdata1[[2]]$lnAUC0INF - seqdata2[[2]]$lnAUC0INF
pre_CD2<-data.frame(subj=seqdata2[[2]]$subj,lnAUC0INF_CD=lnAUC0INF_CD2)
prelnAUC0INF_CD<-rbind(pre_CD1,pre_CD2)
lnAUC0INF_CD<-prelnAUC0INF_CD[ do.call(order,prelnAUC0INF_CD) ,]
lnAUC0INF_CDmean<- mean(lnAUC0INF_CD$lnAUC0INF_CD)


lnAUC0INF_TotalData<-data.frame(subj=Prddata1$subj,Total=lnAUC0INF_Total,U=lnAUC0INF_Total-lnAUC0INF_Totalmean,U2=(lnAUC0INF_Total-lnAUC0INF_Totalmean)^2,
                             CD=lnAUC0INF_CD$lnAUC0INF_CD,V=lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean,V2=(lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean)^2,
                             UV=(lnAUC0INF_Total-lnAUC0INF_Totalmean)*(lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean))

lnAUC0INF_Svv<-sum(lnAUC0INF_TotalData$V2)*(1/(L1+L2-1))
lnAUC0INF_Suu<-sum(lnAUC0INF_TotalData$U2)*(1/(L1+L2-1))
lnAUC0INF_Svu<-sum(lnAUC0INF_TotalData$UV)*(1/(L1+L2-1))

lnAUC0INF_Srr<-sum((RefData$lnAUC0INF - ref_AUC0INF)^2 )*(1/(L1+L2-1))
lnAUC0INF_Stt<-sum((TestData$lnAUC0INF - test_AUC0INF)^2 )*(1/(L1+L2-1))
lnAUC0INF_Srt<-sum((RefData$lnAUC0INF - ref_AUC0INF)*(TestData$lnAUC0INF - test_AUC0INF))*(1/(L1+L2-1))

lnAUC0INF_pearson_P<-cor.test(lnAUC0INF_TotalData$V,lnAUC0INF_TotalData$U,method=c("pearson"))[[3]]
lnAUC0INF_pearson_V<-cor.test(lnAUC0INF_TotalData$V,lnAUC0INF_TotalData$U,method=c("pearson"))[[4]][[1]]
lnAUC0INF_spearman_P<-cor.test(lnAUC0INF_TotalData$V,lnAUC0INF_TotalData$U,method=c("spearman"))[[3]]
lnAUC0INF_spearman_V<-cor.test(lnAUC0INF_TotalData$V,lnAUC0INF_TotalData$U,method=c("spearman"))[[4]][[1]]

lnAUC0INF_Frt<-lnAUC0INF_Stt/lnAUC0INF_Srr
lnAUC0INF_rrt<-lnAUC0INF_Srt/sqrt(lnAUC0INF_Srr*lnAUC0INF_Stt)

lnAUC0INF_Fpm<-((L1+L2-2)*(lnAUC0INF_Frt-1)^2 )/(4*(lnAUC0INF_Frt)*(1-(lnAUC0INF_rrt)^2))
lnAUC0INF_PFpm<-1-pf(lnAUC0INF_Fpm,1,L1+L2-2)

cat("            Pivotal Parameters of BE Study - Summary Report               \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: log(AUC0INF)                                        \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 ,"\n")
cat("        n2(T -> R) =",L2 ,"\n")
cat("          N(n1+n2) =",L1+L2 ,"\n")
cat("    Lower criteria =",formatC(lnAUC0INF_theta1*100,format="f",digits=3),"%\n")
cat("    Upper criteria =",formatC(lnAUC0INF_theta2*100,format="f",digits=3),"%\n")
cat("          MEAN-ref =",ref_AUC0INF,"\n")
cat("         MEAN-test =",test_AUC0INF,"\n")
cat("               MSE =",anova(lnAUC0INF)[5,3],"\n")
cat("                SE =",SE_AUC0INF,"\n")
cat("Estimate(test-ref) =",est_lnAUC0INF,"\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0INF **************\n")
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(est_lnAUC0INF),format="f",digits=3)),
                   CI90_lower=c(formatC(LowerAUC0INF,format="f",digits=3)),
                   CI90_upper=c(formatC(UpperAUC0INF,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnAUC0INF<-data.frame(TOST=c("T_lower","T_upper"),
                 T_value=c(formatC(TL_lnAUC0INF,format="f",digits=3),formatC(TU_lnAUC0INF,format="f",digits=3)),
                 P_value=c(formatC(PTL_lnAUC0INF,format="f",digits=3),formatC(PTU_lnAUC0INF,format="f",digits=3))) 
colnames(TOST_lnAUC0INF)<- c("TOST","   T value","  P value")
show(TOST_lnAUC0INF) 
cat("\n")
if(PTL_lnAUC0INF >= 0.05 || PTU_lnAUC0INF >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnAUC0INF,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnAUC0INF >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("---------------------------------------------------------------------------\n\n")
cat("             *** Intra-subject and Inter-subject Residuals ***            \n")
cat("--------------------------------------------------------------------------\n")
II_lnAUC0INF<-data.frame(subj=IntraInterlnAUC0INF00$subj,
                      Obs=formatC(IntraInterlnAUC0INF00$Obs,format="f",digits=6),
                      Exp=formatC(IntraInterlnAUC0INF00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnAUC0INF00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnAUC0INF00$Stud_Intra,format="f",digits=6),
                      Inter=formatC(IntraInterlnAUC0INF00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnAUC0INF00$Stud_Inter,format="f",digits=6))   
show(II_lnAUC0INF)
cat("--------------------------------------------------------------------------\n")
cat("Obs: Observed lnAUC0INF\n")
cat("Exp: Expected lnAUC0INF\n")
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("\n")
cat("---------------------------------------------------------------------------\n")
cat("Ref.:\n")
cat("1. Chow SC and Liu JP. Design and Analysis of Bioavailability-           \n")
cat("   Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).\n\n")
cat("2. Schuirmann DJ. On hypothesis testing to determine if the mean of a  \n")
cat("   normal distribution is continued in a known interval. Biometrics,37,\n")
cat("   617(1981).                                                           \n\n")
cat("3. Schuirmann DJ. A comparison of the two one-sided tests procedure and the \n")
cat("   power approach for assessing the equivalence of average bioavailability.\n")
cat("   Journal of Pharmacokinetics and Biopharmaceutics,15,657-680 (1987). \n\n")
cat("4. Anderson S and Hauck WW.  A new procedure for testing equivalence in \n")
cat("   comparative bioavailability and other clinical trials. Communications \n")
cat("   in Statistics-Theory and Methods,12,2663-2692 (1983).                \n")
cat("--------------------------------------------------------------------------\n")
cat("\n\n")
}
if(ODAnalysis){
cat("\n\n Generate ODA output now...\n");readline(" Press Enter to proceed...");cat("\n\n")
zzoda <- file(oda_output_xfile,open="wt")
sink(zzoda,split=TRUE)   ### for debugging ... -YJ
description_version()
cat("\n\n")
cat("****************************************************************************\n")
cat("                    Analysis of Outlier Detection (ODA)\n")
cat("****************************************************************************\n\n")
cat(" Test for Normality Assumption (Shapiro-Wilk)  \n")
cat("-----------------------------------------------\n")
if(multiple){
if(pAUC){
outputSW<-data.frame(Parameter=c("lnCmax_ss_Stud_Intra","lnCmax_ss_Stud_Inter",
                              "lnAUC(tau)ss_Stud_Intra","lnAUC(tau)ss_Stud_Inter",
                              "lnpAUC_Stud_Intra","lnpAUC_Stud_Inter"),
                     Test=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnpAUC00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnpAUC00$Stud_Inter)[[1]][[1]],format="f",digits=5)),
                     P_value=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnpAUC00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnpAUC00$Stud_Inter)[[2]],format="f",digits=3)))
}
else{
outputSW<-data.frame(Parameter=c("lnCmax_ss_Stud_Intra","lnCmax_ss_Stud_Inter",
                              "lnAUC(tau)ss_Stud_Intra","lnAUC(tau)ss_Stud_Inter"),
                     Test=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[1]][[1]],format="f",digits=5)),
                     P_value=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[2]],format="f",digits=3)))
 }
}
else{
if(pAUC){
outputSW<-data.frame(Parameter=c("lnCmax_Stud_Intra","lnCmax_Stud_Inter",
                                 "lnAUC0t_Stud_Intra","lnAUC0t_Stud_Inter",
                                 "lnAUC0INF_Stud_Intra","lnAUC0INF_Stud_Inter",
                                 "lnpAUC_Stud_Intra","lnpAUC_Stud_Inter"),
                     Test=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnpAUC00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnpAUC00$Stud_Inter)[[1]][[1]],format="f",digits=5)),
                     P_value=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnpAUC00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnpAUC00$Stud_Inter)[[2]],format="f",digits=3)))
}
else{
outputSW<-data.frame(Parameter=c("lnCmax_Stud_Intra","lnCmax_Stud_Inter",
                                 "lnAUC0t_Stud_Intra","lnAUC0t_Stud_Inter",
                                 "lnAUC0INF_Stud_Intra","lnAUC0INF_Stud_Inter"),
                     Test=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[1]][[1]],format="f",digits=5)),
                     P_value=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[2]],format="f",digits=3),
                               formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[2]],format="f",digits=3),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[2]],format="f",digits=3)))
}
}
colnames(outputSW)<- c("Parameter","      Test"," P value")
show(outputSW)
cat("\n")
cat("-------------------------------------------------\n")
cat(" Stud_Intra: studentized intra-subject residuals\n")
cat(" Stud_Inter: studentized inter-subject residuals\n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat("  The normality of the studentized intra-subject residuals and the \n")
cat(" studentized inter-residuals was examined using the test of Shapiro-Wilk. \n")
cat(" If a P value is more than 0.05,we will fail to reject the normal         \n")
cat(" assumption hypothesis.                                                    \n")
cat("\n")
cat("**Ref: Chow SC and Liu JP. Design and Analysis of Bioavailability-      \n")
cat(" Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).\n")
cat("---------------------------------------------------------------------\n")
cat("\n")
cat(" Test for Normality Assumption (Pearson)  \n")
cat("---------------------------------------------\n")
if(multiple){
if(pAUC){
outputPearson<-data.frame(Parameter=c("lnCmax_ss","lnAUC(tau)ss","lnpAUC"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3)))
}
else{
outputPearson<-data.frame(Parameter=c("lnCmax_ss","lnAUC(tau)ss"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3)))
}
}
else{
if(pAUC){
outputPearson<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF","lnpAUC"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3)))
}
else{
outputPearson<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("pearson"))[[3]],format="f",digits=3)))
}
}
colnames(outputPearson)<- c("Parameter","      Test"," P value")
show(outputPearson)
cat("\n")
cat("-------------------------------------------------\n")
cat(" Pearson: Pearson's correlation coefficient\n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat(" Either Pearson correlation coefficient or Spearman's rank correlation  \n")
cat(" coefficient is used to examine the assumption of independence between   \n")
cat(" intra- and inter-subject variabilities.  Thus,if a P value is more than \n")
cat(" 0.05,there is no evidence to suggest that the assumption is not true.   \n")
cat("\n")
cat("**Ref: Chow SC and Liu JP. Design and Analysis of Bioavailability-          \n")
cat(" Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).     \n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat(" Test for Normality Assumption (Spearman)    \n")
cat("----------------------------------------------\n")
if(multiple){
if(pAUC){
outputSpearman<-data.frame(Parameter=c("lnCmax_ss","lnAUC(tau)ss","lnpAUC"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3)))
}
else{
outputSpearman<-data.frame(Parameter=c("lnCmax_ss","lnAUC(tau)ss"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3)))
 }
}
else{
if(pAUC){
outputSpearman<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF","lnpAUC"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnpAUC00$Stud_Intra,IntraInterlnpAUC00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3)))
}
else{
outputSpearman<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra,IntraInterlnCmax00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra,IntraInterlnAUC0t00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra,IntraInterlnAUC0INF00$Stud_Inter,method=c("spearman"))[[3]],format="f",digits=3)))
}
}
colnames(outputSpearman)<- c("Parameter","      Test"," P value")
show(outputSpearman)
cat("\n")
cat("--------------------------------------------------\n")
cat(" Spearman: Spearman's rank correlation coefficient\n")
cat("--------------------------------------------------\n")
cat("\n")

#######################################
##for Hotelling T^2
if(multiple){
if(pAUC){
HotelData<-data.frame(subj=RefData$subj,Ref_Cmax=RefData$Cmax,Test_Cmax=TestData$Cmax,
                               Ref_lnCmax=RefData$lnCmax,Test_lnCmax=TestData$lnCmax,
                               Ref_AUC0t=RefData$AUC0t,Test_AUC0t=TestData$AUC0t,
                               Ref_lnAUC0t=RefData$lnAUC0t,Test_lnAUC0t=TestData$lnAUC0t,
                               Ref_pAUC=RefData$partAUC,Test_pAUC=TestData$partAUC,
                               Ref_lnpAUC=RefData$lnpAUC,Test_lnpAUC=TestData$lnpAUC)
}
else{
HotelData<-data.frame(subj=RefData$subj,Ref_Cmax=RefData$Cmax,Test_Cmax=TestData$Cmax,
                               Ref_lnCmax=RefData$lnCmax,Test_lnCmax=TestData$lnCmax,
                               Ref_AUC0t=RefData$AUC0t,Test_AUC0t=TestData$AUC0t,
                               Ref_lnAUC0t=RefData$lnAUC0t,Test_lnAUC0t=TestData$lnAUC0t)
}

Hotel<-split(HotelData,list(HotelData$subj))

subj1<-NULL
HT_Cmax<-NULL
HT_lnCmax<-NULL
HT_AUC0t<-NULL
HT_lnAUC0t<-NULL
if(pAUC) {HT_pAUC<-NULL;HT_lnpAUC<-NULL}

HT_Cmax_P<-NULL
HT_lnCmax_P<-NULL
HT_AUC0t_P<-NULL
HT_lnAUC0t_P<-NULL
if(pAUC) {HT_pAUC_P<-NULL;HT_lnpAUC_P<-NULL}

for(i in seq_along(Hotel)){
 subj<-Hotel[[i]]$subj
 Ref_Cmax<-Hotel[[i]]$Ref_Cmax
 Test_Cmax<-Hotel[[i]]$Test_Cmax
 Ref_lnCmax<-Hotel[[i]]$Ref_lnCmax
 Test_lnCmax<-Hotel[[i]]$Test_lnCmax
 
 Ref_AUC0t<-Hotel[[i]]$Ref_AUC0t
 Test_AUC0t<-Hotel[[i]]$Test_AUC0t
 Ref_lnAUC0t<-Hotel[[i]]$Ref_lnAUC0t
 Test_lnAUC0t<-Hotel[[i]]$Test_lnAUC0t
 
 if(pAUC){
 Ref_pAUC<-Hotel[[i]]$Ref_pAUC
 Test_pAUC<-Hotel[[i]]$Test_pAUC
 Ref_lnpAUC<-Hotel[[i]]$Ref_lnpAUC
 Test_lnpAUC<-Hotel[[i]]$Test_lnpAUC
 }
 
 if(pAUC){
    com<-data.frame(subj=subj,Ref_Cmax=Ref_Cmax,Test_Cmax=Test_Cmax,
                    Ref_lnCmax=Ref_lnCmax,Test_lnCmax=Test_lnCmax,
                    Ref_AUC0t=Ref_AUC0t,Test_AUC0t=Test_AUC0t,
                    Ref_lnAUC0t=Ref_lnAUC0t,Test_lnAUC0t=Test_lnAUC0t,
                    Ref_pAUC=Ref_pAUC,Test_pAUC=Test_pAUC,
                    Ref_lnpAUC=Ref_lnpAUC,Test_lnpAUC=Test_lnpAUC)
 }
 else{
    com<-data.frame(subj=subj,Ref_Cmax=Ref_Cmax,Test_Cmax=Test_Cmax,
                   Ref_lnCmax=Ref_lnCmax,Test_lnCmax=Test_lnCmax,
                   Ref_AUC0t=Ref_AUC0t,Test_AUC0t=Test_AUC0t,
                   Ref_lnAUC0t=Ref_lnAUC0t,Test_lnAUC0t=Test_lnAUC0t)
 }

    del<-subset(as.data.frame(HotelData),subj != Hotel[[i]]$subj)
    
    com_Cmax<-data.frame(Ref_Cmax=com$Ref_Cmax,Test_Cmax=com$Test_Cmax) 
    del_Cmax<-data.frame(Ref_Cmax=del$Ref_Cmax,Test_Cmax=del$Test_Cmax) 
     HT_Cmax[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax,test="chi")[[1]][[1]])
     HT_Cmax_P[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax,test="chi")[[2]][[1]])
 
    com_lnCmax<-data.frame(Ref_lnCmax=com$Ref_lnCmax,Test_lnCmax=com$Test_lnCmax) 
    del_lnCmax<-data.frame(Ref_lnCmax=del$Ref_lnCmax,Test_lnCmax=del$Test_lnCmax) 
     HT_lnCmax[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax,test="chi")[[1]][[1]])
     HT_lnCmax_P[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax,test="chi")[[2]][[1]])
    
    com_AUC0t<-data.frame(Ref_AUC0t=com$Ref_AUC0t,Test_AUC0t=com$Test_AUC0t)
    del_AUC0t<-data.frame(Ref_AUC0t=del$Ref_AUC0t,Test_AUC0t=del$Test_AUC0t)
     HT_AUC0t[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t,test="chi")[[1]][[1]])                 
     HT_AUC0t_P[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t,test="chi")[[2]][[1]])    
     
    com_lnAUC0t<-data.frame(Ref_lnAUC0t=com$Ref_lnAUC0t,Test_lnAUC0t=com$Test_lnAUC0t)
    del_lnAUC0t<-data.frame(Ref_lnAUC0t=del$Ref_lnAUC0t,Test_lnAUC0t=del$Test_lnAUC0t)
      HT_lnAUC0t[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t,test="chi")[[1]][[1]])  
      HT_lnAUC0t_P[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t,test="chi")[[2]][[1]])
      
    if(pAUC){
     com_pAUC<-data.frame(Ref_pAUC=com$Ref_pAUC,Test_pAUC=com$Test_pAUC)
     del_pAUC<-data.frame(Ref_pAUC=del$Ref_pAUC,Test_pAUC=del$Test_pAUC)
      HT_pAUC[[i]]<-c(HotellingsT2(com_pAUC,del_pAUC,test="chi")[[1]][[1]])                 
      HT_pAUC_P[[i]]<-c(HotellingsT2(com_pAUC,del_pAUC,test="chi")[[2]][[1]]) 

     com_lnpAUC<-data.frame(Ref_lnpAUC=com$Ref_lnpAUC,Test_lnpAUC=com$Test_lnpAUC)
     del_lnpAUC<-data.frame(Ref_lnpAUC=del$Ref_lnpAUC,Test_lnpAUC=del$Test_lnpAUC)
      HT_lnpAUC[[i]]<-c(HotellingsT2(com_lnpAUC,del_lnpAUC,test="chi")[[1]][[1]])                 
      HT_lnpAUC_P[[i]]<-c(HotellingsT2(com_lnpAUC,del_lnpAUC,test="chi")[[2]][[1]])    
     }   
 }
# show(subj1)
subj1<-c(levels(Hotel[[1]]$subj))  #c((Hotel[[1]]$subj))->c(levels(Hotel[[1]]$subj))
cat(" Hotelling T^2 with Chi-square Test\n")
cat("---------------------------------------------\n")
HotellingCmax<-data.frame(subj1,formatC(HT_Cmax,format="f",digits=5),   
                          formatC(HT_Cmax_P,format="f",digits=5),
                          formatC(HT_lnCmax,format="f",digits=5),
                          formatC(HT_lnCmax_P,format="f",digits=5))                                   
colnames(HotellingCmax)<- c("Subj","Cmax_ss","P value","lnCmax_ss","P value")                                  
                           
show(HotellingCmax) 
cat("\n")

HotellingAUC0t<-data.frame(subj1,formatC(HT_AUC0t,format="f",digits=5),
                           formatC(HT_AUC0t_P,format="f",digits=5),    
                           formatC(HT_lnAUC0t,format="f",digits=5),
                           formatC(HT_lnAUC0t_P,format="f",digits=5))
colnames(HotellingAUC0t)<- c("Subj","AUC(tau)ss","P value","lnAUC(tau)ss","P value")        
show(HotellingAUC0t) 
cat("\n")

if(pAUC){
HotellingpAUC<-data.frame(subj1,formatC(HT_pAUC,format="f",digits=5),
                           formatC(HT_pAUC_P,format="f",digits=5),    
                           formatC(HT_lnpAUC,format="f",digits=5),
                           formatC(HT_lnpAUC_P,format="f",digits=5))
colnames(HotellingpAUC)<- c("Subj","pAUC","P value","lnpAUC","P value")        
show(HotellingpAUC) 
cat("\n")
}

}
else{                 ### for single-dose now
if(pAUC){
HotelData<-data.frame(subj=RefData$subj,Ref_Cmax=RefData$Cmax,Test_Cmax=TestData$Cmax,
                      Ref_lnCmax=RefData$lnCmax,Test_lnCmax=TestData$lnCmax,
                      Ref_AUC0t=RefData$AUC0t,Test_AUC0t=TestData$AUC0t,
                      Ref_lnAUC0t=RefData$lnAUC0t,Test_lnAUC0t=TestData$lnAUC0t,
                      Ref_pAUC=RefData$partAUC,Test_pAUC=TestData$partAUC,
                      Ref_lnpAUC=RefData$lnpAUC,Test_lnpAUC=TestData$lnpAUC,
                      Ref_AUC0INF=RefData$AUC0INF,Test_AUC0INF=TestData$AUC0INF,
                      Ref_lnAUC0INF=RefData$lnAUC0INF,Test_lnAUC0INF=TestData$lnAUC0INF)
}
else{
HotelData<-data.frame(subj=RefData$subj,Ref_Cmax=RefData$Cmax,Test_Cmax=TestData$Cmax,
                      Ref_lnCmax=RefData$lnCmax,Test_lnCmax=TestData$lnCmax,
                      Ref_AUC0t=RefData$AUC0t,Test_AUC0t=TestData$AUC0t,
                      Ref_lnAUC0t=RefData$lnAUC0t,Test_lnAUC0t=TestData$lnAUC0t,
                      Ref_AUC0INF=RefData$AUC0INF,Test_AUC0INF=TestData$AUC0INF,
                      Ref_lnAUC0INF=RefData$lnAUC0INF,Test_lnAUC0INF=TestData$lnAUC0INF)
}

Hotel<-split(HotelData,list(HotelData$subj))

subj1<-NULL
HT_Cmax<-NULL
HT_lnCmax<-NULL
HT_AUC0t<-NULL
HT_lnAUC0t<-NULL
if(pAUC) {HT_pAUC<-NULL;HT_lnpAUC<-NULL}
HT_AUC0INF<-NULL
HT_lnAUC0INF<-NULL
HT_Cmax_P<-NULL
HT_lnCmax_P<-NULL
HT_AUC0t_P<-NULL
HT_lnAUC0t_P<-NULL
if(pAUC) {HT_pAUC_P<-NULL;HT_lnpAUC_P<-NULL}
HT_AUC0INF_P<-NULL
HT_lnAUC0INF_P<-NULL

for(i in seq_along(Hotel)){
 subj<-Hotel[[i]]$subj
 Ref_Cmax<-Hotel[[i]]$Ref_Cmax
 Test_Cmax<-Hotel[[i]]$Test_Cmax
 Ref_lnCmax<-Hotel[[i]]$Ref_lnCmax
 Test_lnCmax<-Hotel[[i]]$Test_lnCmax
 
 Ref_AUC0t<-Hotel[[i]]$Ref_AUC0t
 Test_AUC0t<-Hotel[[i]]$Test_AUC0t
 Ref_lnAUC0t<-Hotel[[i]]$Ref_lnAUC0t
 Test_lnAUC0t<-Hotel[[i]]$Test_lnAUC0t
 
 if(pAUC){
 Ref_pAUC<-Hotel[[i]]$Ref_pAUC
 Test_pAUC<-Hotel[[i]]$Test_pAUC
 Ref_lnpAUC<-Hotel[[i]]$Ref_lnpAUC
 Test_lnpAUC<-Hotel[[i]]$Test_lnpAUC
 }
 
 Ref_AUC0INF<-Hotel[[i]]$Ref_AUC0INF
 Test_AUC0INF<-Hotel[[i]]$Test_AUC0INF
 Ref_lnAUC0INF<-Hotel[[i]]$Ref_lnAUC0INF
 Test_lnAUC0INF<-Hotel[[i]]$Test_lnAUC0INF

if(pAUC){
    com<-data.frame(subj=subj,Ref_Cmax=Ref_Cmax,Test_Cmax=Test_Cmax,
                    Ref_lnCmax=Ref_lnCmax,Test_lnCmax=Test_lnCmax,
                    Ref_AUC0t=Ref_AUC0t,Test_AUC0t=Test_AUC0t,
                    Ref_lnAUC0t=Ref_lnAUC0t,Test_lnAUC0t=Test_lnAUC0t,
                    Ref_pAUC=Ref_pAUC,Test_pAUC=Test_pAUC,
                    Ref_lnpAUC=Ref_lnpAUC,Test_lnpAUC=Test_lnpAUC,
                    Ref_AUC0INF=Ref_AUC0INF,Test_AUC0INF=Test_AUC0INF,
                    Ref_lnAUC0INF=Ref_lnAUC0INF,Test_lnAUC0INF=Test_lnAUC0INF)
 }
 else{
    com<-data.frame(subj=subj,Ref_Cmax=Ref_Cmax,Test_Cmax=Test_Cmax,
                    Ref_lnCmax=Ref_lnCmax,Test_lnCmax=Test_lnCmax,
                    Ref_AUC0t=Ref_AUC0t,Test_AUC0t=Test_AUC0t,
                    Ref_lnAUC0t=Ref_lnAUC0t,Test_lnAUC0t=Test_lnAUC0t,
                    Ref_AUC0INF=Ref_AUC0INF,Test_AUC0INF=Test_AUC0INF,
                    Ref_lnAUC0INF=Ref_lnAUC0INF,Test_lnAUC0INF=Test_lnAUC0INF)
}

    del<-subset(as.data.frame(HotelData),subj != Hotel[[i]]$subj)
    
    com_Cmax<-data.frame(Ref_Cmax=com$Ref_Cmax,Test_Cmax=com$Test_Cmax) 
    del_Cmax<-data.frame(Ref_Cmax=del$Ref_Cmax,Test_Cmax=del$Test_Cmax) 
     HT_Cmax[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax,test="chi")[[1]][[1]])
     HT_Cmax_P[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax,test="chi")[[2]][[1]])
 
    com_lnCmax<-data.frame(Ref_lnCmax=com$Ref_lnCmax,Test_lnCmax=com$Test_lnCmax) 
    del_lnCmax<-data.frame(Ref_lnCmax=del$Ref_lnCmax,Test_lnCmax=del$Test_lnCmax) 
     HT_lnCmax[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax,test="chi")[[1]][[1]])
     HT_lnCmax_P[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax,test="chi")[[2]][[1]])
    
    com_AUC0t<-data.frame(Ref_AUC0t=com$Ref_AUC0t,Test_AUC0t=com$Test_AUC0t)
    del_AUC0t<-data.frame(Ref_AUC0t=del$Ref_AUC0t,Test_AUC0t=del$Test_AUC0t)
     HT_AUC0t[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t,test="chi")[[1]][[1]])                 
     HT_AUC0t_P[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t,test="chi")[[2]][[1]])
                     
    com_lnAUC0t<-data.frame(Ref_lnAUC0t=com$Ref_lnAUC0t,Test_lnAUC0t=com$Test_lnAUC0t)
    del_lnAUC0t<-data.frame(Ref_lnAUC0t=del$Ref_lnAUC0t,Test_lnAUC0t=del$Test_lnAUC0t)
      HT_lnAUC0t[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t,test="chi")[[1]][[1]])  
      HT_lnAUC0t_P[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t,test="chi")[[2]][[1]])      
     
   if(pAUC){
     com_pAUC<-data.frame(Ref_pAUC=com$Ref_pAUC,Test_pAUC=com$Test_pAUC)
     del_pAUC<-data.frame(Ref_pAUC=del$Ref_pAUC,Test_pAUC=del$Test_pAUC)
      HT_pAUC[[i]]<-c(HotellingsT2(com_pAUC,del_pAUC,test="chi")[[1]][[1]])                 
      HT_pAUC_P[[i]]<-c(HotellingsT2(com_pAUC,del_pAUC,test="chi")[[2]][[1]])    

     com_lnpAUC<-data.frame(Ref_lnpAUC=com$Ref_lnpAUC,Test_lnpAUC=com$Test_lnpAUC)
     del_lnpAUC<-data.frame(Ref_lnpAUC=del$Ref_lnpAUC,Test_lnpAUC=del$Test_lnpAUC)
      HT_lnpAUC[[i]]<-c(HotellingsT2(com_lnpAUC,del_lnpAUC,test="chi")[[1]][[1]])                 
      HT_lnpAUC_P[[i]]<-c(HotellingsT2(com_lnpAUC,del_lnpAUC,test="chi")[[2]][[1]])      
     }
      
    com_AUC0INF<-data.frame(Ref_AUC0INF=com$Ref_AUC0INF,Test_AUC0INF=com$Test_AUC0INF)
    del_AUC0INF<-data.frame(Ref_AUC0INF=del$Ref_AUC0INF,Test_AUC0INF=del$Test_AUC0INF)
       HT_AUC0INF[[i]]<-c(HotellingsT2(com_AUC0INF,del_AUC0INF,test="chi")[[1]][[1]]) 
       HT_AUC0INF_P[[i]]<-c(HotellingsT2(com_AUC0INF,del_AUC0INF,test="chi")[[2]][[1]]) 
       
    com_lnAUC0INF<-data.frame(Ref_lnAUC0INF=com$Ref_lnAUC0INF,Test_lnAUC0INF=com$Test_lnAUC0INF)
    del_lnAUC0INF<-data.frame(Ref_lnAUC0INF=del$Ref_lnAUC0INF,Test_lnAUC0INF=del$Test_lnAUC0INF)
       HT_lnAUC0INF[[i]]<-c(HotellingsT2(com_lnAUC0INF,del_lnAUC0INF,test="chi")[[1]][[1]]) 
       HT_lnAUC0INF_P[[i]]<-c(HotellingsT2(com_lnAUC0INF,del_lnAUC0INF,test="chi")[[2]][[1]]) 
       #subj1[[i]]<-c(levels(Hotel[[1]]$subj))
   
 }
# show( subj1)
subj1<-c(levels(Hotel[[1]]$subj))  #c((Hotel[[1]]$subj))->c(levels(Hotel[[1]]$subj))
cat(" Hotelling T^2 with Chi-square Test  \n")
cat("---------------------------------------------\n")
HotellingCmax<-data.frame(subj1,formatC(HT_Cmax,format="f",digits=5),   
                          formatC(HT_Cmax_P,format="f",digits=5),
                          formatC(HT_lnCmax,format="f",digits=5),
                          formatC(HT_lnCmax_P,format="f",digits=5))                                   
colnames(HotellingCmax)<- c("Subj","Cmax","P value","lnCmax","P value")                                  
                           
show(HotellingCmax) 
cat("\n")

HotellingAUC0t<-data.frame(subj1,formatC(HT_AUC0t,format="f",digits=5),
                           formatC(HT_AUC0t_P,format="f",digits=5),    
                           formatC(HT_lnAUC0t,format="f",digits=5),
                           formatC(HT_lnAUC0t_P,format="f",digits=5))
colnames(HotellingAUC0t)<- c("Subj","AUC0t","P value","lnAUC0t","P value")        
show(HotellingAUC0t) 
cat("\n")

if(pAUC){
HotellingpAUC<-data.frame(subj1,formatC(HT_pAUC,format="f",digits=5),
                           formatC(HT_pAUC_P,format="f",digits=5),    
                           formatC(HT_lnpAUC,format="f",digits=5),
                           formatC(HT_lnpAUC_P,format="f",digits=5))
colnames(HotellingpAUC)<- c("Subj","pAUC","P value","lnpAUC","P value")        
show(HotellingpAUC) 
cat("\n")
}

HotellingAUC0INF<-data.frame(subj1,formatC(HT_AUC0INF,format="f",digits=5),
                              formatC(HT_AUC0INF_P,format="f",digits=5),
                              formatC(HT_lnAUC0INF,format="f",digits=5),
                              formatC(HT_lnAUC0INF_P,format="f",digits=5)) 
colnames(HotellingAUC0INF)<- c("Subj","AUC0INF","P value","lnAUC0INF","P value")   
show(HotellingAUC0INF) 
}
cat("\n")
cat("---------------------------------------------\n")
cat("**Interpretation: If subjects have relatively BIG T^2 values which \n")
cat("  cause P value less than 0.05,these subject may be outlying subjects. \n")
cat("\n")
cat("Ref.:\n") 
cat(" 1. Liu JP and Weng CS. Detection of outlying data in bioavailability-\n")
cat("    bioequivalence studies. Statistics in Medicine,10,1375-1389(1991).\n\n")
cat(" 2. Chow SC and Liu JP. Design and Analysis of Bioavailability-        \n")
cat("    Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).\n")
cat("-------------------------------------------------------------------------\n")
cat("\n") 

##summary of point and interval estimation of inter- and intra-subject variability for data
cat(" Test for Equality of Intra-subject Variabilities between Formulations   \n")
cat("--------------------------------------------------------------------------\n")
if(multiple){
if(pAUC){
 outputEquality<-data.frame(Method=c("lnCmax_ss_Pearson","lnCmax_ss_Pitman_Morgan","lnCmax_ss_Spearman",
                                     "lnAUC(tau)ss_Pearson","lnAUC(tau)ss_Pitman_Morgan","lnAUC(tau)ss_Spearman",
                                     "lnpAUC_Pearson","lnpAUC_Pitman_Morgan","lnpAUC_Spearman"),
                           Test=c(formatC(lnCmax_pearson_V,format="f",digits=5),formatC(lnCmax_Fpm,format="f",digits=5),formatC(lnCmax_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0t_pearson_V,format="f",digits=5),formatC(lnAUC0t_Fpm,format="f",digits=5),formatC(lnAUC0t_spearman_V,format="f",digits=5),
                                  formatC(lnpAUC_pearson_V,format="f",digits=5),formatC(lnpAUC_Fpm,format="f",digits=5),formatC(lnpAUC_spearman_V,format="f",digits=5)),              
                           P_value=c(formatC(lnCmax_pearson_P,format="f",digits=3),formatC(lnCmax_PFpm,format="f",digits=3),formatC(lnCmax_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0t_pearson_P,format="f",digits=3),formatC(lnAUC0t_PFpm,format="f",digits=3),formatC(lnAUC0t_spearman_P,format="f",digits=3),
                                     formatC(lnpAUC_pearson_P,format="f",digits=3),formatC(lnpAUC_PFpm,format="f",digits=3),formatC(lnpAUC_spearman_P,format="f",digits=3)))
}
else{
 outputEquality<-data.frame(Method=c("lnCmax_ss_Pearson","lnCmax_ss_Pitman_Morgan","lnCmax_ss_Spearman",
                                       "lnAUC(tau)ss_Pearson","lnAUC(tau)ss_Pitman_Morgan","lnAUC(tau)ss_Spearman"),
                           Test=c(formatC(lnCmax_pearson_V,format="f",digits=5),formatC(lnCmax_Fpm,format="f",digits=5),formatC(lnCmax_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0t_pearson_V,format="f",digits=5),formatC(lnAUC0t_Fpm,format="f",digits=5),formatC(lnAUC0t_spearman_V,format="f",digits=5)),              
                           P_value=c(formatC(lnCmax_pearson_P,format="f",digits=3),formatC(lnCmax_PFpm,format="f",digits=3),formatC(lnCmax_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0t_pearson_P,format="f",digits=3),formatC(lnAUC0t_PFpm,format="f",digits=3),formatC(lnAUC0t_spearman_P,format="f",digits=3)))
 }
}                                     
else{               ### for single-dose study
if(pAUC){
 outputEquality<-data.frame(Method=c("lnCmax_Pearson","lnCmax_Pitman_Morgan","lnCmax_Spearman",
                                     "lnAUC0t_Pearson","lnAUC0t_Pitman_Morgan","lnAUC0t_Spearman",
                                     "lnpAUC_Pearson","lnpAUC_Pitman_Morgan","lnpAUC_Spearman",
                                     "lnAUC0INF_Pearson","lnAUC0INF_Pitman_Morgan","lnAUC0INF_Spearman"),
                           Test=c(formatC(lnCmax_pearson_V,format="f",digits=5),formatC(lnCmax_Fpm,format="f",digits=5),formatC(lnCmax_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0t_pearson_V,format="f",digits=5),formatC(lnAUC0t_Fpm,format="f",digits=5),formatC(lnAUC0t_spearman_V,format="f",digits=5),
                                  formatC(lnpAUC_pearson_V,format="f",digits=5),formatC(lnpAUC_Fpm,format="f",digits=5),formatC(lnpAUC_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0INF_pearson_V,format="f",digits=5),formatC(lnAUC0INF_Fpm,format="f",digits=5),formatC(lnAUC0INF_spearman_V,format="f",digits=5)),           
                           P_value=c(formatC(lnCmax_pearson_P,format="f",digits=3),formatC(lnCmax_PFpm,format="f",digits=3),formatC(lnCmax_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0t_pearson_P,format="f",digits=3),formatC(lnAUC0t_PFpm,format="f",digits=3),formatC(lnAUC0t_spearman_P,format="f",digits=3),
                                     formatC(lnpAUC_pearson_P,format="f",digits=3),formatC(lnpAUC_PFpm,format="f",digits=3),formatC(lnpAUC_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0INF_pearson_P,format="f",digits=3),formatC(lnAUC0INF_PFpm,format="f",digits=3),formatC(lnAUC0INF_spearman_P,format="f",digits=3)))
}
else{
 outputEquality<-data.frame(Method=c("lnCmax_Pearson","lnCmax_Pitman_Morgan","lnCmax_Spearman",
                                       "lnAUC0t_Pearson","lnAUC0t_Pitman_Morgan","lnAUC0t_Spearman",
                                       "lnAUC0INF_Pearson","lnAUC0INF_Pitman_Morgan","lnAUC0INF_Spearman"),
                           Test=c(formatC(lnCmax_pearson_V,format="f",digits=5),formatC(lnCmax_Fpm,format="f",digits=5),formatC(lnCmax_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0t_pearson_V,format="f",digits=5),formatC(lnAUC0t_Fpm,format="f",digits=5),formatC(lnAUC0t_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0INF_pearson_V,format="f",digits=5),formatC(lnAUC0INF_Fpm,format="f",digits=5),formatC(lnAUC0INF_spearman_V,format="f",digits=5)),           
                           P_value=c(formatC(lnCmax_pearson_P,format="f",digits=3),formatC(lnCmax_PFpm,format="f",digits=3),formatC(lnCmax_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0t_pearson_P,format="f",digits=3),formatC(lnAUC0t_PFpm,format="f",digits=3),formatC(lnAUC0t_spearman_P,format="f",digits=3),
                                     formatC(lnAUC0INF_pearson_P,format="f",digits=3),formatC(lnAUC0INF_PFpm,format="f",digits=3),formatC(lnAUC0INF_spearman_P,format="f",digits=3)))
}                   
}
colnames(outputEquality)<- c("Parameter","      Test"," P value")
show(outputEquality) 
cat("\n")
cat("--------------------------------------------------------------------------\n")
cat(" **Interpretation:\n")
cat("   The standard 2*2*2 crossover design was assumed that intra-subject      \n")
cat(" variabilities for the Test and the Reference formulations are the same.    \n")
cat(" Thus,if the intra-subject variabilities between formulations are different,\n")
cat(" equivalence in average bioavailabilities between formulations does not    \n")
cat(" imply that the two formulations are therapeutically equivalent and        \n")
cat(" interchangeable.                                                          \n")
cat(" We use both parametric (Pitman-Morgan's adjusted F test and Pearson \n")
cat(" correlation coefficient) and nonparametric test (Spearman's rank correlation\n")
cat(" coefficient) for testing equality of intra-subject variabilities between \n") 
cat(" formulations.  If a P value is less than 0.05,we may reject the null     \n")
cat(" hypothesis of equality in intra-subject variabilities between formulations.\n")
cat("\n")
cat("**Ref.:\n")
cat(" 1. Chow SC and Liu JP. Design and Analysis of Bioavailability-            \n")
cat("    Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).  \n")
cat(" 2. Haynes JD. Statistical simulation study of new proposed uniformity     \n\n")
cat("    requirements for bioequivalency studies. Journal of Pharmaceutical     \n")
cat("    Sciences,70,673-675 (1981).                                          \n\n")
cat(" 3. McCulloch CE. Tests for equality of variances with paired data.        \n")
cat("    Communications in Statistics-Theory and Methods,16,1377-1391 (1987).  \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")

cat(" Point and Interval Estimation of Inter- and Intra-subject Variability  \n")
cat("--------------------------------------------------------------------------\n")
if(multiple){
if(pAUC){
outputPoint<-data.frame(Parameter=c("lnCmax_ss_intra","lnCmax_ss_inter","lnCmax_ss_intraclass","lnCmax_ss_prob",
                                    "lnAUC(tau)ss_intra","lnAUC(tau)ss_inter","lnAUC(tau)ss_intraclass","lnAUC(tau)ss_prob",
                                    "lnpAUC_intra","lnpAUC_inter","lnpAUC_intraclass","lnpAUC_prob"),
  Point_Estimate=c(formatC(lnCmax_MSintra,format="f",digits=3),formatC(lnCmax_inter,format="f",digits=3),formatC(lnCmax_intraclass,format="f",digits=3),formatC(lnCmax_PFv,format="f",digits=5),
                   formatC(lnAUC0t_MSintra,format="f",digits=3),formatC(lnAUC0t_inter,format="f",digits=3),formatC(lnAUC0t_intraclass,format="f",digits=3),formatC(lnAUC0t_PFv,format="f",digits=5),
                   formatC(lnpAUC_MSintra,format="f",digits=3),formatC(lnpAUC_inter,format="f",digits=3),formatC(lnpAUC_intraclass,format="f",digits=3),formatC(lnpAUC_PFv,format="f",digits=5)),              
  CI95_lower=c(formatC(lnCmax_Le,format="f",digits=3),formatC(lnCmax_Ls,format="f",digits=3),formatC(lnCmax_Lp,format="f",digits=3),"-",
               formatC(lnAUC0t_Le,format="f",digits=3),formatC(lnAUC0t_Ls,format="f",digits=3),formatC(lnAUC0t_Lp,format="f",digits=3),"-",
               formatC(lnpAUC_Le,format="f",digits=3),formatC(lnpAUC_Ls,format="f",digits=3),formatC(lnpAUC_Lp,format="f",digits=3),"-"),          
  CI95_upper=c(formatC(lnCmax_Ue,format="f",digits=3),formatC(lnCmax_Us,format="f",digits=3),formatC(lnCmax_Up,format="f",digits=3),"-",
               formatC(lnAUC0t_Ue,format="f",digits=3),formatC(lnAUC0t_Us,format="f",digits=3),formatC(lnAUC0t_Up,format="f",digits=3),"-",
               formatC(lnpAUC_Ue,format="f",digits=3),formatC(lnpAUC_Us,format="f",digits=3),formatC(lnpAUC_Up,format="f",digits=3),"-"))
}
else{
outputPoint<-data.frame(Parameter=c("lnCmax_ss_intra","lnCmax_ss_inter","lnCmax_ss_intraclass","lnCmax_ss_prob",
                                    "lnAUC(tau)ss_intra","lnAUC(tau)ss_inter","lnAUC(tau)ss_intraclass","lnAUC(tau)ss_prob"),
 Point_Estimate=c(formatC(lnCmax_MSintra,format="f",digits=3),formatC(lnCmax_inter,format="f",digits=3),formatC(lnCmax_intraclass,format="f",digits=3),formatC(lnCmax_PFv,format="f",digits=5),
                  formatC(lnAUC0t_MSintra,format="f",digits=3),formatC(lnAUC0t_inter,format="f",digits=3),formatC(lnAUC0t_intraclass,format="f",digits=3),formatC(lnAUC0t_PFv,format="f",digits=5)),              
 CI95_lower=c(formatC(lnCmax_Le,format="f",digits=3),formatC(lnCmax_Ls,format="f",digits=3),formatC(lnCmax_Lp,format="f",digits=3),"-",
              formatC(lnAUC0t_Le,format="f",digits=3),formatC(lnAUC0t_Ls,format="f",digits=3),formatC(lnAUC0t_Lp,format="f",digits=3),"-"),          
 CI95_upper=c(formatC(lnCmax_Ue,format="f",digits=3),formatC(lnCmax_Us,format="f",digits=3),formatC(lnCmax_Up,format="f",digits=3),"-",
              formatC(lnAUC0t_Ue,format="f",digits=3),formatC(lnAUC0t_Us,format="f",digits=3),formatC(lnAUC0t_Up,format="f",digits=3),"-"))
}
}
else{
if(pAUC){
outputPoint<-data.frame(Parameter=c("lnCmax_intra","lnCmax_inter","lnCmax_intraclass","lnCmax_prob",
                                    "lnAUC0t_intra","lnAUC0t_inter","lnAUC0t_intraclass","lnAUC0t_prob",
                                    "lnpAUC_intra","lnpAUC_inter","lnpAUC_intraclass","lnpAUC_prob",
                                    "lnAUC0INF_intra","lnAUC0INF_inter","lnAUC0INF_intraclass","lnAUC0INF_prob"),
 Point_Estimate=c(formatC(lnCmax_MSintra,format="f",digits=3),formatC(lnCmax_inter,format="f",digits=3),formatC(lnCmax_intraclass,format="f",digits=3),formatC(lnCmax_PFv,format="f",digits=5),
                  formatC(lnAUC0t_MSintra,format="f",digits=3),formatC(lnAUC0t_inter,format="f",digits=3),formatC(lnAUC0t_intraclass,format="f",digits=3),formatC(lnAUC0t_PFv,format="f",digits=5),
                  formatC(lnpAUC_MSintra,format="f",digits=3),formatC(lnpAUC_inter,format="f",digits=3),formatC(lnpAUC_intraclass,format="f",digits=3),formatC(lnpAUC_PFv,format="f",digits=5),
                  formatC(lnAUC0INF_MSintra,format="f",digits=3),formatC(lnAUC0INF_inter,format="f",digits=3),formatC(lnAUC0INF_intraclass,format="f",digits=3),formatC(lnAUC0INF_PFv,format="f",digits=5)),
 CI95_lower=c(formatC(lnCmax_Le,format="f",digits=3),formatC(lnCmax_Ls,format="f",digits=3),formatC(lnCmax_Lp,format="f",digits=3),"-",
              formatC(lnAUC0t_Le,format="f",digits=3),formatC(lnAUC0t_Ls,format="f",digits=3),formatC(lnAUC0t_Lp,format="f",digits=3),"-",
              formatC(lnpAUC_Le,format="f",digits=3),formatC(lnpAUC_Ls,format="f",digits=3),formatC(lnpAUC_Lp,format="f",digits=3),"-",
              formatC(lnAUC0INF_Le,format="f",digits=3),formatC(lnAUC0INF_Ls,format="f",digits=3),formatC(lnAUC0INF_Lp,format="f",digits=3),"-"),
 CI95_upper=c(formatC(lnCmax_Ue,format="f",digits=3),formatC(lnCmax_Us,format="f",digits=3),formatC(lnCmax_Up,format="f",digits=3),"-",
              formatC(lnAUC0t_Ue,format="f",digits=3),formatC(lnAUC0t_Us,format="f",digits=3),formatC(lnAUC0t_Up,format="f",digits=3),"-",
              formatC(lnpAUC_Ue,format="f",digits=3),formatC(lnpAUC_Us,format="f",digits=3),formatC(lnpAUC_Up,format="f",digits=3),"-",
              formatC(lnAUC0INF_Ue,format="f",digits=3),formatC(lnAUC0INF_Us,format="f",digits=3),formatC(lnAUC0INF_Up,format="f",digits=3),"-"))
}
else{
outputPoint<-data.frame(Parameter=c("lnCmax_intra","lnCmax_inter","lnCmax_intraclass","lnCmax_prob",
                                    "lnAUC0t_intra","lnAUC0t_inter","lnAUC0t_intraclass","lnAUC0t_prob",
                                    "lnAUC0INF_intra","lnAUC0INF_inter","lnAUC0INF_intraclass","lnAUC0INF_prob"),
 Point_Estimate=c(formatC(lnCmax_MSintra,format="f",digits=3),formatC(lnCmax_inter,format="f",digits=3),formatC(lnCmax_intraclass,format="f",digits=3),formatC(lnCmax_PFv,format="f",digits=5),
                  formatC(lnAUC0t_MSintra,format="f",digits=3),formatC(lnAUC0t_inter,format="f",digits=3),formatC(lnAUC0t_intraclass,format="f",digits=3),formatC(lnAUC0t_PFv,format="f",digits=5),
                  formatC(lnAUC0INF_MSintra,format="f",digits=3),formatC(lnAUC0INF_inter,format="f",digits=3),formatC(lnAUC0INF_intraclass,format="f",digits=3),formatC(lnAUC0INF_PFv,format="f",digits=5)),
 CI95_lower=c(formatC(lnCmax_Le,format="f",digits=3),formatC(lnCmax_Ls,format="f",digits=3),formatC(lnCmax_Lp,format="f",digits=3),"-",
              formatC(lnAUC0t_Le,format="f",digits=3),formatC(lnAUC0t_Ls,format="f",digits=3),formatC(lnAUC0t_Lp,format="f",digits=3),"-",
              formatC(lnAUC0INF_Le,format="f",digits=3),formatC(lnAUC0INF_Ls,format="f",digits=3),formatC(lnAUC0INF_Lp,format="f",digits=3),"-"),
 CI95_upper=c(formatC(lnCmax_Ue,format="f",digits=3),formatC(lnCmax_Us,format="f",digits=3),formatC(lnCmax_Up,format="f",digits=3),"-",
              formatC(lnAUC0t_Ue,format="f",digits=3),formatC(lnAUC0t_Us,format="f",digits=3),formatC(lnAUC0t_Up,format="f",digits=3),"-",
              formatC(lnAUC0INF_Ue,format="f",digits=3),formatC(lnAUC0INF_Us,format="f",digits=3),formatC(lnAUC0INF_Up,format="f",digits=3),"-"))
}
}
show(outputPoint) 
cat("\n")
cat("-------------------------------------------------\n")
cat(" intra: intra-subject variability\n")
cat(" inter: inter-subject variability\n")
cat(" intraclass: intraclass correlation\n")
cat(" prob: the probability for obtaining a negative estimate of inter-subject\n") 
cat("       variability\n")
cat(" CI95: 95% confidence interval \n")
cat("-------------------------------------------------\n")
cat(" **Interpretation:\n")
cat(" 1. Prior information of inter- and intra-subject variabilities can be used   \n")
cat("    for sample size determination.                                            \n")
cat(" 2. Intraclass correlation shows the precision of intrasubject variability.\n")
cat("    A negative estimate indicates that inter- and intra-variability on the   \n")
cat("    subjects are negatively correlated.                                       \n")
cat(" 3. Searle(1971) provided a formula for calculation of the probability for  \n")
cat("    obtaining a negative estimate of inter-subject variability. In addition,  \n")
cat("    a negative estimate may indicate that the general model is incorrect or   \n")
cat("    sample size is too small.  Thus,prob provides the probability for      \n")
cat("    obtaining a negative estimate.  If P value is less than 0.05,the chance  \n")                                
cat("    of obtaining a negative estimate is negligible.                          \n")
cat("\n")
cat("**Ref.:\n")
cat(" 1. Chow SC and Liu JP. Design and Analysis of Bioavailability-            \n")
cat("    Bioequivalence Studies. 3rd ed.,Chapman & Hall/CRC,New York (2009).  \n\n")
cat(" 2. Searle SR. Linear Models. John Wiley & Sons,New York (1971).          \n")
cat(" 3. Snedecor GW and Cochran WG. Statistical Methods. 7th ed., Iowa State  \n")
cat("    University Press,Ames,IA (1980).                                     \n\n")
cat(" 4. Hocking RP. The Analysis of Linear Models. Brooks/Cole,Monterey,CA   \n")
cat("    (1985).                                                                \n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
##Quantiles
lnCmax_1deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=1,type=1)
lnCmax_0.99deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.99,type=1)
lnCmax_0.95deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.95,type=1)
lnCmax_0.90deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.90,type=1)
lnCmax_0.75deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.75,type=1)
lnCmax_0.50deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.50,type=7)
lnCmax_0.25deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.25,type=1)
lnCmax_0.10deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.10,type=1)
lnCmax_0.05deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.05,type=1)
lnCmax_0.01deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0.01,type=1)
lnCmax_0deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra,probs=0,type=1)

lnCmax_1deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=1,type=1)
lnCmax_0.99deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.99,type=1)
lnCmax_0.95deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.95,type=1)
lnCmax_0.90deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.90,type=1)
lnCmax_0.75deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.75,type=1)
lnCmax_0.50deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.50,type=7)
lnCmax_0.25deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.25,type=1)
lnCmax_0.10deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.10,type=1)
lnCmax_0.05deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.05,type=1)
lnCmax_0.01deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0.01,type=1)
lnCmax_0deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter,probs=0,type=1)

lnAUC0t_1deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=1,type=1)
lnAUC0t_0.99deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.99,type=1)
lnAUC0t_0.95deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.95,type=1)
lnAUC0t_0.90deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.90,type=1)
lnAUC0t_0.75deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.75,type=1)
lnAUC0t_0.50deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.50,type=7)
lnAUC0t_0.25deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.25,type=1)
lnAUC0t_0.10deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.10,type=1)
lnAUC0t_0.05deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.05,type=1)
lnAUC0t_0.01deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0.01,type=1)
lnAUC0t_0deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra,probs=0,type=1)

lnAUC0t_1deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=1,type=1)
lnAUC0t_0.99deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.99,type=1)
lnAUC0t_0.95deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.95,type=1)
lnAUC0t_0.90deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.90,type=1)
lnAUC0t_0.75deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.75,type=1)
lnAUC0t_0.50deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.50,type=1)
lnAUC0t_0.25deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.25,type=1)
lnAUC0t_0.10deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.10,type=1)
lnAUC0t_0.05deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.05,type=1)
lnAUC0t_0.01deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0.01,type=1)
lnAUC0t_0deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter,probs=0,type=1)

if(pAUC){
lnpAUC_1deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=1,type=1)
lnpAUC_0.99deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.99,type=1)
lnpAUC_0.95deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.95,type=1)
lnpAUC_0.90deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.90,type=1)
lnpAUC_0.75deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.75,type=1)
lnpAUC_0.50deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.50,type=7)
lnpAUC_0.25deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.25,type=1)
lnpAUC_0.10deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.10,type=1)
lnpAUC_0.05deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.05,type=1)
lnpAUC_0.01deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0.01,type=1)
lnpAUC_0deciles_intra <- quantile(IntraInterlnpAUC00$Stud_Intra,probs=0,type=1)

lnpAUC_1deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=1,type=1)
lnpAUC_0.99deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.99,type=1)
lnpAUC_0.95deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.95,type=1)
lnpAUC_0.90deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.90,type=1)
lnpAUC_0.75deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.75,type=1)
lnpAUC_0.50deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.50,type=1)
lnpAUC_0.25deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.25,type=1)
lnpAUC_0.10deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.10,type=1)
lnpAUC_0.05deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.05,type=1)
lnpAUC_0.01deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0.01,type=1)
lnpAUC_0deciles_inter <- quantile(IntraInterlnpAUC00$Stud_Inter,probs=0,type=1)
}

if(multiple){
}
else{
lnAUC0INF_1deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=1,type=1)
lnAUC0INF_0.99deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.99,type=1)
lnAUC0INF_0.95deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.95,type=1)
lnAUC0INF_0.90deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.90,type=1)
lnAUC0INF_0.75deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.75,type=1)
lnAUC0INF_0.50deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.50,type=7)
lnAUC0INF_0.25deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.25,type=1)
lnAUC0INF_0.10deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.10,type=1)
lnAUC0INF_0.05deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.05,type=1)
lnAUC0INF_0.01deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0.01,type=1)
lnAUC0INF_0deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra,probs=0,type=1)

lnAUC0INF_1deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=1,type=1)
lnAUC0INF_0.99deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.99,type=1)
lnAUC0INF_0.95deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.95,type=1)
lnAUC0INF_0.90deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.90,type=1)
lnAUC0INF_0.75deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.75,type=1)
lnAUC0INF_0.50deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.50,type=7)
lnAUC0INF_0.25deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.25,type=1)
lnAUC0INF_0.10deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.10,type=1)
lnAUC0INF_0.05deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.05,type=1)
lnAUC0INF_0.01deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0.01,type=1)
lnAUC0INF_0deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter,probs=0,type=1)
}
cat(" Quantiles for Boxplots (intrasubj)  \n")
cat("--------------------------------------------------------------------------\n")
if(multiple){
if(pAUC){
Quantiles_intra<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
      lnCmax_Estimate=c(lnCmax_1deciles_intra,lnCmax_0.99deciles_intra,lnCmax_0.95deciles_intra,lnCmax_0.90deciles_intra,lnCmax_0.75deciles_intra,lnCmax_0.50deciles_intra,
                        lnCmax_0.25deciles_intra,lnCmax_0.10deciles_intra,lnCmax_0.05deciles_intra,lnCmax_0.01deciles_intra,lnCmax_0deciles_intra),
      lnAUC0t_Estimate=c(lnAUC0t_1deciles_intra,lnAUC0t_0.99deciles_intra,lnAUC0t_0.95deciles_intra,lnAUC0t_0.90deciles_intra,lnAUC0t_0.75deciles_intra,lnAUC0t_0.50deciles_intra,
                         lnAUC0t_0.25deciles_intra,lnAUC0t_0.10deciles_intra,lnAUC0t_0.05deciles_intra,lnAUC0t_0.01deciles_intra,lnAUC0t_0deciles_intra),
      lnpAUC_Estimate=c(lnpAUC_1deciles_intra,lnpAUC_0.99deciles_intra,lnpAUC_0.95deciles_intra,lnpAUC_0.90deciles_intra,lnpAUC_0.75deciles_intra,lnpAUC_0.50deciles_intra,
                         lnpAUC_0.25deciles_intra,lnpAUC_0.10deciles_intra,lnpAUC_0.05deciles_intra,lnpAUC_0.01deciles_intra,lnpAUC_0deciles_intra))                         
colnames(Quantiles_intra)<- c("Quantile","lnCmax_ss_Estimate","lnAUC(tau)ss_Estimate","lnpAUC_Estimate") 
}
else{
Quantiles_intra<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
      lnCmax_Estimate=c(lnCmax_1deciles_intra,lnCmax_0.99deciles_intra,lnCmax_0.95deciles_intra,lnCmax_0.90deciles_intra,lnCmax_0.75deciles_intra,lnCmax_0.50deciles_intra,
                        lnCmax_0.25deciles_intra,lnCmax_0.10deciles_intra,lnCmax_0.05deciles_intra,lnCmax_0.01deciles_intra,lnCmax_0deciles_intra),
      lnAUC0t_Estimate=c(lnAUC0t_1deciles_intra,lnAUC0t_0.99deciles_intra,lnAUC0t_0.95deciles_intra,lnAUC0t_0.90deciles_intra,lnAUC0t_0.75deciles_intra,lnAUC0t_0.50deciles_intra,
                         lnAUC0t_0.25deciles_intra,lnAUC0t_0.10deciles_intra,lnAUC0t_0.05deciles_intra,lnAUC0t_0.01deciles_intra,lnAUC0t_0deciles_intra))                         
colnames(Quantiles_intra)<- c("Quantile","lnCmax_ss_Estimate","lnAUC(tau)ss_Estimate") 
}
}
else{

if(pAUC){
Quantiles_intra<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
     lnCmax_Estimate=c(lnCmax_1deciles_intra,lnCmax_0.99deciles_intra,lnCmax_0.95deciles_intra,lnCmax_0.90deciles_intra,lnCmax_0.75deciles_intra,lnCmax_0.50deciles_intra,
                       lnCmax_0.25deciles_intra,lnCmax_0.10deciles_intra,lnCmax_0.05deciles_intra,lnCmax_0.01deciles_intra,lnCmax_0deciles_intra),
     lnAUC0t_Estimate=c(lnAUC0t_1deciles_intra,lnAUC0t_0.99deciles_intra,lnAUC0t_0.95deciles_intra,lnAUC0t_0.90deciles_intra,lnAUC0t_0.75deciles_intra,lnAUC0t_0.50deciles_intra,
                        lnAUC0t_0.25deciles_intra,lnAUC0t_0.10deciles_intra,lnAUC0t_0.05deciles_intra,lnAUC0t_0.01deciles_intra,lnAUC0t_0deciles_intra),
     lnpAUC_Estimate=c(lnpAUC_1deciles_intra,lnpAUC_0.99deciles_intra,lnpAUC_0.95deciles_intra,lnpAUC_0.90deciles_intra,lnpAUC_0.75deciles_intra,lnpAUC_0.50deciles_intra,
                        lnpAUC_0.25deciles_intra,lnpAUC_0.10deciles_intra,lnpAUC_0.05deciles_intra,lnpAUC_0.01deciles_intra,lnpAUC_0deciles_intra),                        
     lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_intra,lnAUC0INF_0.99deciles_intra,lnAUC0INF_0.95deciles_intra,lnAUC0INF_0.90deciles_intra,lnAUC0INF_0.75deciles_intra,lnAUC0INF_0.50deciles_intra,
                       lnAUC0INF_0.25deciles_intra,lnAUC0INF_0.10deciles_intra,lnAUC0INF_0.05deciles_intra,lnAUC0INF_0.01deciles_intra,lnAUC0INF_0deciles_intra))
}
else{                                              
Quantiles_intra<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
     lnCmax_Estimate=c(lnCmax_1deciles_intra,lnCmax_0.99deciles_intra,lnCmax_0.95deciles_intra,lnCmax_0.90deciles_intra,lnCmax_0.75deciles_intra,lnCmax_0.50deciles_intra,
                       lnCmax_0.25deciles_intra,lnCmax_0.10deciles_intra,lnCmax_0.05deciles_intra,lnCmax_0.01deciles_intra,lnCmax_0deciles_intra ),
     lnAUC0t_Estimate=c(lnAUC0t_1deciles_intra,lnAUC0t_0.99deciles_intra,lnAUC0t_0.95deciles_intra,lnAUC0t_0.90deciles_intra,lnAUC0t_0.75deciles_intra,lnAUC0t_0.50deciles_intra,
                        lnAUC0t_0.25deciles_intra,lnAUC0t_0.10deciles_intra,lnAUC0t_0.05deciles_intra,lnAUC0t_0.01deciles_intra,lnAUC0t_0deciles_intra),
     lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_intra,lnAUC0INF_0.99deciles_intra,lnAUC0INF_0.95deciles_intra,lnAUC0INF_0.90deciles_intra,lnAUC0INF_0.75deciles_intra,lnAUC0INF_0.50deciles_intra,
                       lnAUC0INF_0.25deciles_intra,lnAUC0INF_0.10deciles_intra,lnAUC0INF_0.05deciles_intra,lnAUC0INF_0.01deciles_intra,lnAUC0INF_0deciles_intra))
}
}
show(Quantiles_intra) 
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat(" Quantiles for Boxplots (intersubj)  \n")
cat("--------------------------------------------------------------------------\n")
if(multiple){
if(pAUC){
Quantiles_inter<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_inter,lnCmax_0.99deciles_inter,lnCmax_0.95deciles_inter,lnCmax_0.90deciles_inter,lnCmax_0.75deciles_inter,lnCmax_0.50deciles_inter,
                                              lnCmax_0.25deciles_inter,lnCmax_0.10deciles_inter,lnCmax_0.05deciles_inter,lnCmax_0.01deciles_inter,lnCmax_0deciles_inter),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_inter,lnAUC0t_0.99deciles_inter,lnAUC0t_0.95deciles_inter,lnAUC0t_0.90deciles_inter,lnAUC0t_0.75deciles_inter,lnAUC0t_0.50deciles_inter,
                                               lnAUC0t_0.25deciles_inter,lnAUC0t_0.10deciles_inter,lnAUC0t_0.05deciles_inter,lnAUC0t_0.01deciles_inter,lnAUC0t_0deciles_inter),
                            lnpAUC_Estimate=c(lnpAUC_1deciles_inter,lnpAUC_0.99deciles_inter,lnpAUC_0.95deciles_inter,lnpAUC_0.90deciles_inter,lnpAUC_0.75deciles_inter,lnpAUC_0.50deciles_inter,
                                               lnpAUC_0.25deciles_inter,lnpAUC_0.10deciles_inter,lnpAUC_0.05deciles_inter,lnpAUC_0.01deciles_inter,lnpAUC_0deciles_inter))
colnames(Quantiles_inter)<- c("Quantile","lnCmax_ss_Estimate","lnAUC(tau)ss_Estimate","lnpAUC_Estimate") 
}
else{
Quantiles_inter<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_inter,lnCmax_0.99deciles_inter,lnCmax_0.95deciles_inter,lnCmax_0.90deciles_inter,lnCmax_0.75deciles_inter,lnCmax_0.50deciles_inter,
                                              lnCmax_0.25deciles_inter,lnCmax_0.10deciles_inter,lnCmax_0.05deciles_inter,lnCmax_0.01deciles_inter,lnCmax_0deciles_inter ),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_inter,lnAUC0t_0.99deciles_inter,lnAUC0t_0.95deciles_inter,lnAUC0t_0.90deciles_inter,lnAUC0t_0.75deciles_inter,lnAUC0t_0.50deciles_inter,
                                               lnAUC0t_0.25deciles_inter,lnAUC0t_0.10deciles_inter,lnAUC0t_0.05deciles_inter,lnAUC0t_0.01deciles_inter,lnAUC0t_0deciles_inter ))
colnames(Quantiles_inter)<- c("Quantile","lnCmax_ss_Estimate","lnAUC(tau)ss_Estimate") 
}
}
else{
if(pAUC){
Quantiles_inter<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_inter,lnCmax_0.99deciles_inter,lnCmax_0.95deciles_inter,lnCmax_0.90deciles_inter,lnCmax_0.75deciles_inter,lnCmax_0.50deciles_inter,
                                              lnCmax_0.25deciles_inter,lnCmax_0.10deciles_inter,lnCmax_0.05deciles_inter,lnCmax_0.01deciles_inter,lnCmax_0deciles_inter),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_inter,lnAUC0t_0.99deciles_inter,lnAUC0t_0.95deciles_inter,lnAUC0t_0.90deciles_inter,lnAUC0t_0.75deciles_inter,lnAUC0t_0.50deciles_inter,
                                               lnAUC0t_0.25deciles_inter,lnAUC0t_0.10deciles_inter,lnAUC0t_0.05deciles_inter,lnAUC0t_0.01deciles_inter,lnAUC0t_0deciles_inter),
                            lnpAUC_Estimate=c(lnpAUC_1deciles_inter,lnpAUC_0.99deciles_inter,lnpAUC_0.95deciles_inter,lnpAUC_0.90deciles_inter,lnpAUC_0.75deciles_inter,lnpAUC_0.50deciles_inter,
                                               lnpAUC_0.25deciles_inter,lnpAUC_0.10deciles_inter,lnpAUC_0.05deciles_inter,lnpAUC_0.01deciles_inter,lnpAUC_0deciles_inter),
                            lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_inter,lnAUC0INF_0.99deciles_inter,lnAUC0INF_0.95deciles_inter,lnAUC0INF_0.90deciles_inter,lnAUC0INF_0.75deciles_inter,lnAUC0INF_0.50deciles_inter,
                                              lnAUC0INF_0.25deciles_inter,lnAUC0INF_0.10deciles_inter,lnAUC0INF_0.05deciles_inter,lnAUC0INF_0.01deciles_inter,lnAUC0INF_0deciles_inter))
}
else{
Quantiles_inter<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_inter,lnCmax_0.99deciles_inter,lnCmax_0.95deciles_inter,lnCmax_0.90deciles_inter,lnCmax_0.75deciles_inter,lnCmax_0.50deciles_inter,
                                              lnCmax_0.25deciles_inter,lnCmax_0.10deciles_inter,lnCmax_0.05deciles_inter,lnCmax_0.01deciles_inter,lnCmax_0deciles_inter),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_inter,lnAUC0t_0.99deciles_inter,lnAUC0t_0.95deciles_inter,lnAUC0t_0.90deciles_inter,lnAUC0t_0.75deciles_inter,lnAUC0t_0.50deciles_inter,
                                               lnAUC0t_0.25deciles_inter,lnAUC0t_0.10deciles_inter,lnAUC0t_0.05deciles_inter,lnAUC0t_0.01deciles_inter,lnAUC0t_0deciles_inter),
                            lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_inter,lnAUC0INF_0.99deciles_inter,lnAUC0INF_0.95deciles_inter,lnAUC0INF_0.90deciles_inter,lnAUC0INF_0.75deciles_inter,lnAUC0INF_0.50deciles_inter,
                                              lnAUC0INF_0.25deciles_inter,lnAUC0INF_0.10deciles_inter,lnAUC0INF_0.05deciles_inter,lnAUC0INF_0.01deciles_inter,lnAUC0INF_0deciles_inter))
}
}
show(Quantiles_inter)
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat(" Cook's distance  \n")
cat("----------------------------------------------------\n")
L<-length(TotalData$subj)
 
if(multiple){ 
 ##cook's distance
 if(pAUC){
  cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax_ss=cooks.distance(lnCmax),lnAUCtau_ss=cooks.distance(lnAUC0t),
                    lnpAUC=cooks.distance(lnpAUC))  
 }
 else{
  cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax_ss=cooks.distance(lnCmax),lnAUCtau_ss=cooks.distance(lnAUC0t)) 
 }
  cooks<-split(cook,cook$drug)[[1]]
 show(cooks)  
 cat("\n")
 cat("----------------------------------------------------\n")
 D1<-subset(cooks,cooks$lnCmax_ss >(4/(L-4-1)))
 D2<-subset(cooks,cooks$lnCmax_ss > 1)
 D3<-subset(cooks,cooks$lnCmax_ss >(4/L))
 D1_lnCmax_ss<-data.frame(subj=D1$subj,lnCmax_ss=D1$lnCmax_ss)
 colnames(D1_lnCmax_ss)<- c("Subj","  lnCmax_ss") 
 D2_lnCmax_ss<-data.frame(subj=D2$subj,lnCmax_ss=D2$lnCmax_ss)
 colnames(D2_lnCmax_ss)<- c("Subj","  lnCmax_ss")  
 D3_lnCmax_ss<-data.frame(subj=D3$subj,lnCmax_ss=D3$lnCmax_ss) 
 colnames(D3_lnCmax_ss)<- c("Subj","  lnCmax_ss") 
 
 D4<-subset(cooks,cooks$lnAUCtau_ss >(4/(L-4-1)))
 D5<-subset(cooks,cooks$lnAUCtau_ss > 1)
 D6<-subset(cooks,cooks$lnAUCtau_ss >(4/L))
 D4_lnAUCtau_ss<-data.frame(subj=D4$subj,lnAUCtau_ss=D4$lnAUCtau_ss) 
 D5_lnAUCtau_ss<-data.frame(subj=D5$subj,lnAUCtau_ss=D5$lnAUCtau_ss) 
 D6_lnAUCtau_ss<-data.frame(subj=D6$subj,lnAUCtau_ss=D6$lnAUCtau_ss) 
 
 if(pAUC){
 D4<-subset(cooks,cooks$lnpAUC >(4/(L-4-1)))
 D5<-subset(cooks,cooks$lnpAUC > 1)
 D6<-subset(cooks,cooks$lnpAUC >(4/L))
 D4_lnpAUC<-data.frame(subj=D4$subj,lnpAUC=D4$lnpAUC) 
 D5_lnpAUC<-data.frame(subj=D5$subj,lnpAUC=D5$lnpAUC) 
 D6_lnpAUC<-data.frame(subj=D6$subj,lnpAUC=D6$lnpAUC)  
 }
 
cat("**Criterion: D > 1\n") 
 if(length(D2$subj)== 0){
   cat(" lnCmax_ss: no outlier detected.\n") 
   }else{
   show(D2_lnCmax_ss)
   cat("\n")
   } 
  if(length(D5$subj)== 0){
   cat(" lnAUC(tau)ss: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D5_lnAUCtau_ss)
   cat("\n")
   } 
if(pAUC){
  if(length(D5$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D5_lnpAUC)
   cat("\n")
   } 
}
cat("**Criterion: D > 4/(n-k-1) =",(4/(L-5)),"\n")
  if(length(D1$subj)== 0){
   cat(" lnCmax_ss: no outlier detected\n") 
   }else{
   show(D1_lnCmax_ss)
   cat("\n")
   } 
  if(length(D4$subj)== 0){
   cat(" lnAUC(tau)ss: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D4_lnAUCtau_ss)
   cat("\n")
   } 
if(pAUC){
  if(length(D4$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D4_lnpAUC)
   cat("\n")
   } 
}
cat("**Criterion: D > (4/n) =",(4/L),"\n") 
 if(length(D3$subj)== 0){
   cat(" lnCmax_ss: no outlier detected\n") 
   }else{
   show(D3_lnCmax_ss)
   cat("\n")
   } 
  if(length(D6$subj)== 0){
   cat(" lnAUC(tau)ss: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D6_lnAUCtau_ss)
   cat("\n")
   }    
if(pAUC){
  if(length(D6$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n") 
   cat("\n")
   }else{
   show(D6_lnpAUC)
   cat("\n")
   }   
}
 }
 else{
  ##cook's distance
if(pAUC){
  cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax=cooks.distance(lnCmax),lnAUC0t=cooks.distance(lnAUC0t),
  lnpAUC=cooks.distance(lnpAUC),lnAUC0INF= cooks.distance(lnAUC0INF)) 
}
else{
  cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax=cooks.distance(lnCmax),lnAUC0t=cooks.distance(lnAUC0t),
  lnAUC0INF= cooks.distance(lnAUC0INF)) 
}
  cooks<-split(cook,cook$drug)[[1]] 
 show(cooks)  
 cat("\n")
 cat("\n")
 cat("----------------------------------------------------\n")
 D1<-subset(cooks,cooks$lnCmax >(4/(L-4-1)))
 D2<-subset(cooks,cooks$lnCmax > 1)
 D3<-subset(cooks,cooks$lnCmax >(4/L))
 D1_lnCmax<-data.frame(subj=D1$subj,lnCmax=D1$lnCmax)
 D2_lnCmax<-data.frame(subj=D2$subj,lnCmax=D2$lnCmax)
 D3_lnCmax<-data.frame(subj=D3$subj,lnCmax=D3$lnCmax)

 D4<-subset(cooks,cooks$lnAUC0t >(4/(L-4-1)))
 D5<-subset(cooks,cooks$lnAUC0t > 1)
 D6<-subset(cooks,cooks$lnAUC0t >(4/L))
 D4_lnAUC0t<-data.frame(subj=D4$subj,lnAUC0t=D4$lnAUC0t)
 D5_lnAUC0t<-data.frame(subj=D5$subj,lnAUC0t=D5$lnAUC0t)
 D6_lnAUC0t<-data.frame(subj=D6$subj,lnAUC0t=D6$lnAUC0t)
 
if(pAUC){
 D4<-subset(cooks,cooks$lnpAUC >(4/(L-4-1)))
 D5<-subset(cooks,cooks$lnpAUC > 1)
 D6<-subset(cooks,cooks$lnpAUC >(4/L))
 D4_lnpAUC<-data.frame(subj=D4$subj,lnpAUC=D4$lnpAUC)
 D5_lnpAUC<-data.frame(subj=D5$subj,lnpAUC=D5$lnpAUC)
 D6_lnpAUC<-data.frame(subj=D6$subj,lnpAUC=D6$lnpAUC)
}
 
 D7<-subset(cooks,cooks$lnAUC0INF >(4/(L-4-1)))
 D8<-subset(cooks,cooks$lnAUC0INF > 1)
 D9<-subset(cooks,cooks$lnAUC0INF >(4/L))
 D7_lnAUC0INF<-data.frame(subj=D4$subj,lnAUC0INF=D4$lnAUC0INF)
 D8_lnAUC0INF<-data.frame(subj=D5$subj,lnAUC0INF=D5$lnAUC0INF)
 D9_lnAUC0INF<-data.frame(subj=D6$subj,lnAUC0INF=D6$lnAUC0INF)
 
cat("**Criterion: D > 1\n")
 if(length(D2$subj)== 0){
   cat(" lnCmax: no outlier detected.\n")
   }else{
   show(D2_lnCmax)
   cat("\n")
   }
  if(length(D5$subj)== 0){
   cat(" lnAUC0t: no outlier detected.\n")
   }else{
   show(D5_lnAUC0t)
   cat("\n")
   }
if(pAUC){
  if(length(D5$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n")
   }else{
   show(D5_lnpAUC)
   cat("\n")
   }
}
   if(length(D8$subj)== 0){
   cat(" lnAUC0INF: no outlier detected.\n")
   cat("\n")
   }else{
   show(D8_lnAUC0INF)
   cat("\n")
   }  
cat("**Criterion: D > 4/(n-k-1) =",(4/(L-5)),"\n")
  if(length(D1$subj)== 0){
   cat(" lnCmax: no outlier detected.\n")
   }else{
   show(D1_lnCmax)
   cat("\n")
   }
  if(length(D4$subj)== 0){
   cat(" lnAUC0t: no outlier detected.\n")
   }else{
   show(D4_lnAUC0t)
   cat("\n")
   }
if(pAUC){
  if(length(D4$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n")
   }else{
   show(D4_lnpAUC)
   cat("\n")
   }
}
  if(length(D7$subj)== 0){
   cat(" lnAUC0INF: no outlier detected.\n")
   cat("\n")
   }else{
   show(D7_lnAUC0INF)
   cat("\n")
   } 
cat("**Criterion: D > 4/n =",(4/L),"\n")
 if(length(D3$subj)== 0){
   cat(" lnCmax: no outlier detected.\n")
   }else{
   show(D3_lnCmax)
   cat("\n")
   }
  if(length(D6$subj)== 0){
   cat(" lnAUC0t: no outlier detected.\n")
   cat("\n")
   }else{
   show(D6_lnAUC0t)
   cat("\n")
  }
if(pAUC){
  if(length(D6$subj)== 0){
   cat(" lnpAUC: no outlier detected.\n")
   cat("\n")
   }else{
   show(D6_lnpAUC)
   cat("\n")
  }
}
   if(length(D9$subj)== 0){
   cat(" lnAUC0INF: no outlier detected.\n")
   cat("\n")
   }else{
   show(D9_lnAUC0INF)
   cat("\n")
   }  
}
cat("--------------------------------------------------------------------------\n")
cat("**Interpretation:\n")
cat(" Cook's distance is used to detect outliers and is a measure of the \n")
cat(" simultaneous change in the parameter estimates when an observation is \n")
cat(" deleted from analysis. Fox(1991) suggested a cut-off for detecting         \n")
cat(" influential values of D greater than 4/(n-k-1),where n was sample size    \n")
cat(" and k was the number of independents. Others may use D > 1 as the criterion\n")
cat(" for a serious outlier problem,while using D > (4/n) the criterion for a   \n")
cat(" possible outlier problem.                                                \n\n")
cat("**Ref: Fox J. Regression Diagnostics. Thousand Oak,CA: Sage Publications.\n")
cat("      (1991).  \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
sink()
close(zzoda)
}
} 

