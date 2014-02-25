###
### This is for parallel or replicated crossover BE study. non-replicate --> BANOVA() --YJ
###

RepMIX<-function(TotalData,L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,ref_lnpAUC,test_lnCmax,test_lnAUC0t,test_lnAUC0INF,
    test_lnpAUC,lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
    lnpAUC_theta1,lnpAUC_theta2,parallel=FALSE, multiple=FALSE)
{
### require(nlme)

lin.AUC<-lin.AUC
pAUC<-pAUC               ### for pAUC
lambda_z_calc<-lambda_z_calc
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

if(pAUC){lnpAUC_theta1<-BE_LL; lnpAUC_theta2<-BE_UL}   ### something can be wrong if this line is req.  -YJ
####
ctrl <- lmeControl(opt='optim')    ### default was 'nlminb' for lme(); but it will fail to converge frequently;
                                   ### changed back to old default 'optim' & try
                                   ### ref. link: ??lmeControl --> click 'nlem::lmeControl' for more inf.
                                   ### & https://stats.stackexchange.com/questions/40647/lme-error-iteration-limit-reached
                                   ### same for following lme().  --YJ
####
##Cmax/Cmax_ss
if(parallel){                      ## so here is for parallel BE!
   if(multiple){  ### here means for the parallel, multiple-dose study
    cat("*** This is a 2-treatment parallel multiple-dosed study.\n\n")
    description_BE_criteria(BE_LL,BE_UL)
    if(pAUC){
    Data<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),Cmax_ss=TotalData$Cmax,
                     AUCtau_ss=TotalData$AUC0t,partAUC=TotalData$partAUC,lnCmax_ss=TotalData$lnCmax,
                     lnAUCtau_ss=TotalData$lnAUC0t,lnpAUC=TotalData$lnpAUC)
    }
    else{
    Data<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),Cmax_ss=TotalData$Cmax, 
                     AUCtau_ss=TotalData$AUC0t,lnCmax_ss=TotalData$lnCmax,lnAUCtau_ss=TotalData$lnAUC0t)
    }
   }
   else{
    cat("*** This is a 2-treatment parallel single-dosed study.\n\n")
    description_BE_criteria(BE_LL,BE_UL)
   }
 }
else{   ### for replicated, sinlge-dose only! replicated study cannot have multiple-dose study. 
    prdcount<-length(levels(TotalData$prd))   ## count # of periods (such as TRT/RTRT/etc.)
    cat(paste("*** This is a 2-treatment, 2-sequence, and ",prdcount,"-period replicated design.\n\n",sep=""))
    description_BE_criteria(BE_LL,BE_UL)
} 
cat("-------------------------------------------------\n")
cat("\n\n")
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study             \n")
} 
cat("-------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: Cmax_ss                                             \n")
}
else{
cat("  Dependent Variable: Cmax                                                \n")
}
### if(!parallel) show(TotalData$Cmax);readline(" pause here")
lme_lm.mod(TotalData$Cmax, TotalData, lme.switch="A")  ### for multiple-dose, Cmax_ss == Cmax

##AUC0t/AUC(tau)ss
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study             \n")
} 
cat("-------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: AUCtau_ss                                           \n")
}
else{
cat("  Dependent Variable: AUC0t                                               \n")
}
lme_lm.mod(TotalData$AUC0t, TotalData, lme.switch="B")

### for pAUC
###
if(pAUC){
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study             \n")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: partAUC                                 \n")
lme_lm.mod(TotalData$partAUC, TotalData, lme.switch="D")
}
### end of pAUC

### AUC0-inf
if(multiple){
}
else{
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study                 \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: AUC0INF                                   \n")
lme_lm.mod(TotalData$AUC0INF, TotalData, lme.switch="C")
}

##lnCmax /lnCmax_ss
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study                 \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
} 
cat("-------------------------------------------------\n")
if(parallel){
if(multiple){
cat("  Dependent Variable: log(Cmax_ss)                              \n")
modlnCmax_ss<-lm(log(Cmax) ~ drug, data=TotalData)          ### this will be req. for later use.
}
else{
cat("  Dependent Variable: log(Cmax)                                 \n")
modlnCmax<-lm(log(Cmax) ~ drug, data=TotalData)             ### this will be req. for later use.
}
}
else{
cat("  Dependent Variable: log(Cmax)                                           \n")
modlnCmax<-lme(log(Cmax) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=TotalData, method="REML")   ### this will be req. for later use.
}
lme_lm.mod(log(TotalData$Cmax), TotalData, lme.switch="A1")

## lnAUC0t/lnAUC(tau)ss 
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study             \n")
} 
cat("-------------------------------------------------\n")
if(parallel){
if(multiple){
cat("  Dependent Variable: lnAUC(tau)ss                            \n")
modlnAUCtau_ss<-lm(log(AUC0t) ~ drug, data=TotalData)   ### req. for later use. 
}
else{
cat("  Dependent Variable: log(AUC0t)                              \n") 
modlnAUC0t<-lm(log(AUC0t) ~ drug, data=TotalData)       ### req. for later use.
}
}
else{
cat("  Dependent Variable: log(AUC0t)                              \n")     
modlnAUC0t<-lme(log(AUC0t) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=TotalData, method="REML")
}  
lme_lm.mod(log(TotalData$AUC0t), TotalData, lme.switch="B1")
###
### doing ln(pAUC)
###
if(pAUC){
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
modlnpAUC<-lm(log(partAUC) ~drug, data=TotalData)
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study             \n")
modlnpAUC<-lme(log(partAUC) ~ seq +  prd + drug, random=~drug - 1|subj, control=ctrl,
               weights=varIdent(form = ~ 1 | drug), 
               data=TotalData, method="REML")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: log(pAUC)                               \n")
lme_lm.mod(log(TotalData$partAUC), TotalData, lme.switch="D1")
}
### end of ln(pAUC)
##lnAUC0INF
if(multiple){
}
else{
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study                \n")
modlnAUC0INF<-lm(log(AUC0INF) ~drug, data=TotalData)
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modlnAUC0INF<-lme(log(AUC0INF) ~ seq +  prd + drug, random=~drug - 1|subj, control=ctrl,
               weights=varIdent(form = ~ 1 | drug), 
               data=TotalData, method="REML")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: log(AUC0INF)                              \n")
lme_lm.mod(log(TotalData$AUC0INF), TotalData, lme.switch="C1")
}
if(parallel){
### show(summary(modlnCmax))
### show(summary(modlnCmax)$df)
### show(summary(modlnCmax)$df[2])
### show(summary(modlnCmax)$coefficients)
### show(summary(modlnCmax)$coefficients[2,1])
### show(summary(modlnCmax)$coefficients[2,2])
### show(summary(modlnCmax)$coefficients[2,3])
### show(summary(modlnCmax)$residuals)
### show(summary(modlnCmax)$r.squared)
### show(intervals(fm1, which = "fixed", level=0.9))
### show(intervals(fm1, level=0.9))
### cat("\n")
  if(multiple){
   upperCmax_ss<-100*exp(summary(modlnCmax_ss)$coefficients[2,1])*exp(qt(0.95,summary(modlnCmax_ss)$df[2])*summary(modlnCmax_ss)$coefficients[2,2]) 
   lowerCmax_ss<-100*exp(summary(modlnCmax_ss)$coefficients[2,1])*exp(-qt(0.95,summary(modlnCmax_ss)$df[2])*summary(modlnCmax_ss)$coefficients[2,2])
   upperAUCtau_ss<-100*exp(summary(modlnAUCtau_ss)$coefficients[2,1])*exp(qt(0.95,summary(modlnAUCtau_ss)$df[2])*summary(modlnAUCtau_ss)$coefficients[2,2])
   lowerAUCtau_ss<-100*exp(summary(modlnAUCtau_ss)$coefficients[2,1])*exp(-qt(0.95,summary(modlnAUCtau_ss)$df[2])*summary(modlnAUCtau_ss)$coefficients[2,2])
   SlnCmax_ss<-(summary(modlnCmax_ss)$coefficients[2,1])
   SlnAUCtau_ss<-(summary(modlnAUCtau_ss)$coefficients[2,1])
   SE_lnCmax_ss<-summary(modlnCmax_ss)$coefficients[2,2]
   SE_lnAUCtau_ss<-summary(modlnAUCtau_ss)$coefficients[2,2]
  }
  else{  
   upperCmax<-100*exp(summary(modlnCmax)$coefficients[2,1])*exp(qt(0.95,summary(modlnCmax)$df[2])*summary(modlnCmax)$coefficients[2,2]) 
   lowerCmax<-100*exp(summary(modlnCmax)$coefficients[2,1])*exp(-qt(0.95,summary(modlnCmax)$df[2])*summary(modlnCmax)$coefficients[2,2])
   upperAUC0t<-100*exp(summary(modlnAUC0t)$coefficients[2,1])*exp(qt(0.95,summary(modlnAUC0t)$df[2])*summary(modlnAUC0t)$coefficients[2,2])
   lowerAUC0t<-100*exp(summary(modlnAUC0t)$coefficients[2,1])*exp(-qt(0.95,summary(modlnAUC0t)$df[2])*summary(modlnAUC0t)$coefficients[2,2])
   SlnCmax<-(summary(modlnCmax)$coefficients[2,1])
   SlnAUC0t<-(summary(modlnAUC0t)$coefficients[2,1])
   SE_lnCmax<-summary(modlnCmax)$coefficients[2,2]
   SE_lnAUC0t<-summary(modlnAUC0t)$coefficients[2,2]
   upperAUC0INF<-100*exp(summary(modlnAUC0INF)$coefficients[2,1])*exp(qt(0.95,summary(modlnAUC0INF)$df[2])*summary(modlnAUC0INF)$coefficients[2,2])
   lowerAUC0INF<-100*exp(summary(modlnAUC0INF)$coefficients[2,1])*exp(-qt(0.95,summary(modlnAUC0INF)$df[2])*summary(modlnAUC0INF)$coefficients[2,2])
   SlnAUC0INF<-(summary(modlnAUC0INF)$coefficients[2,1]) 
   SE_lnAUC0INF<-summary(modlnAUC0INF)$coefficients[2,2]
   }
   if(pAUC){
   upperlnpAUC<-100*exp(summary(modlnpAUC)$coefficients[2,1])*exp(qt(0.95,summary(modlnpAUC)$df[2])*summary(modlnpAUC)$coefficients[2,2])
   lowerlnpAUC<-100*exp(summary(modlnpAUC)$coefficients[2,1])*exp(-qt(0.95,summary(modlnpAUC)$df[2])*summary(modlnpAUC)$coefficients[2,2])
   SlnpAUC<-(summary(modlnpAUC)$coefficients[2,1])
   SE_lnpAUC<-summary(modlnpAUC)$coefficients[2,2]
   }
  }
else{ 
if(prdcount==3){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][5,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][5,3])*summary(modlnCmax)[20][[1]][5,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][5,3])*summary(modlnCmax)[20][[1]][5,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][5,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][5,3])*summary(modlnAUC0t)[20][[1]][5,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][5,3])*summary(modlnAUC0t)[20][[1]][5,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][5,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][5,3])*summary(modlnAUC0INF)[20][[1]][5,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][5,3])*summary(modlnAUC0INF)[20][[1]][5,2])

SlnCmax<-(summary(modlnCmax)[20][[1]][5,1])
SlnAUC0t<-(summary(modlnAUC0t)[20][[1]][5,1])
SlnAUC0INF<-(summary(modlnAUC0INF)[20][[1]][5,1]) 

SE_lnCmax<-summary(modlnCmax)[20][[1]][5,2]
SE_lnAUC0t<-summary(modlnAUC0t)[20][[1]][5,2]
SE_lnAUC0INF<-summary(modlnAUC0INF)[20][[1]][5,2]

if(pAUC){
upperlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][5,1])*exp(qt(0.95,summary(modlnpAUC)[20][[1]][5,3])*summary(modlnpAUC)[20][[1]][5,2])
lowerlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnpAUC)[20][[1]][5,3])*summary(modlnpAUC)[20][[1]][5,2])
SlnpAUC<-(summary(modlnpAUC)[20][[1]][5,1])
SE_lnpAUC<-summary(modlnpAUC)[20][[1]][5,2]
}
}
if (prdcount==4){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][6,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][6,3])*summary(modlnCmax)[20][[1]][6,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][6,3])*summary(modlnCmax)[20][[1]][6,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][6,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][6,3])*summary(modlnAUC0t)[20][[1]][6,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][6,3])*summary(modlnAUC0t)[20][[1]][6,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][6,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][6,3])*summary(modlnAUC0INF)[20][[1]][6,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][6,3])*summary(modlnAUC0INF)[20][[1]][6,2])

SlnCmax<-(summary(modlnCmax)[20][[1]][6,1])
SlnAUC0t<-(summary(modlnAUC0t)[20][[1]][6,1])
SlnAUC0INF<-(summary(modlnAUC0INF)[20][[1]][6,1])  

SE_lnCmax<-summary(modlnCmax)[20][[1]][6,2]
SE_lnAUC0t<-summary(modlnAUC0t)[20][[1]][6,2]
SE_lnAUC0INF<-summary(modlnAUC0INF)[20][[1]][6,2]
if(pAUC){
upperlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][6,1])*exp(qt(0.95,summary(modlnpAUC)[20][[1]][6,3])*summary(modlnpAUC)[20][[1]][6,2])
lowerlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnpAUC)[20][[1]][6,3])*summary(modlnpAUC)[20][[1]][6,2])
SlnpAUC<-(summary(modlnpAUC)[20][[1]][6,1])
SE_lnpAUC<-summary(modlnpAUC)[20][[1]][6,2]
}
}
if (prdcount==5){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][7,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][7,3])*summary(modlnCmax)[20][[1]][7,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][7,3])*summary(modlnCmax)[20][[1]][7,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][7,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][7,3])*summary(modlnAUC0t)[20][[1]][7,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][7,3])*summary(modlnAUC0t)[20][[1]][7,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][7,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][7,3])*summary(modlnAUC0INF)[20][[1]][7,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][7,3])*summary(modlnAUC0INF)[20][[1]][7,2])

SlnCmax<-(summary(modlnCmax)[20][[1]][7,1])
SlnAUC0t<-(summary(modlnAUC0t)[20][[1]][7,1])
SlnAUC0INF<-(summary(modlnAUC0INF)[20][[1]][7,1]) 

SE_lnCmax<-summary(modlnCmax)[20][[1]][7,2]
SE_lnAUC0t<-summary(modlnAUC0t)[20][[1]][7,2]
SE_lnAUC0INF<-summary(modlnAUC0INF)[20][[1]][7,2]
if(pAUC){
upperlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][7,1])*exp(qt(0.95,summary(modlnpAUC)[20][[1]][7,3])*summary(modlnpAUC)[20][[1]][7,2])
lowerlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnpAUC)[20][[1]][7,3])*summary(modlnpAUC)[20][[1]][7,2])
SlnpAUC<-(summary(modlnpAUC)[20][[1]][7,1])
SE_lnpAUC<-summary(modlnpAUC)[20][[1]][7,2]
}
}
if (prdcount==6){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][8,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][8,3])*summary(modlnCmax)[20][[1]][8,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][8,3])*summary(modlnCmax)[20][[1]][8,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][8,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][8,3])*summary(modlnAUC0t)[20][[1]][8,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][8,3])*summary(modlnAUC0t)[20][[1]][8,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][8,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][8,3])*summary(modlnAUC0INF)[20][[1]][8,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][8,3])*summary(modlnAUC0INF)[20][[1]][8,2])

SlnCmax<-(summary(modlnCmax)[20][[1]][8,1])
SlnAUC0t<-(summary(modlnAUC0t)[20][[1]][8,1])
SlnAUC0INF<-(summary(modlnAUC0INF)[20][[1]][8,1])
 
SE_lnCmax<-summary(modlnCmax)[20][[1]][8,2]
SE_lnAUC0t<-summary(modlnAUC0t)[20][[1]][8,2]
SE_lnAUC0INF<-summary(modlnAUC0INF)[20][[1]][8,2]
if(pAUC){
upperlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][8,1])*exp(qt(0.95,summary(modlnpAUC)[20][[1]][8,3])*summary(modlnpAUC)[20][[1]][8,2])
lowerlnpAUC<-100*exp(summary(modlnpAUC)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnpAUC)[20][[1]][8,3])*summary(modlnpAUC)[20][[1]][8,2])
SlnpAUC<-(summary(modlnpAUC)[20][[1]][8,1])
SE_lnpAUC<-summary(modlnpAUC)[20][[1]][8,2]
}
}
}

####two-one side (TOST) and Anderson and Hauck's test

if(multiple){
#lnCmax_ss
TL_lnCmax_ss<-(SlnCmax_ss-log(lnCmax_theta1))/SE_lnCmax_ss
TU_lnCmax_ss<-(SlnCmax_ss-log(lnCmax_theta2))/SE_lnCmax_ss
PTL_lnCmax_ss<-pt(TL_lnCmax_ss,L1+L2-2)
PTU_lnCmax_ss<-pt(TU_lnCmax_ss,L1+L2-2)
TAH_lnCmax_ss<-SlnCmax_ss/SE_lnCmax_ss
NP_lnCmax_ss<-log(lnCmax_theta2)/SE_lnCmax_ss
EP_lnCmax_ss<- pt((abs(TAH_lnCmax_ss)-NP_lnCmax_ss),L1+L2-2) - pt((-abs(TAH_lnCmax_ss)-NP_lnCmax_ss),L1+L2-2)

#lnAUCtau_ss
TL_lnAUCtau_ss<-(SlnAUCtau_ss-log(lnAUC0t_theta1))/SE_lnAUCtau_ss
TU_lnAUCtau_ss<-(SlnAUCtau_ss-log(lnAUC0t_theta2))/SE_lnAUCtau_ss
PTL_lnAUCtau_ss<-pt(TL_lnAUCtau_ss,L1+L2-2)
PTU_lnAUCtau_ss<-pt(TU_lnAUCtau_ss,L1+L2-2)
TAH_lnAUCtau_ss<-SlnAUCtau_ss/SE_lnAUCtau_ss
NP_lnAUCtau_ss<-log(lnAUC0t_theta2)/SE_lnAUCtau_ss
EP_lnAUCtau_ss<- pt((abs(TAH_lnAUCtau_ss)-NP_lnAUCtau_ss),L1+L2-2) - pt((-abs(TAH_lnAUCtau_ss)-NP_lnAUCtau_ss),L1+L2-2)
}
else{
#lnCmax
TL_lnCmax<-(SlnCmax-log(lnCmax_theta1))/SE_lnCmax
TU_lnCmax<-(SlnCmax-log(lnCmax_theta2))/SE_lnCmax
PTL_lnCmax<-pt(TL_lnCmax,L1+L2-2)
PTU_lnCmax<-pt(TU_lnCmax,L1+L2-2)
TAH_lnCmax<-SlnCmax/SE_lnCmax
NP_lnCmax<-log(lnCmax_theta2)/SE_lnCmax
EP_lnCmax<- pt((abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2) - pt((-abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2)

#lnAUC0t
TL_lnAUC0t<-(SlnAUC0t-log(lnAUC0t_theta1))/SE_lnAUC0t
TU_lnAUC0t<-(SlnAUC0t-log(lnAUC0t_theta2))/SE_lnAUC0t
PTL_lnAUC0t<-pt(TL_lnAUC0t,L1+L2-2)
PTU_lnAUC0t<-pt(TU_lnAUC0t,L1+L2-2)
TAH_lnAUC0t<-SlnAUC0t/SE_lnAUC0t
NP_lnAUC0t<-log(lnAUC0t_theta2)/SE_lnAUC0t
EP_lnAUC0t<- pt((abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2) - pt((-abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2)

#lnAUC0INF
TL_lnAUC0INF<-(SlnAUC0INF-log(lnAUC0INF_theta1))/SE_lnAUC0INF
TU_lnAUC0INF<-(SlnAUC0INF-log(lnAUC0INF_theta2))/SE_lnAUC0INF
PTL_lnAUC0INF<-pt(TL_lnAUC0INF,L1+L2-2)
PTU_lnAUC0INF<-pt(TU_lnAUC0INF,L1+L2-2)
TAH_lnAUC0INF<-SlnAUC0INF/SE_lnAUC0INF
NP_lnAUC0INF<-log(lnAUC0INF_theta2)/SE_lnAUC0INF
EP_lnAUC0INF<- pt((abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2) - pt((-abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2)  
}

if(pAUC){
#ln(pAUC)
TL_lnpAUC<-(SlnpAUC-log(lnpAUC_theta1))/SE_lnpAUC
TU_lnpAUC<-(SlnpAUC-log(lnpAUC_theta2))/SE_lnpAUC
PTL_lnpAUC<-pt(TL_lnpAUC,L1+L2-2)
PTU_lnpAUC<-pt(TU_lnpAUC,L1+L2-2)
TAH_lnpAUC<-SlnpAUC/SE_lnpAUC
NP_lnpAUC<-log(lnpAUC_theta2)/SE_lnpAUC
EP_lnpAUC<- pt((abs(TAH_lnpAUC)-NP_lnpAUC),L1+L2-2) - pt((-abs(TAH_lnpAUC)-NP_lnpAUC),L1+L2-2)
}

if(parallel){
cat("  Summary Report: - Pivotal Parameters of Parallel BE Study -             \n")
 }
else{ 
cat("  Summary Report: - Pivotal Parameters of Replicate BE Study -            \n")
} 
cat("-------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: log(Cmax_ss)                                        \n")
}
else{
cat("  Dependent Variable: log(Cmax)                                           \n")
}
cat("-------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
if(multiple){
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnCmax_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnCmax_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnCmax, "\n")
cat("         MEAN-test =",test_lnCmax, "\n")
cat("                SE =",SE_lnCmax_ss, "\n")
cat("Estimate(test-ref) =",SlnCmax_ss, "\n")
}
else{
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnCmax_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnCmax_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnCmax, "\n")
cat("         MEAN-test =",test_lnCmax, "\n")
cat("                SE =",SE_lnCmax, "\n")
cat("Estimate(test-ref) =",SlnCmax, "\n")
}
cat("\n")
if(multiple){
cat("************** Classical (Shortest) 90% C.I. for lnCmax_ss ****************\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(SlnCmax_ss),format="f",digits=3)),
                   CI90_lower=c( formatC(lowerCmax_ss,format="f",digits=3)),
                   CI90_upper=c( formatC(upperCmax_ss,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnCmax_ss)
CI90_lower <- lowerCmax_ss
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for Cmax_ss =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
cat("---------------------------------------------------------------------------\n")
}
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnCmax_ss ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = Data)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  }
## end of Welch T test
}
else{
cat("**************** Classical (Shortest) 90% C.I. for lnCmax ****************\n")

cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(SlnCmax),format="f",digits=3)),
                   CI90_lower=c(formatC(lowerCmax,format="f",digits=3)),
                   CI90_upper=c(formatC(upperCmax,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnCmax)
CI90_lower <- lowerCmax
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for Cmax =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
}
cat("---------------------------------------------------------------------------\n")
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnCmax ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = TotalData)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  }
## end of Welch T test
}
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
if(multiple){
TOST_lnCmax_ss<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnCmax_ss,format="f",digits=4),formatC(TU_lnCmax_ss,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnCmax_ss,format="f",digits=4),formatC(PTU_lnCmax_ss,format="f",digits=4))) 
colnames(TOST_lnCmax_ss)<- c("TOST","  T value","  P value")
show(TOST_lnCmax_ss)
if(PTL_lnCmax_ss >= 0.05 || PTU_lnCmax_ss >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
}
else{                           
TOST_lnCmax<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnCmax,format="f",digits=4),formatC(TU_lnCmax,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnCmax,format="f",digits=4),formatC(PTU_lnCmax,format="f",digits=4))) 
colnames(TOST_lnCmax)<- c("TOST","  T value","  P value")
show(TOST_lnCmax) 
cat("\n")
if(PTL_lnCmax >= 0.05 || PTU_lnCmax >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
if(multiple){
cat("          P value =",formatC(EP_lnCmax_ss,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnCmax_ss >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
}
else{
cat("          P value =",formatC(EP_lnCmax,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnCmax >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
}
cat("---------------------------------------------------------------------------\n")
cat("\n")
cat("\n")

if(parallel){
cat("  Pivotal  Parameters of BE Study - Summary Report  - parallel BE study    \n")
 }
else{ 
cat("  Pivotal  Parameters of BE Study - Summary Report  - replicate BE study   \n")
} 
cat("-------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: log(AUCtau_ss)                                      \n")
}
else{
cat("  Dependent Variable: log(AUC0t)                                          \n")
}                                         
cat("-------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
if(multiple){
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0t_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnAUC0t_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnAUC0t, "\n")
cat("         MEAN-test =",test_lnAUC0t, "\n")
cat("                SE =",SE_lnAUCtau_ss, "\n")
cat("Estimate(test-ref) =",SlnAUCtau_ss, "\n")
}
else{
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0t_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnAUC0t_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnAUC0t, "\n")
cat("         MEAN-test =",test_lnAUC0t, "\n")
cat("                SE =",SE_lnAUC0t, "\n")
cat("Estimate(test-ref) =",SlnAUC0t, "\n")
}
cat("\n")
if(multiple){
cat("*********** Classical (Shortest) 90% C.I. for lnAUC(tau)ss **************\n")
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(SlnAUCtau_ss),format="f",digits=3)),
                   CI90_lower=c( formatC(lowerAUCtau_ss,format="f",digits=3)),
                   CI90_upper=c( formatC(upperAUCtau_ss,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnAUCtau_ss)
CI90_lower <- lowerAUCtau_ss
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for lnAUC(tau)ss =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
}
cat("---------------------------------------------------------------------------\n")
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnAUCtau_ss ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = Data)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  cat("\n")
}
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnAUCtau_ss<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnAUCtau_ss,format="f",digits=4),formatC(TU_lnAUCtau_ss,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnAUCtau_ss,format="f",digits=4),formatC(PTU_lnAUCtau_ss,format="f",digits=4))) 
colnames(TOST_lnAUCtau_ss)<- c("TOST","  T value","  P value")
show(TOST_lnAUCtau_ss) 
cat("\n")
if(PTL_lnAUCtau_ss >= 0.05 || PTU_lnAUCtau_ss >= 0.05){
description_TOST_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
else{
description_TOST1_lnCmax(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
}
else{
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0t ****************\n")
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(SlnAUC0t),format="f",digits=3)),
                   CI90_lower=c( formatC(lowerAUC0t,format="f",digits=3)),
                   CI90_upper=c( formatC(upperAUC0t,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnAUC0t)
CI90_lower <- lowerAUC0t
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for lnAUC0t =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
}
cat("---------------------------------------------------------------------------\n")
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnAUC0t ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = TotalData)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  }
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnAUC0t<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnAUC0t,format="f",digits=4),formatC(TU_lnAUC0t,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnAUC0t,format="f",digits=4),formatC(PTU_lnAUC0t,format="f",digits=4))) 
colnames(TOST_lnAUC0t)<- c("TOST","  T value","  P value")
show(TOST_lnAUC0t) 
cat("\n")
if(PTL_lnAUC0t >= 0.05 || PTU_lnAUC0t >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
if(multiple){
cat("          P value =",formatC(EP_lnAUCtau_ss,digits=6),"\n") 
cat("\n")
if(EP_lnAUCtau_ss >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2)
}
}
else{
cat("          P value =",formatC(EP_lnAUC0t,digits=6),"\n") 
cat("\n")
if(EP_lnAUC0t >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
}
cat("---------------------------------------------------------------------------\n")
cat("\n")
### for ln(pAUC)
if(pAUC){
if(parallel){
cat("  Summary Report: - Pivotal Parameters of Parallel BE Study -             \n")
 }
else{ 
cat("  Summary Report: - Pivotal Parameters of Replicate BE Study -            \n")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: log(pAUC)                                           \n")
cat("-------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnpAUC_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnpAUC_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnpAUC, "\n")
cat("         MEAN-test =",test_lnpAUC, "\n")
cat("                SE =",SE_lnpAUC, "\n")
cat("Estimate(test-ref) =",SlnpAUC, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnpAUC **************\n")
cat("\n")
output<-data.frame(Point_estimate=c(formatC(100*exp(SlnpAUC),format="f",digits=3)),
                   CI90_lower=c(formatC(lowerlnpAUC,format="f",digits=3)),
                   CI90_upper=c(formatC(upperlnpAUC,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnpAUC)
CI90_lower <- lowerlnpAUC
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for lnpAUC =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
}
cat("---------------------------------------------------------------------------\n")
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnpAUC ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = TotalData)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  cat("\n")
  }
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnpAUC<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnpAUC,format="f",digits=4),formatC(TU_lnpAUC,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnpAUC,format="f",digits=4),formatC(PTU_lnpAUC,format="f",digits=4))) 
colnames(TOST_lnpAUC)<- c("TOST","  T value","  P value")
show(TOST_lnpAUC) 
cat("\n")
if(PTL_lnpAUC >= 0.05 || PTU_lnpAUC >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnpAUC,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnpAUC >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)  ### same as lnAUC0INF
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)  ### same as lnAUC0INF
}
cat("---------------------------------------------------------------------------\n")
cat("\n")
}
### end of ln(pAUC)
if(multiple){
}
else{
if(parallel){
cat("  Summary Report: - Pivotal Parameters of Parallel BE Study -             \n")
 }
else{ 
cat("  Summary Report: - Pivotal Parameters of Replicate BE Study -            \n")
} 
cat("-------------------------------------------------\n")
cat("  Dependent Variable: log(AUC0INF)                                        \n")
cat("-------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0INF_theta1*100,format="f",digits=3), "%\n")
cat("    Upper criteria =",formatC(lnAUC0INF_theta2*100,format="f",digits=3), "%\n")
cat("          MEAN-ref =",ref_lnAUC0INF, "\n")
cat("         MEAN-test =",test_lnAUC0INF, "\n")
cat("                SE =",SE_lnAUC0INF, "\n")
cat("Estimate(test-ref) =",SlnAUC0INF, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0INF **************\n")
cat("\n")
output<-data.frame(Point_estimate=c( formatC(100*exp(SlnAUC0INF),format="f",digits=3)),
                   CI90_lower=c( formatC(lowerAUC0INF,format="f",digits=3)),
                   CI90_upper=c( formatC(upperAUC0INF,format="f",digits=3)))
dimnames(output) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
show(output)
if(!parallel){
Point_estimate <- 100*exp(SlnAUC0INF)
CI90_lower <- lowerAUC0INF
delta_CI <- log(Point_estimate)-log(CI90_lower)
MSE <- 2*(delta_CI/((sqrt(1/L1+1/L2)*qt(0.95, L1+L2-2))))^2
CVintra <- 100*sqrt(exp(MSE)-1)
cat("\n")
cat(" The estimated intra-subject CV for lnAUC0INF =",formatC(CVintra, format="f", digits=5),"%\n")
cat(" CV(intra)% = 100*sqrt(exp(MSE)-1)), where MSE =",formatC(MSE, format="f", digits=5),"\n")
}
cat("---------------------------------------------------------------------------\n")
## Welch t-test starts here if parallel
  if(parallel){
  result <- t.test(lnAUC0INF ~ drug, paired=FALSE, var.equal = FALSE, 
                        conf.level = 0.90, data = TotalData)
  show(result)
  cat("T/R [%] with alpha 0.05 (90% CI)", sep=" ", fill=TRUE)
  tbldiff <- matrix(
  c(as.numeric(exp(diff(result$estimate))),
  sort(as.numeric(exp(-result$conf.int)))),
  byrow = TRUE, nrow = 1)
  dimnames(tbldiff) <- list("Ratio",
                          c("  Point Estimate",
                            "  CI90 lower",
                            "  CI90 upper" ))
  show(round(tbldiff*100,3))
  cat("\n")
}
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnAUC0INF<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnAUC0INF,format="f",digits=4),formatC(TU_lnAUC0INF,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnAUC0INF,format="f",digits=4),formatC(PTU_lnAUC0INF,format="f",digits=4))) 
colnames(TOST_lnAUC0INF)<- c("TOST","  T value","  P value")
show(TOST_lnAUC0INF) 
cat("\n")
if(PTL_lnAUC0INF >= 0.05 || PTU_lnAUC0INF >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnAUC0INF,format="f",digits=6),"\n") 
cat("\n")
if(EP_lnAUC0INF >= 0.05){
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
  }
}
if(parallel){
cat("\n")
cat("Codes for the Welch t-test were kindly provided by Helmut Schutz.   \n")
cat("--------------------------------------------------------------------\n")
cat("Ref.: \n")
cat("1. Helmut Schutz: http://forum.bebac.at/mix_entry.php?id=674\n")
cat("2. Helmut Schutz: http://forum.bebac.at/mix_entry.php?id=5126\n")
cat("3. Wang H and S-C Chow, A practical approach for comparing means of \n")
cat("   two groups without equal variance assumption, Statist. Med. 21:  \n")
cat("   3137-3151 (2002).\n")
cat("--------------------------------------------------------------------\n\n")
}
cat("---------------------------------------------------------------------------\n")
cat("Ref.:                                                                      \n")
cat("1. Chow SC and Liu JP. Design and Analysis of Bioavailability-             \n")
cat("   Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).   \n")
cat("2. Schuirmann DJ. On hypothesis testing to determine if the mean of a      \n")
cat("   normal distribution is continued in a known interval. Biometrics, 37,   \n")
cat("   617(1981).                                                              \n")
cat("3. Schuirmann DJ. A comparison of the two one-sided tests procedure and the\n")
cat("   power approach for assessing the equivalence of average bioavailability.\n")
cat("   Journal of Pharmacokinetics and Biopharmaceutics, 15, 657-680 (1987).   \n")
cat("4. Anderson S and Hauck WW.  A new procedure for testing equivalence in    \n")
cat("   comparative bioavailability and other clinical trials. Communications   \n")
cat("   in Statistics-Theory and Methods, 12, 2663-2692 (1983).                 \n")
cat("-------------------------------------------------------------------------- \n")
cat("\n\n")
}