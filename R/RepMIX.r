#replicated study
RepMIX<-function(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF, 
                 lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
                 parallel=FALSE, multiple=FALSE)
{
cat("\n")
##Cmax/Cmax_ss
if(parallel){
   if(multiple){
    cat("*** This is a 2-treatment parallel multiple-dosed study. \n")
   Data<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),Cmax_ss=TotalData$Cmax, 
                    AUCtau_ss=TotalData$AUC0t, lnCmax_ss=TotalData$lnCmax,lnAUCtau_ss=TotalData$lnAUC0t) 
   }
   else{
    cat("*** This is a 2-treatment parallel single-dosed study. \n")
   }
 }
else{ 
prdcount<-length(levels(TotalData$prd)) #count periods
cat("*** This is a 2-treatment, 2-sequence, and ",prdcount,"-period replicated design. \n") 
} 
cat("--------------------------------------------------------------------------\n")
cat("\n")
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
   if(multiple){
    modCmax_ss<-lm(Cmax_ss ~  drug, data=Data)
   }
   else{
    modCmax<-lm(Cmax ~ drug, data=TotalData)
   }
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
  modCmax<-lme(Cmax ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: Cmax_ss                                                 \n")
cat("\n")
print(summary(modCmax_ss))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modCmax_ss))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modCmax_ss, type="marginal"))
}
}
else{
cat("  Dependent Variable: Cmax                                                 \n")
cat("\n")
print(summary(modCmax))
if(! parallel){
  cat("\n")
  cat("Type I Tests of Fixed Effects\n")
  print(anova(modCmax))
  cat("\n")
  cat("Type III Tests of Fixed Effects\n")
  print(anova(modCmax, type="marginal"))
}
}                                            
cat("\n")
cat("\n")
##AUC0t/AUC(tau)ss
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
  if(multiple){
   modAUCtau_ss<-lm(AUCtau_ss ~ drug, data=Data) 
  }
  else{
   modAUC0t<-lm(AUC0t ~ drug, data=TotalData)
  }
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modAUC0t<-lme(AUC0t ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: AUC(tau)ss                                        \n")
cat("\n")
print(summary(modAUCtau_ss))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modAUCtau_ss))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modAUCtau_ss, type="marginal"))
}
}
else{
cat("  Dependent Variable: AUC0t                                                \n")
cat("\n")
print(summary(modAUC0t))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modAUC0t))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modAUC0t, type="marginal"))
cat("\n")
cat("\n")
}
}        

##AUC0-inf
if(multiple){
}
else{
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
   modAUC0INF<-lm(AUC0INF ~ drug, data=TotalData)
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modAUC0INF<-lme(AUC0INF ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: AUC0INF                                                 \n")
cat("\n")
print(summary(modAUC0INF))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modAUC0INF))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modAUC0INF, type="marginal"))
cat("\n")
cat("\n")
}
}

##lnCmax /lnCmax_ss
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
  if(multiple){
   modlnCmax_ss<-lm(lnCmax_ss ~ drug, data=Data)
  }
  else{
    modlnCmax<-lm(lnCmax ~ drug, data=TotalData)
###    fm1<-lme(lnCmax ~ drug , random=~1|subj, data=TotalData, method="REML")
   }
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modlnCmax<-lme(lnCmax ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: lnCmax_ss                                           \n")
cat("\n")
print(summary(modlnCmax_ss))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modlnCmax_ss))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modlnCmax_ss, type="marginal"))
}
}
else{
cat("  Dependent Variable: lnCmax                                              \n")
cat("\n")
print(summary(modlnCmax))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modlnCmax))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modlnCmax, type="marginal"))
cat("\n")
cat("\n")
}
}

## lnAUC0t/lnAUC(tau)ss 
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
  if(multiple){
     modlnAUCtau_ss<-lm(lnAUCtau_ss ~ drug, data=Data )
  }
  else{
     modlnAUC0t<-lm(lnAUC0t ~ drug, data=TotalData)
   }
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modlnAUC0t<-lme(lnAUC0t ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: lnAUC(tau)ss                                      \n")
cat("\n")
print(summary(modlnAUCtau_ss))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modlnAUCtau_ss))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modlnAUCtau_ss, type="marginal"))
}
}
else{
cat("  Dependent Variable: lnAUC0t                                             \n")       
cat("\n")
print(summary(modlnAUC0t))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modlnAUC0t))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modlnAUC0t, type="marginal"))
cat("\n")
cat("\n")
}
}


##lnAUC0INF
if(multiple){
}
else{
if(parallel){
cat("  Statistical analysis (lm) - parallel BE study               \n")
 modlnAUC0INF<-lm(lnAUC0INF ~drug, data=TotalData)
 }
else{ 
cat("  Statistical analysis (lme) - replicate BE study               \n")
modlnAUC0INF<-lme(lnAUC0INF ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
} 
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0INF                                                 \n")
cat("\n")
print(summary(modlnAUC0INF))
cat("\n")
if(! parallel){
cat("Type I Tests of Fixed Effects\n")
print(anova(modlnAUC0INF))
cat("\n")
cat("Type III Tests of Fixed Effects\n")
print(anova(modlnAUC0INF, type="marginal"))
cat("\n")
cat("\n")
}
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
upperCmax<-100*exp(summary(modlnCmax)$coefficients[2,1])*exp(qt(0.95,summary(modlnCmax)$df[2])*summary(modlnCmax)$coefficients[2,2]) 
lowerCmax<-100*exp(summary(modlnCmax)$coefficients[2,1])*exp(-qt(0.95,summary(modlnCmax)$df[2])*summary(modlnCmax)$coefficients[2,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)$coefficients[2,1])*exp(qt(0.95,summary(modlnAUC0t)$df[2])*summary(modlnAUC0t)$coefficients[2,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)$coefficients[2,1])*exp(-qt(0.95,summary(modlnAUC0t)$df[2])*summary(modlnAUC0t)$coefficients[2,2])

SlnCmax<-(summary(modlnCmax)$coefficients[2,1])
SlnAUC0t<-(summary(modlnAUC0t)$coefficients[2,1])
 
SE_lnCmax<-summary(modlnCmax)$coefficients[2,2]
SE_lnAUC0t<-summary(modlnAUC0t)$coefficients[2,2]
  
  if(multiple){
  }
  else{  
  upperAUC0INF<-100*exp(summary(modlnAUC0INF)$coefficients[2,1])*exp(qt(0.95,summary(modlnAUC0INF)$df[2])*summary(modlnAUC0INF)$coefficients[2,2])
  lowerAUC0INF<-100*exp(summary(modlnAUC0INF)$coefficients[2,1])*exp(-qt(0.95,summary(modlnAUC0INF)$df[2])*summary(modlnAUC0INF)$coefficients[2,2])
  SlnAUC0INF<-(summary(modlnAUC0INF)$coefficients[2,1]) 
  SE_lnAUC0INF<-summary(modlnAUC0INF)$coefficients[2,2]
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
  }
}  

####two-one side and Anderson and Hauck's test
#lnCmax
TL_lnCmax<--(SlnCmax-log(lnCmax_theta1))/SE_lnCmax
TU_lnCmax<-(SlnCmax-log(lnCmax_theta2))/SE_lnCmax
PTL_lnCmax<-pt(TL_lnCmax,L1+L2-2)
PTU_lnCmax<-pt(TU_lnCmax,L1+L2-2)
TAH_lnCmax<-SlnCmax/SE_lnCmax
NP_lnCmax<-log(lnCmax_theta2)/SE_lnCmax
EP_lnCmax<- pt((abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2) - pt((-abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2)

#lnAUC0t
TL_lnAUC0t<--(SlnAUC0t-log(lnAUC0t_theta1))/SE_lnAUC0t
TU_lnAUC0t<-(SlnAUC0t-log(lnAUC0t_theta2))/SE_lnAUC0t
PTL_lnAUC0t<-pt(TL_lnAUC0t,L1+L2-2)
PTU_lnAUC0t<-pt(TU_lnAUC0t,L1+L2-2)
TAH_lnAUC0t<-SlnAUC0t/SE_lnAUC0t
NP_lnAUC0t<-log(lnAUC0t_theta2)/SE_lnAUC0t
EP_lnAUC0t<- pt((abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2) - pt((-abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2)

if(multiple){
}
else{
#lnAUC0INF
TL_lnAUC0INF<--(SlnAUC0INF-log(lnAUC0INF_theta1))/SE_lnAUC0INF
TU_lnAUC0INF<-(SlnAUC0INF-log(lnAUC0INF_theta2))/SE_lnAUC0INF
PTL_lnAUC0INF<-pt(TL_lnAUC0INF,L1+L2-2)
PTU_lnAUC0INF<-pt(TU_lnAUC0INF,L1+L2-2)
TAH_lnAUC0INF<-SlnAUC0INF/SE_lnAUC0INF
NP_lnAUC0INF<-log(lnAUC0INF_theta2)/SE_lnAUC0INF
EP_lnAUC0INF<- pt((abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2) - pt((-abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2)  
}

if(parallel){
cat("  Pivotal Parameters of BE Study - Summary Report  - parallel BE study                         \n")
 }
else{ 
cat("  Pivotal Parameters of BE Study - Summary Report  - replicate BE study                        \n")
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: lnCmax_ss                                            \n")
}
else{
cat("  Dependent Variable: lnCmax                                               \n")
}
cat("--------------------------------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnCmax_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnCmax_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_lnCmax, "\n")
cat("         MEAN-test =",test_lnCmax, "\n")
cat("                SE =",SE_lnCmax, "\n")
cat("Estimate(test-ref) =",SlnCmax, "\n")
cat("\n")
if(multiple){
cat("************** Classical (Shortest) 90% C.I. for lnCmax_ss ****************\n")
}
else{
cat("**************** Classical (Shortest) 90% C.I. for lnCmax ****************\n")
}
cat("\n")
output<-data.frame(CI90_lower=c( formatC(lowerCmax,format="f",digits=3)),
                   Point_estimated=c( formatC(100*exp(SlnCmax),format="f",digits=3)),
                   CI90_upper=c( formatC(upperCmax,format="f",digits=3)))
show(output)                              
cat("\n")
cat("---------------------- Two One-Sided Tests (TOST) -------------------------\n")
cat("\n")
TOST_lnCmax<-data.frame(TOST=c("T_lower", "T_upper"),
                 T_value=c(formatC(TL_lnCmax,format="f",digits=4),formatC(TU_lnCmax,format="f",digits=4)),
                 P_value=c(formatC(PTL_lnCmax,format="f",digits=4),formatC(PTU_lnCmax,format="f",digits=4))) 
colnames(TOST_lnCmax)<- c("TOST","  T value","  P value")
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
cat("---------------------------------------------------------------------------\n")
cat("Ref.:\n")
cat("1. Chow SC and Liu JP. Design and Analysis of Bioavailability-       \n")
cat("   Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).\n")
cat("2. Schuirmann DJ. On hypothesis testing to determine if the mean of a  \n")
cat("   normal distribution is continued in a known interval. Biometrics, 37, \n")
cat("   617(1981).                                                           \n")
cat("3. Schuirmann DJ. A comparison of the two one-sided tests procedure and the \n")
cat("   power approach for assessing the equivalence of average bioavailability.\n")
cat("   Journal of Pharmacokinetics and Biopharmaceutics, 15, 657-680 (1987). \n")
cat("4. Anderson S and Hauck WW.  A new procedure for testing equivalence in \n")
cat("   comparative bioavailability and other clinical trials. Communications \n")
cat("   in Statistics-Theory and Methods, 12, 2663-2692 (1983).     \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
cat("\n")

if(parallel){
cat("  Pivotal  Parameters of BE Study - Summary Report  - parallel BE study    \n")
 }
else{ 
cat("  Pivotal  Parameters of BE Study - Summary Report  - replicate BE study   \n")
} 
cat("--------------------------------------------------------------------------\n")
if(multiple){
cat("  Dependent Variable: lnAUC(tau)ss                                       \n")
}
else{
cat("  Dependent Variable: lnAUC0t                                              \n")
}                                         
cat("--------------------------------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0t_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnAUC0t_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_lnAUC0t, "\n")
cat("         MEAN-test =",test_lnAUC0t, "\n")
cat("                SE =",SE_lnAUC0t, "\n")
cat("Estimate(test-ref) =",SlnAUC0t, "\n")
cat("\n")
if(multiple){
cat("*********** Classical (Shortest) 90% C.I. for lnAUC(tau)ss **************\n")
}
else{
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0t ****************\n")
}
cat("\n")
output<-data.frame(CI90_lower=c( formatC(lowerAUC0t,format="f",digits=3)),
                   Point_estimated=c( formatC(100*exp(SlnAUC0t),format="f",digits=3)),
                   CI90_upper=c( formatC(upperAUC0t,format="f",digits=3)))
show(output)    
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
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("\n")
cat("------------------------ Anderson-Hauck Test ------------------------------\n")
cat("\n")
cat("          P value =",formatC(EP_lnAUC0t,digits=6),"\n") 
cat("\n")
if(EP_lnAUC0t >= 0.05){
description_TOST_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0t(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("---------------------------------------------------------------------------\n")
cat("\n")
cat("\n")

if(multiple){
}
else{
if(parallel){
cat("  Pivotal Parameters of BE Study - Summary Report  - parallel BE study                         \n")
 }
else{ 
cat("  Pivotal Parameters of BE Study - Summary Report  - replicate BE study                        \n")
} 
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0INF                                             \n")
cat("--------------------------------------------------------------------------\n")
if(parallel){
cat("         n1(drug 1)=",L1 , "\n")
cat("         n2(drug 2)=",L2 , "\n")
}
else{
cat("          n1(seq 1)=",L1 , "\n")
cat("          n2(seq 2)=",L2 , "\n")
}
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0INF_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnAUC0INF_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_lnAUC0INF, "\n")
cat("         MEAN-test =",test_lnAUC0INF, "\n")
cat("                SE =",SE_lnAUC0INF, "\n")
cat("Estimate(test-ref) =",SlnAUC0INF, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0INF **************\n")
cat("\n")
output<-data.frame(CI90_lower=c( formatC(lowerAUC0INF,format="f",digits=3)),
                   Point_estimated=c( formatC(100*exp(SlnAUC0INF),format="f",digits=3)),
                   CI90_upper=c( formatC(upperAUC0INF,format="f",digits=3)))
show(output)   
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
description_TOST_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
else{
description_TOST1_lnAUC0INF(lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2 )
}
cat("\n")
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
cat("---------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
  }
}