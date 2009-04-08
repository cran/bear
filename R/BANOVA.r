##ANOVA
library(ICSNP) 
BANOVA<-function(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2)
{
#theta1:  lower acceptance limit
#theta1:  lower acceptance limit
#represent GLM
cat("  Statistical analysis (ANOVA(lm))                  \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: Cmax                                                 \n")
cat("\n")
cat("Type I SS\n")
Cmax<- lm(Cmax ~ seq + subj:seq + prd + drug , data=TotalData)
show(anova(Cmax))
cat("\n")
cat("Type III SS\n")
show(drop1(Cmax, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(Cmax ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#GLM_AUC0t.txt
cat("  Statistical analysis (ANOVA(lm))                   \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: AUC0t                                                \n")
cat("\n")
cat("Type I SS\n")
AUC0t<- lm(AUC0t ~ seq + subj:seq+ prd + drug , data=TotalData)
show(anova(AUC0t))
cat("\n")
cat("Type III SS\n")
show(drop1(AUC0t, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(AUC0t ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#GLM_AUC0INF.txt
cat("  Statistical analysis (ANOVA(lm), 90%CI, Outlier Detection, and etc.)    \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: AUC0INF                                             \n")
cat("\n")
cat("Type I SS\n")
AUC0INF<- lm(AUC0INF ~ seq + subj:seq + prd + drug , data=TotalData)
show(anova(AUC0INF))
cat("\n")
cat("Type III SS\n")
show(drop1(AUC0INF, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(AUC0INF ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("\n")
cat("\n")

#GLM_lnCmax.txt
cat("  Statistical analysis (ANOVA(lm))    \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnCmax                                               \n")
cat("\n")
cat("Type I SS\n")
lnCmax<- lm(lnCmax ~ seq + subj:seq + prd + drug , data=TotalData)
show(anova(lnCmax))
cat("\n")
cat("Type III SS\n")
show(drop1(lnCmax, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(lnCmax ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("Intra_subj. CV = 100*sqrt(abs(exp(MSResidual)-1)) =",formatC(100*sqrt(abs(exp(anova(lnCmax)[5,3])-1)),format="f",digits=4),"%\n")
cat("Inter_subj. CV = 100*sqrt(abs(exp((MSSubject(seq)-MSResidual)/2)-1)) =",formatC(100*sqrt(abs(exp((anova(lnCmax)[4,3]-anova(lnCmax)[5,3])/2)-1)),format="f",digits=4),"%\n")
cat("    MSResidual =",anova(lnCmax)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnCmax)[4,3],"\n")
cat("\n")
cat("\n")

#GLM_lnAUC0t.txt
cat("  Statistical analysis (ANOVA(lm), 90%CI, Outlier Detection, and etc.)    \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0t                                               \n")
cat("\n")
cat("Type I SS\n")
lnAUC0t<- lm(lnAUC0t ~ seq + subj:seq + prd + drug , data=TotalData)
show(anova(lnAUC0t))
cat("\n")
cat("Type III SS\n")
show(drop1(lnAUC0t, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(lnAUC0t ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("Intra_subj. CV = 100*sqrt(abs(exp(MSResidual)-1)) =", formatC(100*sqrt(abs(exp(anova(lnAUC0t)[5,3])-1)),format="f",digits=4),"%\n")
cat("Inter_subj. CV = 100*sqrt(abs(exp((MSSubject(seq)-MSResidual)/2)-1)) =",formatC(100*sqrt(abs(exp((anova(lnAUC0t)[4,3]-anova(lnAUC0t)[5,3])/2)-1)),format="f",digits=4),"%\n")
cat("    MSResidual =",anova(lnAUC0t)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnAUC0t)[4,3],"\n")
cat("\n")
cat("\n")

#GLM_AUC0INF.txt
cat("  Statistical analysis (ANOVA(lm))    \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0INF                                             \n")
cat("\n")
cat("Type I SS\n")
lnAUC0INF<- lm(lnAUC0INF ~ seq + subj:seq + prd + drug , data=TotalData)
show(anova(lnAUC0INF))
cat("\n")
cat("Type III SS\n")
show(drop1(lnAUC0INF, test="F"))
cat("\n")
cat("Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
print(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) , data=TotalData)))
cat("\n")
cat("Intra_subj. CV = 100*sqrt(abs(exp(MSResidual)-1)) =",formatC(100*sqrt(abs(exp(anova(lnAUC0INF)[5,3])-1)),format="f",digits=4),"%\n")
cat("Inter_subj. CV = 100*sqrt(abs(exp((MSSubject(seq)-MSResidual)/2)-1)) =",formatC(100*sqrt(abs(exp((anova(lnAUC0INF)[4,3]-anova(lnAUC0INF)[5,3])/2)-1)),format="f",digits=4),"%\n")
cat("    MSResidual =",anova(lnAUC0INF)[5,3],"\n")
cat("MSSubject(seq) =",anova(lnAUC0INF)[4,3],"\n")     
cat("\n")
cat("\n")

##########################################################################################
###shortest confidence interval 
#L1(Reference-->Test),L2(Test-->Reference sequence)
Todata<-split(TotalData, list(TotalData$prd,TotalData$seq))
T<-qt(0.95,(L1+L2-2))

ref_Cmax<-mean(RefData$lnCmax)
ref_AUC0t<-mean(RefData$lnAUC0t)
ref_AUC0INF<-mean(RefData$lnAUC0INF)

test_Cmax<-mean(TestData$lnCmax)
test_AUC0t<-mean(TestData$lnAUC0t)
test_AUC0INF<-mean(TestData$lnAUC0INF)

SE_Cmax<-sqrt((anova(lnCmax)[5,3]/2) * (1/L1+1/L2))
SE_AUC0t<-sqrt((anova(lnAUC0t)[5,3]/2) * (1/L1+1/L2))
SE_AUC0INF<-sqrt((anova(lnAUC0INF)[5,3]/2) * (1/L1+1/L2))

est_lnCmax<-lnCmax$coef[[4]] 
est_lnAUC0t<-lnAUC0t$coef[[4]] 
est_lnAUC0INF<-lnAUC0INF$coef[[4]] 

lowerCmax<-100*exp(est_lnCmax-(T*SE_Cmax))
upperCmax<-100*exp(est_lnCmax+(T*SE_Cmax))
lowerAUC0t<-100*exp(est_lnAUC0t-(T*SE_AUC0t))
UpperAUC0t<-100*exp(est_lnAUC0t+(T*SE_AUC0t))
LowerAUC0INF<-100*exp(est_lnAUC0INF-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp(est_lnAUC0INF+(T*SE_AUC0INF))

###two-one side and Anderson and Hauck's test
#lnCmax
TL_lnCmax<--((test_Cmax-ref_Cmax)-log(lnCmax_theta1))/SE_Cmax
TU_lnCmax<-((test_Cmax-ref_Cmax)-log(lnCmax_theta2))/SE_Cmax
PTL_lnCmax<-pt(TL_lnCmax,L1+L2-2)
PTU_lnCmax<-pt(TU_lnCmax,L1+L2-2)
TAH_lnCmax<-(test_Cmax-ref_Cmax)/SE_Cmax
NP_lnCmax<-log(lnCmax_theta2)/SE_Cmax
EP_lnCmax<- pt((abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2) - pt((-abs(TAH_lnCmax)-NP_lnCmax),L1+L2-2)

#lnAUC0t
TL_lnAUC0t<--((test_AUC0t-ref_AUC0t)-log(lnAUC0t_theta1))/SE_AUC0t
TU_lnAUC0t<-((test_AUC0t-ref_AUC0t)-log(lnAUC0t_theta2))/SE_AUC0t
PTL_lnAUC0t<-pt(TL_lnAUC0t,L1+L2-2)
PTU_lnAUC0t<-pt(TU_lnAUC0t,L1+L2-2)
TAH_lnAUC0t<-(test_AUC0t-ref_AUC0t)/SE_AUC0t
NP_lnAUC0t<-log(lnAUC0t_theta2)/SE_AUC0t
EP_lnAUC0t<- pt((abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2) - pt((-abs(TAH_lnAUC0t)-NP_lnAUC0t),L1+L2-2)

#lnAUC0INF
TL_lnAUC0INF<--((test_AUC0INF-ref_AUC0INF)-log(lnAUC0INF_theta1))/SE_AUC0INF
TU_lnAUC0INF<-((test_AUC0INF-ref_AUC0INF)-log(lnAUC0INF_theta2))/SE_AUC0INF
PTL_lnAUC0INF<-pt(TL_lnAUC0INF,L1+L2-2)
PTU_lnAUC0INF<-pt(TU_lnAUC0INF,L1+L2-2)
TAH_lnAUC0INF<-(test_AUC0INF-ref_AUC0INF)/SE_AUC0INF
NP_lnAUC0INF<-log(lnAUC0INF_theta2)/SE_AUC0INF
EP_lnAUC0INF<- pt((abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2) - pt((-abs(TAH_lnAUC0INF)-NP_lnAUC0INF),L1+L2-2)

##############################################################################
Prddata<-split(TotalData, list(TotalData$prd))
 
#chi-square distribution
chinv_1<-qchisq(0.025, df= (L1+L2-2))
chinv_2<-qchisq(0.975, df= (L1+L2-2))
#F distribution
F_1<-qf(0.025,L1+L2-2,L1+L2-2)
F_2<-qf(0.975,L1+L2-2,L1+L2-2)

Prddata1<-Prddata[[1]][ do.call(order, Prddata[[1]]) ,]
Prddata2<-Prddata[[2]][ do.call(order, Prddata[[2]]) ,]
seqdata1<-split(Prddata1, list(Prddata1$seq))
seqdata2<-split(Prddata2, list(Prddata2$seq))

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
lnCmax_CD<-prelnCmax_CD[ do.call(order, prelnCmax_CD) ,]
lnCmax_CDmean<- mean(lnCmax_CD$lnCmax_CD)


lnCmax_TotalData<-data.frame(subj=Prddata1$subj, Total=lnCmax_Total, U=lnCmax_Total-lnCmax_Totalmean, U2=(lnCmax_Total-lnCmax_Totalmean)^2,
                             CD=lnCmax_CD$lnCmax_CD, V=lnCmax_CD$lnCmax_CD-lnCmax_CDmean, V2=(lnCmax_CD$lnCmax_CD-lnCmax_CDmean)^2,
                             UV=(lnCmax_Total-lnCmax_Totalmean)*(lnCmax_CD$lnCmax_CD-lnCmax_CDmean)) 

lnCmax_Svv<-sum(lnCmax_TotalData$V2)*(1/(L1+L2-1))
lnCmax_Suu<-sum(lnCmax_TotalData$U2)*(1/(L1+L2-1))
lnCmax_Svu<-sum(lnCmax_TotalData$UV)*(1/(L1+L2-1))

lnCmax_Srr<-sum((RefData$lnCmax - ref_Cmax)^2 )*(1/(L1+L2-1))
lnCmax_Stt<-sum((TestData$lnCmax - test_Cmax)^2 )*(1/(L1+L2-1))
lnCmax_Srt<-sum((RefData$lnCmax - ref_Cmax)*(TestData$lnCmax - test_Cmax))*(1/(L1+L2-1))

lnCmax_pearson_P<-cor.test(lnCmax_TotalData$V, lnCmax_TotalData$U, method=c("pearson"))[[3]]
lnCmax_pearson_V<-cor.test(lnCmax_TotalData$V, lnCmax_TotalData$U, method=c("pearson"))[[4]][[1]]
lnCmax_spearman_P<-cor.test(lnCmax_TotalData$V, lnCmax_TotalData$U, method=c("spearman"))[[3]]
lnCmax_spearman_V<-cor.test(lnCmax_TotalData$V, lnCmax_TotalData$U, method=c("spearman"))[[4]][[1]]

lnCmax_Frt<-lnCmax_Stt/lnCmax_Srr
lnCmax_rrt<-lnCmax_Srt/sqrt(lnCmax_Srr*lnCmax_Stt)

lnCmax_Fpm<-((L1+L2-2)*(lnCmax_Frt-1)^2 )/(4*(lnCmax_Frt)*(1-(lnCmax_rrt)^2))
lnCmax_PFpm<-1-pf(lnCmax_Fpm, 1, L1+L2-2)
           
#Report for lnCmax, lnAUC0t and lnAUC0inf
cat("\n")
cat("\n")
cat("  Pivotal Parameters of BE Study - Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnCmax                                               \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 , "\n")
cat("        n2(T -> R) =",L2 , "\n")
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnCmax_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnCmax_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_Cmax, "\n")
cat("         MEAN-test =",test_Cmax, "\n")
cat("               MSE =",anova(lnCmax)[5,3], "\n")
cat("                SE =",SE_Cmax, "\n")
cat("Estimate(test-ref) =",est_lnCmax, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnCmax ****************\n")
cat("\n")
output<-data.frame(CI90_lower=c(formatC(lowerCmax,format="f",digits=3)),
                   Point_estimated=c(formatC(100*exp(est_lnCmax),format="f",digits=3)),
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
cat("\n")
cat("  Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
II_lnCmax<-data.frame(subj=IntraInterlnCmax00$subj,
                      Obs=formatC(IntraInterlnCmax00$Obs,format="f",digits=6), 
                      Exp=formatC(IntraInterlnCmax00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnCmax00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnCmax00$Stud_Intra,format="f",digits=6), 
                      Inter=formatC(IntraInterlnCmax00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnCmax00$Stud_Inter,format="f",digits=6))   
show(II_lnCmax)
cat("------------------------------------------------\n")
cat("Obs: Observed lnCmax\n")
cat("Exp: Expected lnCmax\n")
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
lnAUC0t_CD<-prelnAUC0t_CD[ do.call(order, prelnAUC0t_CD) ,]
lnAUC0t_CDmean<- mean(lnAUC0t_CD$lnAUC0t_CD)


lnAUC0t_TotalData<-data.frame(subj=Prddata1$subj, Total=lnAUC0t_Total, U=lnAUC0t_Total-lnAUC0t_Totalmean, U2=(lnAUC0t_Total-lnAUC0t_Totalmean)^2,
                             CD=lnAUC0t_CD$lnAUC0t_CD, V=lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean, V2=(lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean)^2,
                             UV=(lnAUC0t_Total-lnAUC0t_Totalmean)*(lnAUC0t_CD$lnAUC0t_CD-lnAUC0t_CDmean))

lnAUC0t_Svv<-sum(lnAUC0t_TotalData$V2)*(1/(L1+L2-1))
lnAUC0t_Suu<-sum(lnAUC0t_TotalData$U2)*(1/(L1+L2-1))
lnAUC0t_Svu<-sum(lnAUC0t_TotalData$UV)*(1/(L1+L2-1))

lnAUC0t_Srr<-sum((RefData$lnAUC0t - ref_AUC0t)^2 )*(1/(L1+L2-1))
lnAUC0t_Stt<-sum((TestData$lnAUC0t - test_AUC0t)^2 )*(1/(L1+L2-1))
lnAUC0t_Srt<-sum((RefData$lnAUC0t - ref_AUC0t)*(TestData$lnAUC0t - test_AUC0t))*(1/(L1+L2-1))

lnAUC0t_pearson_P<-cor.test(lnAUC0t_TotalData$V, lnAUC0t_TotalData$U, method=c("pearson"))[[3]]
lnAUC0t_pearson_V<-cor.test(lnAUC0t_TotalData$V, lnAUC0t_TotalData$U, method=c("pearson"))[[4]][[1]]
lnAUC0t_spearman_P<-cor.test(lnAUC0t_TotalData$V, lnAUC0t_TotalData$U, method=c("spearman"))[[3]]
lnAUC0t_spearman_V<-cor.test(lnAUC0t_TotalData$V, lnAUC0t_TotalData$U, method=c("spearman"))[[4]][[1]]

lnAUC0t_Frt<-lnAUC0t_Stt/lnAUC0t_Srr
lnAUC0t_rrt<-lnAUC0t_Srt/sqrt(lnAUC0t_Srr*lnAUC0t_Stt)

lnAUC0t_Fpm<-((L1+L2-2)*(lnAUC0t_Frt-1)^2 )/(4*(lnAUC0t_Frt)*(1-(lnAUC0t_rrt)^2))
lnAUC0t_PFpm<-1-pf(lnAUC0t_Fpm, 1, L1+L2-2)

cat("  Pivotal Parameters of BE Study - Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0t                                               \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 , "\n")
cat("        n2(T -> R) =",L2 , "\n")
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0t_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnAUC0t_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_AUC0t, "\n")
cat("         MEAN-test =",test_AUC0t, "\n")
cat("               MSE =",anova(lnAUC0t)[5,3], "\n")
cat("                SE =",SE_AUC0t, "\n")
cat("Estimate(test-ref) =",est_lnAUC0t, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0t ****************\n")
cat("\n")
output<-data.frame(CI90_lower=c( formatC(lowerAUC0t,format="f",digits=3)),
                   Point_estimated=c( formatC(100*exp(est_lnAUC0t),format="f",digits=3)),
                   CI90_upper=c( formatC(UpperAUC0t,format="f",digits=3)))
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
cat("---------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("  Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
II_lnAUC0t<-data.frame(subj=IntraInterlnAUC0t00$subj,
                      Obs=formatC(IntraInterlnAUC0t00$Obs,format="f",digits=6), 
                      Exp=formatC(IntraInterlnAUC0t00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnAUC0t00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnAUC0t00$Stud_Intra,format="f",digits=6), 
                      Inter=formatC(IntraInterlnAUC0t00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnAUC0t00$Stud_Inter,format="f",digits=6))   
show(II_lnAUC0t)
cat("------------------------------------------------\n")
cat("Obs: Observed lnAUC0t\n")
cat("Exp: Expected lnAUC0t\n")
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")

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
lnAUC0INF_CD<-prelnAUC0INF_CD[ do.call(order, prelnAUC0INF_CD) ,]
lnAUC0INF_CDmean<- mean(lnAUC0INF_CD$lnAUC0INF_CD)


lnAUC0INF_TotalData<-data.frame(subj=Prddata1$subj, Total=lnAUC0INF_Total, U=lnAUC0INF_Total-lnAUC0INF_Totalmean, U2=(lnAUC0INF_Total-lnAUC0INF_Totalmean)^2,
                             CD=lnAUC0INF_CD$lnAUC0INF_CD, V=lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean, V2=(lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean)^2,
                             UV=(lnAUC0INF_Total-lnAUC0INF_Totalmean)*(lnAUC0INF_CD$lnAUC0INF_CD-lnAUC0INF_CDmean))

lnAUC0INF_Svv<-sum(lnAUC0INF_TotalData$V2)*(1/(L1+L2-1))
lnAUC0INF_Suu<-sum(lnAUC0INF_TotalData$U2)*(1/(L1+L2-1))
lnAUC0INF_Svu<-sum(lnAUC0INF_TotalData$UV)*(1/(L1+L2-1))

lnAUC0INF_Srr<-sum((RefData$lnAUC0INF - ref_AUC0INF)^2 )*(1/(L1+L2-1))
lnAUC0INF_Stt<-sum((TestData$lnAUC0INF - test_AUC0INF)^2 )*(1/(L1+L2-1))
lnAUC0INF_Srt<-sum((RefData$lnAUC0INF - ref_AUC0INF)*(TestData$lnAUC0INF - test_AUC0INF))*(1/(L1+L2-1))

lnAUC0INF_pearson_P<-cor.test(lnAUC0INF_TotalData$V, lnAUC0INF_TotalData$U, method=c("pearson"))[[3]]
lnAUC0INF_pearson_V<-cor.test(lnAUC0INF_TotalData$V, lnAUC0INF_TotalData$U, method=c("pearson"))[[4]][[1]]
lnAUC0INF_spearman_P<-cor.test(lnAUC0INF_TotalData$V, lnAUC0INF_TotalData$U, method=c("spearman"))[[3]]
lnAUC0INF_spearman_V<-cor.test(lnAUC0INF_TotalData$V, lnAUC0INF_TotalData$U, method=c("spearman"))[[4]][[1]]

lnAUC0INF_Frt<-lnAUC0INF_Stt/lnAUC0INF_Srr
lnAUC0INF_rrt<-lnAUC0INF_Srt/sqrt(lnAUC0INF_Srr*lnAUC0INF_Stt)

lnAUC0INF_Fpm<-((L1+L2-2)*(lnAUC0INF_Frt-1)^2 )/(4*(lnAUC0INF_Frt)*(1-(lnAUC0INF_rrt)^2))
lnAUC0INF_PFpm<-1-pf(lnAUC0INF_Fpm, 1, L1+L2-2)

cat("  Pivotal Parameters of BE Study - Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0INF                                             \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R -> T) =",L1 , "\n")
cat("        n2(T -> R) =",L2 , "\n")
cat("          N(n1+n2) =",L1+L2 , "\n")
cat("    Lower criteria =",formatC(lnAUC0INF_theta1*100,format="f",digits=0), "%\n")
cat("    Upper criteria =",formatC(lnAUC0INF_theta2*100,format="f",digits=0), "%\n")
cat("          MEAN-ref =",ref_AUC0INF, "\n")
cat("         MEAN-test =",test_AUC0INF, "\n")
cat("               MSE =",anova(lnAUC0INF)[5,3], "\n")
cat("                SE =",SE_AUC0INF, "\n")
cat("Estimate(test-ref) =",est_lnAUC0INF, "\n")
cat("\n")
cat("**************** Classical (Shortest) 90% C.I. for lnAUC0INF **************\n")
cat("\n")
output<-data.frame(CI90_lower=c( formatC(LowerAUC0INF,format="f",digits=3)),
                   Point_estimated=c( formatC(100*exp(est_lnAUC0INF),format="f",digits=3)),
                   CI90_upper=c( formatC(UpperAUC0INF,format="f",digits=3)))
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
cat("Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
II_lnAUC0INF<-data.frame(subj=IntraInterlnAUC0INF00$subj,
                      Obs=formatC(IntraInterlnAUC0INF00$Obs,format="f",digits=6), 
                      Exp=formatC(IntraInterlnAUC0INF00$Exp,format="f",digits=6),
                      Intra=formatC(IntraInterlnAUC0INF00$Intra,format="f",digits=6),
                      Stud_Intra=formatC(IntraInterlnAUC0INF00$Stud_Intra,format="f",digits=6), 
                      Inter=formatC(IntraInterlnAUC0INF00$Inter,format="f",digits=6),
                      Stud_Inter=formatC(IntraInterlnAUC0INF00$Stud_Inter,format="f",digits=6))   
show(II_lnAUC0INF)
cat("------------------------------------------------\n")
cat("Obs: Observed lnAUC0INF\n")
cat("Exp: Expected lnAUC0INF\n")
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("**Ref: Chow SC and Liu JP. Design and Analysis of Bioavailability-          \n")
cat("Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).     \n")
cat("---------------------------------------------------------------------\n") 
cat("\n")
cat("\n")

cat("****************************************************************************\n")
cat("Analysis of Outlier Detection\n")
cat("****************************************************************************\n")
cat("\n")
cat("\n")
cat("Test for Normality Assumption  (Shapiro-Wilk)             \n")
cat("--------------------------------------------------------------------------\n")
outputSW<-data.frame(Parameter=c("lnCmax_Stud_Intra","lnCmax_Stud_Inter",
                              "lnAUC0t_Stud_Intra","lnAUC0t_Stud_Inter",
                              "lnAUC0INF_Stud_Intra","lnAUC0INF_Stud_Inter"),
                     Test=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[1]][[1]],format="f",digits=5),
                            formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[1]][[1]],format="f",digits=5),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[1]][[1]],format="f",digits=5)),
                     P_value=c(formatC(shapiro.test(IntraInterlnCmax00$Stud_Intra)[[2]],format="f",digits=4),formatC(shapiro.test(IntraInterlnCmax00$Stud_Inter)[[2]],format="f",digits=4),
                               formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Intra)[[2]],format="f",digits=4),formatC(shapiro.test(IntraInterlnAUC0t00$Stud_Inter)[[2]],format="f",digits=4),
                               formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Intra)[[2]],format="f",digits=4),formatC(shapiro.test(IntraInterlnAUC0INF00$Stud_Inter)[[2]],format="f",digits=4)))
colnames(outputSW)<- c("Parameter","Test","P value")
show(outputSW)
cat("\n")
cat("-------------------------------------------------\n")
cat("Stud_Intra: studentized intra-subject residuals\n")
cat("Stud_Inter: studentized inter-subject residuals\n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat("  The normality of the studentized intra-subject residuals and the \n")
cat("studentized inter-residuals was examined using the test of Shapiro-Wilk. \n")
cat("If a P value is more than 0.05, we will fail to reject the normal         \n")
cat("assumption hypothesis.                                                    \n")
cat("\n")
cat("**Ref: Chow SC and Liu JP. Design and Analysis of Bioavailability-      \n")
cat("Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).\n")
cat("---------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

cat("Test for Normality Assumption  (Pearson)             \n")
cat("--------------------------------------------------------------------------\n")
outputPearson<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4)))
colnames(outputPearson)<- c("Parameter","Test","P value")
show(outputPearson)
cat("\n")
cat("-------------------------------------------------\n")
cat("Pearson: Pearson's correlation coefficient\n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat("  Either Pearson correlation coefficient or Spearman's rank correlation  \n")
cat("coefficient is used to examine the assumption of independence between   \n")
cat("intra- and inter-subject variabilities.  Thus, if a P value is more than \n")
cat("0.05, there is no evidence to suggest that the assumption is not true.   \n")
cat("\n")
cat("**Ref: Chow SC and Liu JP. Design and Analysis of Bioavailability-          \n")
cat("Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).     \n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat("\n")

cat("Test for Normality Assumption  (Spearman)             \n")
cat("--------------------------------------------------------------------------\n")
outputSpearman<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4)))
colnames(outputSpearman)<- c("Parameter","Test","P value")
show(outputSpearman)
cat("\n")
cat("-------------------------------------------------\n")
cat("Spearman: Spearman's rank correlation coefficient\n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

#######################################
##for Hotelling T^2
HotelData<-data.frame(subj=RefData$subj, Ref_Cmax=RefData$Cmax, Test_Cmax=TestData$Cmax,
                                         Ref_lnCmax=RefData$lnCmax,  Test_lnCmax=TestData$lnCmax,
                                         Ref_AUC0t=RefData$AUC0t, Test_AUC0t=TestData$AUC0t,
                                         Ref_lnAUC0t=RefData$lnAUC0t,  Test_lnAUC0t=TestData$lnAUC0t,
                                         Ref_AUC0INF=RefData$AUC0INF, Test_AUC0INF=TestData$AUC0INF,
                                         Ref_lnAUC0INF=RefData$lnAUC0INF,  Test_lnAUC0INF=TestData$lnAUC0INF)

Hotel<-split(HotelData,list(HotelData$subj))

subj1<-NULL
HT_Cmax<-NULL
HT_lnCmax<-NULL
HT_AUC0t<-NULL
HT_lnAUC0t<-NULL
HT_AUC0INF<-NULL
HT_lnAUC0INF<-NULL
HT_Cmax_P<-NULL
HT_lnCmax_P<-NULL
HT_AUC0t_P<-NULL
HT_lnAUC0t_P<-NULL
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
 
 Ref_AUC0INF<-Hotel[[i]]$Ref_AUC0INF
 Test_AUC0INF<-Hotel[[i]]$Test_AUC0INF
 Ref_lnAUC0INF<-Hotel[[i]]$Ref_lnAUC0INF
 Test_lnAUC0INF<-Hotel[[i]]$Test_lnAUC0INF
 
    com<-data.frame(subj=subj, Ref_Cmax=Ref_Cmax, Test_Cmax=Test_Cmax,
                               Ref_lnCmax=Ref_lnCmax,  Test_lnCmax=Test_lnCmax,
                               Ref_AUC0t=Ref_AUC0t, Test_AUC0t=Test_AUC0t,
                               Ref_lnAUC0t=Ref_lnAUC0t,  Test_lnAUC0t=Test_lnAUC0t,
                               Ref_AUC0INF=Ref_AUC0INF, Test_AUC0INF=Test_AUC0INF,
                               Ref_lnAUC0INF=Ref_lnAUC0INF,  Test_lnAUC0INF=Test_lnAUC0INF)

    del<-subset(as.data.frame(HotelData), subj != Hotel[[i]]$subj)
    
    com_Cmax<-data.frame(Ref_Cmax=com$Ref_Cmax, Test_Cmax=com$Test_Cmax) 
    del_Cmax<-data.frame(Ref_Cmax=del$Ref_Cmax, Test_Cmax=del$Test_Cmax) 
     HT_Cmax[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax, test="chi")[[1]][[1]])
     HT_Cmax_P[[i]]<-c(HotellingsT2(com_Cmax,del_Cmax, test="chi")[[2]][[1]])
 
    com_lnCmax<-data.frame(Ref_lnCmax=com$Ref_lnCmax, Test_lnCmax=com$Test_lnCmax) 
    del_lnCmax<-data.frame(Ref_lnCmax=del$Ref_lnCmax, Test_lnCmax=del$Test_lnCmax) 
     HT_lnCmax[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax, test="chi")[[1]][[1]])
     HT_lnCmax_P[[i]]<-c(HotellingsT2(com_lnCmax,del_lnCmax, test="chi")[[2]][[1]])
    
    com_AUC0t<-data.frame(Ref_AUC0t=com$Ref_AUC0t, Test_AUC0t=com$Test_AUC0t)
    del_AUC0t<-data.frame(Ref_AUC0t=del$Ref_AUC0t, Test_AUC0t=del$Test_AUC0t)
     HT_AUC0t[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t, test="chi")[[1]][[1]])                 
     HT_AUC0t_P[[i]]<-c(HotellingsT2(com_AUC0t,del_AUC0t, test="chi")[[2]][[1]])    
                     
    com_lnAUC0t<-data.frame(Ref_lnAUC0t=com$Ref_lnAUC0t, Test_lnAUC0t=com$Test_lnAUC0t)
    del_lnAUC0t<-data.frame(Ref_lnAUC0t=del$Ref_lnAUC0t, Test_lnAUC0t=del$Test_lnAUC0t)
      HT_lnAUC0t[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t, test="chi")[[1]][[1]])  
      HT_lnAUC0t_P[[i]]<-c(HotellingsT2(com_lnAUC0t,del_lnAUC0t, test="chi")[[2]][[1]])      
      
    com_AUC0INF<-data.frame(Ref_AUC0INF=com$Ref_AUC0INF, Test_AUC0INF=com$Test_AUC0INF)
    del_AUC0INF<-data.frame(Ref_AUC0INF=del$Ref_AUC0INF, Test_AUC0INF=del$Test_AUC0INF)
       HT_AUC0INF[[i]]<-c(HotellingsT2(com_AUC0INF,del_AUC0INF, test="chi")[[1]][[1]]) 
       HT_AUC0INF_P[[i]]<-c(HotellingsT2(com_AUC0INF,del_AUC0INF, test="chi")[[2]][[1]]) 
       
    com_lnAUC0INF<-data.frame(Ref_lnAUC0INF=com$Ref_lnAUC0INF, Test_lnAUC0INF=com$Test_lnAUC0INF)
    del_lnAUC0INF<-data.frame(Ref_lnAUC0INF=del$Ref_lnAUC0INF, Test_lnAUC0INF=del$Test_lnAUC0INF)
       HT_lnAUC0INF[[i]]<-c(HotellingsT2(com_lnAUC0INF,del_lnAUC0INF, test="chi")[[1]][[1]]) 
       HT_lnAUC0INF_P[[i]]<-c(HotellingsT2(com_lnAUC0INF,del_lnAUC0INF, test="chi")[[2]][[1]]) 
       #subj1[[i]]<-c(levels(Hotel[[1]]$subj))
   
 }
# show( subj1)
subj1<-c(levels(Hotel[[1]]$subj))  #c((Hotel[[1]]$subj))->c(levels(Hotel[[1]]$subj))
cat("Hotelling T^2 with Chi-square test  \n")
cat("--------------------------------------------------------------------------\n")
HotellingCmax<-data.frame(subj1, formatC(HT_Cmax,format="f",digits=5),    
                          formatC(HT_Cmax_P,format="f",digits=5),
                          formatC(HT_lnCmax,format="f",digits=5),
                          formatC(HT_lnCmax_P,format="f",digits=5))                                   
colnames(HotellingCmax)<- c("subj","Cmax","P value","lnCmax","P value")                                  
                           
show(HotellingCmax) 
cat("\n")

HotellingAUC0t<-data.frame(subj1, formatC(HT_AUC0t,format="f",digits=5), 
                           formatC(HT_AUC0t_P,format="f",digits=5),     
                           formatC(HT_lnAUC0t,format="f",digits=5), 
                           formatC(HT_lnAUC0t_P,format="f",digits=5))
colnames(HotellingAUC0t)<- c("subj","AUC0t","P value","lnAUC0t","P value")        
show(HotellingAUC0t) 
cat("\n")

HotellingAUC0INF<-data.frame(subj1, formatC(HT_AUC0INF,format="f",digits=5),
                              formatC(HT_AUC0INF_P,format="f",digits=5),
                              formatC(HT_lnAUC0INF,format="f",digits=5),
                              formatC(HT_lnAUC0INF_P,format="f",digits=5)) 
colnames(HotellingAUC0INF)<- c("subj","AUC0INF","P value","lnAUC0INF","P value")   
show(HotellingAUC0INF) 
cat("\n")
cat("-------------------------------------------------\n")
cat("**Interpretation: If subjects have relatively BIG T^2 values which \n")
cat(" cause P value less than 0.05, these subject may be outlying subjects. \n")
cat("\n")
cat("Ref.:\n") 
cat("1. Liu JP and Weng CS. Detection of outlying data in bioavailability-\n")
cat("   bioequivalence studies. Statistics in Medicine, 10, 1375-1389(1991).\n")
cat("2. Chow SC and Liu JP. Design and Analysis of Bioavailability-        \n")
cat("   Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).\n")
cat("-------------------------------------------------------------------------\n")
cat("\n") 
cat("\n")
cat("\n")

##summary of point and interval estimation of inter- and intra-subject variability for data
cat("  Test for Equality of Intra-subject variabilities between formulations   \n")
cat("--------------------------------------------------------------------------\n")
outputEquality<-data.frame(Method=c("lnCmax_Pearson","lnCmax_Pitman_Morgan","lnCmax_Spearman",
                                       "lnAUC0t_Pearson","lnAUC0t_Pitman_Morgan","lnAUC0t_Spearman",
                                       "lnAUC0INF_Pearson","lnAUC0INF_Pitman_Morgan","lnAUC0INF_Spearman"),
                           Test=c(formatC(lnCmax_pearson_V,format="f",digits=5),formatC(lnCmax_Fpm,format="f",digits=5),formatC(lnCmax_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0t_pearson_V,format="f",digits=5),formatC(lnAUC0t_Fpm,format="f",digits=5),formatC(lnAUC0t_spearman_V,format="f",digits=5),
                                  formatC(lnAUC0INF_pearson_V,format="f",digits=5),formatC(lnAUC0INF_Fpm,format="f",digits=5),formatC(lnAUC0INF_spearman_V,format="f",digits=5)),            
                           P_value=c(formatC(lnCmax_pearson_P,format="f",digits=4),formatC(lnCmax_PFpm,format="f",digits=4),formatC(lnCmax_spearman_P,format="f",digits=4),
                                     formatC(lnAUC0t_pearson_P,format="f",digits=4),formatC(lnAUC0t_PFpm,format="f",digits=4),formatC(lnAUC0t_spearman_P,format="f",digits=4),
                                     formatC(lnAUC0INF_pearson_P,format="f",digits=4),formatC(lnAUC0INF_PFpm,format="f",digits=4),formatC(lnAUC0INF_spearman_P,format="f",digits=4)))                   
colnames(outputEquality)<- c("Parameter","Test","P value")
show(outputEquality) 
cat("\n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat("  The standard 2*2*2 crossover design was assumed that intra-subject      \n")
cat("variabilities for  the Test and the Reference formulations are the same.    \n")
cat("Thus, if the intra-subject variabilities between formulations are different, \n")
cat("equivalence in average bioavailabilities between formulations does not    \n")
cat("imply that the two formulations are therapeutically equivalent and        \n")
cat("interchangeable.                                                          \n")
cat("  We use both parametric (Pitman-Morgan's adjusted F test and Pearson \n")
cat("correlation coefficient) and nonparametric test (Spearman's rank correlation\n")
cat("coefficient) for testing equality of intra-subject variabilities between \n") 
cat("formulations.  If a P value is less than 0.05, we may reject the null     \n")
cat("hypothesis of equality in intra-subject variabilities between formulations.\n")
cat("\n")
cat("**Ref.:\n")
cat(" 1. Chow SC and Liu JP. Design and Analysis of Bioavailability-            \n")
cat("    Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).  \n")
cat(" 2. Haynes JD. Statistical simulation study of new proposed uniformity     \n")
cat("    requirements for bioequivalency studies. Journal of Pharmaceutical     \n")
cat("    Sciences, 70, 673-675 (1981).                                          \n")
cat(" 3. McCulloch CE. Tests for equality of variances with paired data.        \n")
cat("    Communications in Statistics-Theory and Methods, 16, 1377-1391 (1987).  \n")
cat("--------------------------------------------------------------------------\n")
cat("\n") 
cat("\n")
cat("\n")

cat("Point and Interval Estimation of Inter- and Intra-subject Variability  \n")
cat("--------------------------------------------------------------------------\n")
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

show(outputPoint) 
cat("\n")
cat("-------------------------------------------------\n")
cat("intra: intra-subject variability\n")
cat("inter: inter-subject variability\n")
cat("intraclass: intraclass correlation\n")
cat("prob: the probability for obtaining a negative estimate of inter-subject\n") 
cat("      variability\n")
cat("CI95: 95% confidence interval \n")
cat("-------------------------------------------------\n")
cat("**Interpretation:\n")
cat("1. Prior information of inter- and intra- subject variabilities can be used   \n")
cat("   for sample size determination.                                            \n")
cat("2. Intraclass correlation shows the precision of intra-subject variability.\n")
cat("   A negative estimate indicates that inter- and intra- variability on the   \n")
cat("   subjects are negatively correlated.                                       \n")
cat("3. Searle(1971) provided a formula for calculation of the probability for  \n")
cat("   obtaining a negative estimate of inter-subject variability. In addition,   \n")
cat("   a negative estimate may indicate that the general model is incorrect or   \n")
cat("   sample size is too small.  Thus, prob provides the probability for      \n")
cat("   obtaining a negative estimate.  If P value is less than 0.05, the chance  \n")                                
cat("   of obtaining a negative estimate is negligible.                          \n")
cat("\n")
cat("**Ref.:\n")
cat(" 1. Chow SC and Liu JP. Design and Analysis of Bioavailability-            \n")
cat("    Bioequivalence Studies. 3rd ed., Chapman & Hall/CRC, New York (2009).  \n")
cat(" 2. Searle SR. Linear Models. John Wiley & Sons, New York (1971).          \n")
cat(" 3. Snedecor GW and Cochran WG. Statistical Methods. 7th ed.,  Iowa State  \n")
cat("    University Press, Ames, IA (1980).                                     \n")
cat(" 4. Hocking RP. The Analysis of Linear Models. Brooks/Cole, Monterey, CA   \n")
cat("    (1985).                                                                \n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")
cat("\n")
##Quantiles
lnCmax_1deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=1, type=1)
lnCmax_0.99deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.99, type=1)
lnCmax_0.95deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.95, type=1)
lnCmax_0.90deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.90, type=1)
lnCmax_0.75deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.75, type=1)
lnCmax_0.50deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.50, type=7)
lnCmax_0.25deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.25, type=1)
lnCmax_0.10deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.10, type=1)
lnCmax_0.05deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.05, type=1)
lnCmax_0.01deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0.01, type=1)
lnCmax_0deciles_intra <- quantile(IntraInterlnCmax00$Stud_Intra, probs=0, type=1)

lnCmax_1deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=1, type=1)
lnCmax_0.99deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.99, type=1)
lnCmax_0.95deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.95, type=1)
lnCmax_0.90deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.90, type=1)
lnCmax_0.75deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.75, type=1)
lnCmax_0.50deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.50, type=7)
lnCmax_0.25deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.25, type=1)
lnCmax_0.10deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.10, type=1)
lnCmax_0.05deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.05, type=1)
lnCmax_0.01deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0.01, type=1)
lnCmax_0deciles_inter <- quantile(IntraInterlnCmax00$Stud_Inter, probs=0, type=1)

lnAUC0t_1deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=1, type=1)
lnAUC0t_0.99deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.99, type=1)
lnAUC0t_0.95deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.95, type=1)
lnAUC0t_0.90deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.90, type=1)
lnAUC0t_0.75deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.75, type=1)
lnAUC0t_0.50deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.50, type=7)
lnAUC0t_0.25deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.25, type=1)
lnAUC0t_0.10deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.10, type=1)
lnAUC0t_0.05deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.05, type=1)
lnAUC0t_0.01deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0.01, type=1)
lnAUC0t_0deciles_intra <- quantile(IntraInterlnAUC0t00$Stud_Intra, probs=0, type=1)

lnAUC0t_1deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=1, type=1)
lnAUC0t_0.99deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.99, type=1)
lnAUC0t_0.95deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.95, type=1)
lnAUC0t_0.90deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.90, type=1)
lnAUC0t_0.75deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.75, type=1)
lnAUC0t_0.50deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.50, type=1)
lnAUC0t_0.25deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.25, type=1)
lnAUC0t_0.10deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.10, type=1)
lnAUC0t_0.05deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.05, type=1)
lnAUC0t_0.01deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0.01, type=1)
lnAUC0t_0deciles_inter <- quantile(IntraInterlnAUC0t00$Stud_Inter, probs=0, type=1)

lnAUC0INF_1deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=1, type=1)
lnAUC0INF_0.99deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.99, type=1)
lnAUC0INF_0.95deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.95, type=1)
lnAUC0INF_0.90deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.90, type=1)
lnAUC0INF_0.75deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.75, type=1)
lnAUC0INF_0.50deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.50, type=7)
lnAUC0INF_0.25deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.25, type=1)
lnAUC0INF_0.10deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.10, type=1)
lnAUC0INF_0.05deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.05, type=1)
lnAUC0INF_0.01deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0.01, type=1)
lnAUC0INF_0deciles_intra <- quantile(IntraInterlnAUC0INF00$Stud_Intra, probs=0, type=1)

lnAUC0INF_1deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=1, type=1)
lnAUC0INF_0.99deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.99, type=1)
lnAUC0INF_0.95deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.95, type=1)
lnAUC0INF_0.90deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.90, type=1)
lnAUC0INF_0.75deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.75, type=1)
lnAUC0INF_0.50deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.50, type=7)
lnAUC0INF_0.25deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.25, type=1)
lnAUC0INF_0.10deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.10, type=1)
lnAUC0INF_0.05deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.05, type=1)
lnAUC0INF_0.01deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0.01, type=1)
lnAUC0INF_0deciles_inter <- quantile(IntraInterlnAUC0INF00$Stud_Inter, probs=0, type=1)

cat("Quantiles for Boxplots (intrasubj)  \n")
cat("--------------------------------------------------------------------------\n")
Quantiles_intra<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_intra,lnCmax_0.99deciles_intra,lnCmax_0.95deciles_intra,lnCmax_0.90deciles_intra,lnCmax_0.75deciles_intra,lnCmax_0.50deciles_intra,
                                              lnCmax_0.25deciles_intra,lnCmax_0.10deciles_intra,lnCmax_0.05deciles_intra,lnCmax_0.01deciles_intra,lnCmax_0deciles_intra ),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_intra,lnAUC0t_0.99deciles_intra,lnAUC0t_0.95deciles_intra,lnAUC0t_0.90deciles_intra,lnAUC0t_0.75deciles_intra,lnAUC0t_0.50deciles_intra,
                                               lnAUC0t_0.25deciles_intra,lnAUC0t_0.10deciles_intra,lnAUC0t_0.05deciles_intra,lnAUC0t_0.01deciles_intra,lnAUC0t_0deciles_intra ),
                            lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_intra,lnAUC0INF_0.99deciles_intra,lnAUC0INF_0.95deciles_intra,lnAUC0INF_0.90deciles_intra,lnAUC0INF_0.75deciles_intra,lnAUC0INF_0.50deciles_intra,
                                              lnAUC0INF_0.25deciles_intra,lnAUC0INF_0.10deciles_intra,lnAUC0INF_0.05deciles_intra,lnAUC0INF_0.01deciles_intra,lnAUC0INF_0deciles_intra ))

show(Quantiles_intra) 
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("Quantiles for Boxplots (intersubj)  \n")
cat("--------------------------------------------------------------------------\n")
Quantiles_inter<-data.frame(Quantile=c("Max 100%","99%","95%","90%","Q3 75%","Median 50%",
                                       "Q1 25%","10%","5%","1%","Min 0%"),
                            lnCmax_Estimate=c(lnCmax_1deciles_inter,lnCmax_0.99deciles_inter,lnCmax_0.95deciles_inter,lnCmax_0.90deciles_inter,lnCmax_0.75deciles_inter,lnCmax_0.50deciles_inter,
                                              lnCmax_0.25deciles_inter,lnCmax_0.10deciles_inter,lnCmax_0.05deciles_inter,lnCmax_0.01deciles_inter,lnCmax_0deciles_inter ),
                            lnAUC0t_Estimate=c(lnAUC0t_1deciles_inter,lnAUC0t_0.99deciles_inter,lnAUC0t_0.95deciles_inter,lnAUC0t_0.90deciles_inter,lnAUC0t_0.75deciles_inter,lnAUC0t_0.50deciles_inter,
                                               lnAUC0t_0.25deciles_inter,lnAUC0t_0.10deciles_inter,lnAUC0t_0.05deciles_inter,lnAUC0t_0.01deciles_inter,lnAUC0t_0deciles_inter ),
                            lnAUC0INF_Estimate=c(lnAUC0INF_1deciles_inter,lnAUC0INF_0.99deciles_inter,lnAUC0INF_0.95deciles_inter,lnAUC0INF_0.90deciles_inter,lnAUC0INF_0.75deciles_inter,lnAUC0INF_0.50deciles_inter,
                                              lnAUC0INF_0.25deciles_inter,lnAUC0INF_0.10deciles_inter,lnAUC0INF_0.05deciles_inter,lnAUC0INF_0.01deciles_inter,lnAUC0INF_0deciles_inter ))

show(Quantiles_inter)
cat("-------------------------------------------------------------------------\n")
}