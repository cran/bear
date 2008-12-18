##ANOVA
BANOVA<-function(RefData, TestData,TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00)
{

#represent GLM
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                  \n")
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
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                   \n")
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
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                   \n")
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
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                   \n")
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
cat("Intra_subj. CV=100*sqrt(exp(MSResidual)-1)=",100*sqrt(exp(anova(lnCmax)[5,3])-1),"%\n")
cat("Inter_subj. CV=100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)=",100*sqrt(exp((anova(lnCmax)[4,3]-anova(lnCmax)[5,3])/2)-1),"%\n")
cat("\n")
cat("\n")
cat("\n")

#GLM_lnAUC0t.txt
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                   \n")
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
cat("Intra_subj. CV=100*sqrt(exp(MSResidual)-1)=",100*sqrt(exp(anova(lnAUC0t)[5,3])-1),"%\n")
cat("Inter_subj. CV=100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)=",100*sqrt(exp((anova(lnAUC0t)[4,3]-anova(lnAUC0t)[5,3])/2)-1),"%\n")
cat("\n")
cat("\n")
cat("\n")

#GLM_AUC0INF.txt
cat("  Statistical analysis (ANOVA(lm), 90%CI...)                   \n")
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
cat("Intra_subj. CV=100*sqrt(exp(MSResidual)-1)=",100*sqrt(exp(anova(lnAUC0INF)[5,3])),"%\n")
cat("Inter_subj. CV=100*sqrt(exp((MSSubject(seq)-MSResidual)/2)-1)=",100*sqrt(exp((anova(lnAUC0INF)[4,3]-anova(lnAUC0INF)[5,3])/2)-1),"%\n")
cat("\n")
cat("\n")
cat("\n")

##########################################################################################
########REPORT
#L1(Reference-->Test),L2(Test-->Reference sequence)
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

Z_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qnorm(0.95)
Z_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qnorm(0.95)
Z_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qnorm(0.95)

T_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qt(0.975,L1+L2-2)
T_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qt(0.975,L1+L2-2)
T_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qt(0.975,L1+L2-2)

lowerCmax<-100*exp((test_Cmax-ref_Cmax)-(T*SE_Cmax))
upperCmax<-100*exp((test_Cmax-ref_Cmax)+(T*SE_Cmax))
lowerAUC0t<-100*exp((test_AUC0t-ref_AUC0t)-(T*SE_AUC0t))
UpperAUC0t<-100*exp((test_AUC0t-ref_AUC0t)+(T*SE_AUC0t))
LowerAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)+(T*SE_AUC0INF))

PowerCmaxZ<-pnorm(abs(Z_Cmax))
PowerCmaxT<-pt(T_Cmax,L1+L2-2)
PowerZAUC0t<-pnorm(abs(Z_AUC0t))
PowerTAUC0t<-pt(T_AUC0t,L1+L2-2)
PowerZAUC0INF<-pnorm(abs(Z_AUC0INF))
PowerTAUC0INF<-pt(T_AUC0INF,L1+L2-2)

Todata<-split(TotalData, list(TotalData$prd,TotalData$seq))
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
cat("  BE Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnCmax                                               \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R=>T) =",L1 , "\n")
cat("        n2(T=>R) =",L2 , "\n")
cat("        N(n1+n2) =",L1+L2 , "\n")
cat("      LSMEAN-ref =",ref_Cmax, "\n")
cat("     LSMEAN-test =",test_Cmax, "\n")
cat("             MSE =",anova(lnCmax)[5,3], "\n")
cat("              SE =",SE_Cmax, "\n")
cat("Diff. (test-ref) =",test_Cmax-ref_Cmax, "\n")
cat("     t(0.95,N-2) =",T , "\n")
cat("         Z(beta) =",Z_Cmax, "\n")
cat(" Power(1-beta)_Z =",formatC(PowerCmaxZ,format="f",digits=3),"\n")
cat("         t(beta) =",T_Cmax, "\n")
cat(" Power(1-beta)_t =",formatC(PowerCmaxT,format="f",digits=3) ,"\n")
cat("\n")
cat("************************90% C.I. for lnCmax********************************\n")
cat("     90%CI Lower =",round(lowerCmax,3) ,"\n")
cat("     90%CI Upper =",round(upperCmax,3) ,"\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("  Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
show(IntraInterlnCmax00)
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

cat("  BE Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0t                                               \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R=>T) =",L1 , "\n")
cat("        n2(T=>R) =",L2 , "\n")
cat("        N(n1+n2) =",L1+L2 , "\n")
cat("      LSMEAN-ref =",ref_AUC0t, "\n")
cat("     LSMEAN-test =",test_AUC0t, "\n")
cat("             MSE =",anova(lnAUC0t)[5,3], "\n")
cat("              SE =",SE_AUC0t, "\n")
cat("Diff. (test-ref) =",test_AUC0t-ref_AUC0t, "\n")
cat("     t(0.95,N-2) =",T , "\n")
cat("         Z(beta) =",Z_AUC0t, "\n")
cat(" Power(1-beta)_Z =",formatC(PowerZAUC0t,format="f",digits=3),"\n")
cat("         t(beta) =",T_AUC0t, "\n")
cat(" Power(1-beta)_t =",formatC(PowerTAUC0t,format="f",digits=3),"\n")
cat("\n")
cat("************************90% C.I. for lnAUC0t*******************************\n")
cat("     90%CI Lower =",round(lowerAUC0t,3) ,"\n")
cat("     90%CI Upper =",round(UpperAUC0t,3) ,"\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("  Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
show(IntraInterlnAUC0t00)
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

cat("  BE Summary Report                            \n")
cat("--------------------------------------------------------------------------\n")
cat("  Dependent Variable: lnAUC0INF                                             \n")
cat("--------------------------------------------------------------------------\n")
cat("        n1(R=>T) =",L1 , "\n")
cat("        n2(T=>R) =",L2 , "\n")
cat("        N(n1+n2) =",L1+L2 , "\n")
cat("      LSMEAN-ref =",ref_AUC0INF, "\n")
cat("     LSMEAN-test =",test_AUC0INF, "\n")
cat("             MSE =",anova(lnAUC0INF)[5,3], "\n")
cat("              SE =",SE_AUC0INF, "\n")
cat("Diff. (test-ref) =",test_AUC0INF - ref_AUC0INF, "\n")
cat("     t(0.95,N-2) =",T , "\n")
cat("         Z(beta) =",Z_AUC0INF, "\n")
cat(" Power(1-beta)_Z =",formatC(PowerZAUC0INF,format="f",digits=3),"\n")
cat("         t(beta) =",T_AUC0INF, "\n")
cat(" Power(1-beta)_t =",formatC(PowerTAUC0INF,format="f",digits=3),"\n")
cat("\n")
cat("************************90% C.I. for lnAUC0INF*****************************\n")
cat("     90%CI Lower =",round(LowerAUC0INF,3) ,"\n")
cat("     90%CI Upper =",round(UpperAUC0INF,3) ,"\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("  Intra-subject and Inter-subject Residuals                  \n")
cat("--------------------------------------------------------------------------\n")
show(IntraInterlnAUC0INF00)
cat("------------------------------------------------\n")
cat("Obs: Observed lnAUC0INF\n")
cat("Exp: Expected lnAUC0INF\n")
cat("Intra: Intra-subject residuals\n")
cat("Stud_Intra: Studentized intra-subject residuals\n")
cat("Inter: Inter-subject residuals\n")
cat("Stud_Inter: Studentized inter-subject residuals\n")
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("Analysis of Outlier Detection\n")
cat("****************************************************************************\n")
cat("\n")
cat("\n")
cat("  Test for Normality Assumption  (Shapiro-Wilk)             \n")
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
show(outputSW)
cat("\n")
cat("-------------------------------------------------\n")
cat("Stud_Intra: studentized intra-subject residuals\n")
cat("Stud_Inter: studentized intra-subject residuals\n")
cat("Shapiro-Wilk: the normality of the studentized intra-subject residuals and \n")
cat("              the studentized inter-residuals was examined using the test  \n")
cat("              of Shapiro-Wilk.                                             \n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

cat("  Test for Normality Assumption  (Pearson)             \n")
cat("--------------------------------------------------------------------------\n")
outputPearson<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("pearson"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("pearson"))[[3]],format="f",digits=4)))
show(outputPearson)
cat("\n")
cat("-------------------------------------------------\n")
cat("Pearson: Pearson's correlation coefficient\n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")

cat("  Test for Normality Assumption  (Spearman)             \n")
cat("--------------------------------------------------------------------------\n")
outputSpearman<-data.frame(Parameter=c("lnCmax","lnAUC0t","lnAUC0INF"),
                     Test=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("spearman"))[[4]][[1]],format="f",digits=5)),
                     P_value=c(formatC(cor.test(IntraInterlnCmax00$Stud_Intra, IntraInterlnCmax00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0t00$Stud_Intra, IntraInterlnAUC0t00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4),
                            formatC(cor.test(IntraInterlnAUC0INF00$Stud_Intra, IntraInterlnAUC0INF00$Stud_Inter, method=c("spearman"))[[3]],format="f",digits=4)))
show(outputSpearman)
cat("\n")
cat("-------------------------------------------------\n")
cat("Spearman: Spearman's rank correlation coefficient\n")
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
show(outputEquality) 
cat("\n")
cat("\n") 
cat("\n")
cat("\n")
cat("  Point and Interval Estimation of Inter- and Intra-subject Variability  \n")
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
cat("-------------------------------------------------------------------------\n") 
cat("\n")
cat("\n")
cat("\n")

}