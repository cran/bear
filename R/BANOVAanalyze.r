### Generalized Linear Models (GLM)
### at the end, HY also showed the ODA plots on SCREEN; BANOVAoutput() call BANOVAplot() to log all ODA plots into pdf file.
### so I omit this part (to display ODA plots on screen) since bear v2.5.4.9. -YJ
### this is for non replicated study; if replicate --> RepMIXanalyze()  --YJ
###
BANOVAanalyze<-function(TotalData,multiple=FALSE,separateWindows=TRUE)
{
## ODA is off by default
ODAnalysis<-ODAnalysis
##
description_BANOVA()
###
### select AUC calculation method
### selection has been made in NCA.BANOVAanalyze()
###
lin.AUC<-lin.AUC
pAUC<-pAUC               ### for pAUC
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz

if(multiple){
  if(pAUC){
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax_ss,AUC0t=TotalData$AUCtau_ss,partAUC=TotalData$partAUC,
                   lnCmax=TotalData$lnCmax_ss,lnAUC0t=TotalData$lnAUCtau_ss,lnpAUC=TotalData$lnpAUC)
  }
  else{
  TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax_ss,AUC0t=TotalData$AUCtau_ss,
                   lnCmax=TotalData$lnCmax_ss,lnAUC0t=TotalData$lnAUCtau_ss)}
 }
else{
  if(pAUC){
   TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,
                   partAUC=TotalData$partAUC,lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,
                   lnAUC0INF=TotalData$lnAUC0INF,lnpAUC=TotalData$lnpAUC) 
  }
  else{
   TotalData<-data.frame(subj=as.factor(TotalData$subj),drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax,AUC0t=TotalData$AUC0t,AUC0INF=TotalData$AUC0INF,
                   lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF) 
  }
}

  lnCmax_theta1    <- BE_LL        # theta1: lower acceptance limit
  lnCmax_theta2    <- BE_UL
  lnAUC0t_theta1   <- BE_LL        # theta1: lower acceptance limit
  lnAUC0t_theta2   <- BE_UL
  lnAUC0INF_theta1 <- BE_LL        # theta1: lower acceptance limit
  lnAUC0INF_theta2 <- BE_UL
  
  if(pAUC){lnpAUC_theta1<- BE_LL;lnpAUC_theta2<- BE_UL}

Fdata<-split(TotalData,list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

SeqLeg<-split(RefData,list(RefData$seq))
L1<-length(SeqLeg[[1]]$seq)
L2<-length(SeqLeg[[2]]$seq)

Todata<-split(TotalData,list(TotalData$prd,TotalData$seq))

##MSinter and MSintra  for lnCmax
lnCmax_MSinter<-(summary(aov(lnCmax ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,3]
lnCmax_MSintra<-(summary(aov(lnCmax ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,3]

lnCmax_SSinter<-(summary(aov(lnCmax ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,2]
lnCmax_SSintra<-(summary(aov(lnCmax ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,2]
#Y1 (lnCmax in period 1) 

#EY mean of (ref and test)
lnCmax_Y1<-RefData$lnCmax 
 
lnCmax_EYseq1<-(Todata[[1]]$lnCmax+Todata[[2]]$lnCmax)/2
lnCmax_EYseq2<-(Todata[[3]]$lnCmax+Todata[[4]]$lnCmax)/2
#Y11(prd=1,seq=1), Y22(prd=2,seq=2),Y12(prd=1,seq=2), Y21(prd=2,seq=1)

lnCmax_Y11<- mean(Todata[[1]]$lnCmax)
lnCmax_Y21<- mean(Todata[[2]]$lnCmax)
lnCmax_Y12<- mean(Todata[[3]]$lnCmax)
lnCmax_Y22<- mean(Todata[[4]]$lnCmax)

lnCmax_Yseq1<- (lnCmax_Y11 + lnCmax_Y21)/2
lnCmax_Yseq2<- (lnCmax_Y12 + lnCmax_Y22)/2

lnCmax_Intra_residual1<-Todata[[1]]$lnCmax-(lnCmax_EYseq1+lnCmax_Y11-lnCmax_Yseq1)
lnCmax_Intra_residual2<-Todata[[3]]$lnCmax-(lnCmax_EYseq2+lnCmax_Y12-lnCmax_Yseq2)

lnCmax_Inter_residual1 <-2*(lnCmax_EYseq1-lnCmax_Yseq1)
lnCmax_Inter_residual2 <-2*(lnCmax_EYseq2-lnCmax_Yseq2)

lnCmax_stud1<-sqrt(((L1-1)/(2*L1))*lnCmax_MSintra)
lnCmax_stud2<-sqrt(((L2-1)/(2*L2))*lnCmax_MSintra)

lnCmax_inter_stud1<-sqrt((2*(L1-1)/L1)*lnCmax_MSinter)
lnCmax_inter_stud2<-sqrt((2*(L2-1)/L2)*lnCmax_MSinter)
#Expected_Y

IntraInterlnCmaxseq11<-data.frame(subj=Todata[[1]]$subj,Obs=Todata[[1]]$lnCmax,
                                 Exp=(lnCmax_EYseq1+lnCmax_Y11-lnCmax_Yseq1),
                                 Intra=lnCmax_Intra_residual1,
                                 Stud_Intra=lnCmax_Intra_residual1/lnCmax_stud1,
                                 Inter=lnCmax_Inter_residual1,
                                 Stud_Inter=lnCmax_Inter_residual1/lnCmax_inter_stud1)

IntraInterlnCmaxseq22<-data.frame(subj=Todata[[3]]$subj,Obs=Todata[[3]]$lnCmax,
                                 Exp=(lnCmax_EYseq2+lnCmax_Y12-lnCmax_Yseq2),
                                 Intra=lnCmax_Intra_residual2,
                                 Stud_Intra=lnCmax_Intra_residual2/lnCmax_stud2,
                                 Inter=lnCmax_Inter_residual2,
                                 Stud_Inter=lnCmax_Inter_residual2/lnCmax_inter_stud2)

IntraInterlnCmax11<-rbind(IntraInterlnCmaxseq11,IntraInterlnCmaxseq22)
IntraInterlnCmax00<-IntraInterlnCmax11[ do.call(order, IntraInterlnCmax11) ,]

##MSinter and MSintra
lnAUC0t_MSinter<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,3]
lnAUC0t_MSintra<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,3]

lnAUC0t_SSinter<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,2]
lnAUC0t_SSintra<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,2]
#Y1 (lnAUC0t in period 1)
#EY mean of (ref and test)
lnAUC0t_Y1<-RefData$lnAUC0t

lnAUC0t_EYseq1<-(Todata[[1]]$lnAUC0t+Todata[[2]]$lnAUC0t)/2
lnAUC0t_EYseq2<-(Todata[[3]]$lnAUC0t+Todata[[4]]$lnAUC0t)/2
#Y11(prd=1,seq=1), Y22(prd=2,seq=2),Y12(prd=1,seq=2), Y21(prd=2,seq=1)

lnAUC0t_Y11<- mean(Todata[[1]]$lnAUC0t)
lnAUC0t_Y21<- mean(Todata[[2]]$lnAUC0t)
lnAUC0t_Y12<- mean(Todata[[3]]$lnAUC0t)
lnAUC0t_Y22<- mean(Todata[[4]]$lnAUC0t)

lnAUC0t_Yseq1<- (lnAUC0t_Y11 + lnAUC0t_Y21)/2
lnAUC0t_Yseq2<- (lnAUC0t_Y12 + lnAUC0t_Y22)/2

lnAUC0t_Intra_residual1<-Todata[[1]]$lnAUC0t-(lnAUC0t_EYseq1+lnAUC0t_Y11-lnAUC0t_Yseq1)
lnAUC0t_Intra_residual2<-Todata[[3]]$lnAUC0t-(lnAUC0t_EYseq2+lnAUC0t_Y12-lnAUC0t_Yseq2)

lnAUC0t_Inter_residual1 <-2*(lnAUC0t_EYseq1-lnAUC0t_Yseq1)
lnAUC0t_Inter_residual2 <-2*(lnAUC0t_EYseq2-lnAUC0t_Yseq2)

lnAUC0t_stud1<-sqrt(((L1-1)/(2*L1))*lnAUC0t_MSintra)
lnAUC0t_stud2<-sqrt(((L2-1)/(2*L2))*lnAUC0t_MSintra)

lnAUC0t_inter_stud1<-sqrt((2*(L1-1)/L1)*lnAUC0t_MSinter)
lnAUC0t_inter_stud2<-sqrt((2*(L2-1)/L2)*lnAUC0t_MSinter)
#Expected_Y

IntraInterlnAUC0tseq11<-data.frame(subj=Todata[[1]]$subj,Obs=Todata[[1]]$lnAUC0t,
                                 Exp=(lnAUC0t_EYseq1+lnAUC0t_Y11-lnAUC0t_Yseq1),
                                 Intra=lnAUC0t_Intra_residual1,
                                 Stud_Intra=lnAUC0t_Intra_residual1/lnAUC0t_stud1,
                                 Inter=lnAUC0t_Inter_residual1,
                                 Stud_Inter=lnAUC0t_Inter_residual1/lnAUC0t_inter_stud1)

IntraInterlnAUC0tseq22<-data.frame(subj=Todata[[3]]$subj,Obs=Todata[[3]]$lnAUC0t,
                                 Exp=(lnAUC0t_EYseq2+lnAUC0t_Y12-lnAUC0t_Yseq2),
                                 Intra=lnAUC0t_Intra_residual2,
                                 Stud_Intra=lnAUC0t_Intra_residual2/lnAUC0t_stud2,
                                 Inter=lnAUC0t_Inter_residual2,
                                 Stud_Inter=lnAUC0t_Inter_residual2/lnAUC0t_inter_stud2)

IntraInterlnAUC0t11<-rbind(IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22)
IntraInterlnAUC0t00<-IntraInterlnAUC0t11[ do.call(order, IntraInterlnAUC0t11),]
###
### must be placed before 'if(mutliple){...}; otherwise it will miss when it is multiple-dsoe study.
### for pAUC
### Then BANOVA() & BANOVAoutput() need to be fixed. 
###
if(pAUC){
##MSinter and MSintra
lnpAUC_MSinter<-(summary(aov(lnpAUC ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,3]
lnpAUC_MSintra<-(summary(aov(lnpAUC ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,3]
lnpAUC_SSinter<-(summary(aov(lnpAUC ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,2]
lnpAUC_SSintra<-(summary(aov(lnpAUC ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,2]
#Y1 (lnpAUC in period 1)
#EY mean of (ref and test)
lnpAUC_Y1<-RefData$lnpAUC

lnpAUC_EYseq1<-(Todata[[1]]$lnpAUC+Todata[[2]]$lnpAUC)/2
lnpAUC_EYseq2<-(Todata[[3]]$lnpAUC+Todata[[4]]$lnpAUC)/2
#Y11(prd=1,seq=1), Y22(prd=2,seq=2),Y12(prd=1,seq=2), Y21(prd=2,seq=1)

lnpAUC_Y11<- mean(Todata[[1]]$lnpAUC)
lnpAUC_Y21<- mean(Todata[[2]]$lnpAUC)
lnpAUC_Y12<- mean(Todata[[3]]$lnpAUC)
lnpAUC_Y22<- mean(Todata[[4]]$lnpAUC)

lnpAUC_Yseq1<- (lnpAUC_Y11 + lnpAUC_Y21)/2
lnpAUC_Yseq2<- (lnpAUC_Y12 + lnpAUC_Y22)/2

lnpAUC_Intra_residual1<-Todata[[1]]$lnpAUC-(lnpAUC_EYseq1+lnpAUC_Y11-lnpAUC_Yseq1)
lnpAUC_Intra_residual2<-Todata[[3]]$lnpAUC-(lnpAUC_EYseq2+lnpAUC_Y12-lnpAUC_Yseq2)

lnpAUC_Inter_residual1 <-2*(lnpAUC_EYseq1-lnpAUC_Yseq1)
lnpAUC_Inter_residual2 <-2*(lnpAUC_EYseq2-lnpAUC_Yseq2)

lnpAUC_stud1<-sqrt(((L1-1)/(2*L1))*lnpAUC_MSintra)
lnpAUC_stud2<-sqrt(((L2-1)/(2*L2))*lnpAUC_MSintra)

lnpAUC_inter_stud1<-sqrt((2*(L1-1)/L1)*lnpAUC_MSinter)
lnpAUC_inter_stud2<-sqrt((2*(L2-1)/L2)*lnpAUC_MSinter)
#Expected_Y

IntraInterlnpAUCseq11<-data.frame(subj=Todata[[1]]$subj,Obs=Todata[[1]]$lnpAUC,
                                 Exp=(lnpAUC_EYseq1+lnpAUC_Y11-lnpAUC_Yseq1),
                                 Intra=lnpAUC_Intra_residual1,
                                 Stud_Intra=lnpAUC_Intra_residual1/lnpAUC_stud1,
                                 Inter=lnpAUC_Inter_residual1,
                                 Stud_Inter=lnpAUC_Inter_residual1/lnpAUC_inter_stud1)

IntraInterlnpAUCseq22<-data.frame(subj=Todata[[3]]$subj,Obs=Todata[[3]]$lnpAUC,
                                 Exp=(lnpAUC_EYseq2+lnpAUC_Y12-lnpAUC_Yseq2),
                                 Intra=lnpAUC_Intra_residual2,
                                 Stud_Intra=lnpAUC_Intra_residual2/lnpAUC_stud2,
                                 Inter=lnpAUC_Inter_residual2,
                                 Stud_Inter=lnpAUC_Inter_residual2/lnpAUC_inter_stud2)

IntraInterlnpAUC11<-rbind(IntraInterlnpAUCseq11,IntraInterlnpAUCseq22)
IntraInterlnpAUC00<-IntraInterlnpAUC11[ do.call(order, IntraInterlnpAUC11) ,]
}

if(multiple){
MultipleBANOVA(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnpAUC00,
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnpAUC_theta1,lnpAUC_theta2)
MultipleBANOVAoutput(RefData,TestData,TotalData,L1,L2,
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,IntraInterlnpAUC00,
       IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,IntraInterlnAUC0tseq11,
       IntraInterlnAUC0tseq22,IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,
       IntraInterlnpAUCseq11,IntraInterlnpAUCseq22,lnCmax_theta1,lnCmax_theta2,
       lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,lnAUC0INF_theta2,
       lnpAUC_theta1,lnpAUC_theta2)
       
##show in console
graphics.off()
###########                          ODA                   ################
### if(ODAnalysis){
### ### windows(record = TRUE)
### dev.new()
### .SavedPlots<-NULL
### MultipleBANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,
###                    IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
###                    IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,TotalData)
###               }
}
else{
##MSinter and MSintra
lnAUC0INF_MSinter<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,3]
lnAUC0INF_MSintra<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,3]
lnAUC0INF_SSinter<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) ,data=TotalData)))[[1]][[1]][2,2]
lnAUC0INF_SSintra<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) ,data=TotalData)))[[2]][[1]][3,2]
#Y1 (lnAUC0INF in period 1)
#EY mean of (ref and test)
lnAUC0INF_Y1<-RefData$lnAUC0INF

lnAUC0INF_EYseq1<-(Todata[[1]]$lnAUC0INF+Todata[[2]]$lnAUC0INF)/2
lnAUC0INF_EYseq2<-(Todata[[3]]$lnAUC0INF+Todata[[4]]$lnAUC0INF)/2
#Y11(prd=1,seq=1), Y22(prd=2,seq=2),Y12(prd=1,seq=2), Y21(prd=2,seq=1)

lnAUC0INF_Y11<- mean(Todata[[1]]$lnAUC0INF)
lnAUC0INF_Y21<- mean(Todata[[2]]$lnAUC0INF)
lnAUC0INF_Y12<- mean(Todata[[3]]$lnAUC0INF)
lnAUC0INF_Y22<- mean(Todata[[4]]$lnAUC0INF)

lnAUC0INF_Yseq1<- (lnAUC0INF_Y11 + lnAUC0INF_Y21)/2
lnAUC0INF_Yseq2<- (lnAUC0INF_Y12 + lnAUC0INF_Y22)/2

lnAUC0INF_Intra_residual1<-Todata[[1]]$lnAUC0INF-(lnAUC0INF_EYseq1+lnAUC0INF_Y11-lnAUC0INF_Yseq1)
lnAUC0INF_Intra_residual2<-Todata[[3]]$lnAUC0INF-(lnAUC0INF_EYseq2+lnAUC0INF_Y12-lnAUC0INF_Yseq2)

lnAUC0INF_Inter_residual1 <-2*(lnAUC0INF_EYseq1-lnAUC0INF_Yseq1)
lnAUC0INF_Inter_residual2 <-2*(lnAUC0INF_EYseq2-lnAUC0INF_Yseq2)

lnAUC0INF_stud1<-sqrt(((L1-1)/(2*L1))*lnAUC0INF_MSintra)
lnAUC0INF_stud2<-sqrt(((L2-1)/(2*L2))*lnAUC0INF_MSintra)

lnAUC0INF_inter_stud1<-sqrt((2*(L1-1)/L1)*lnAUC0INF_MSinter)
lnAUC0INF_inter_stud2<-sqrt((2*(L2-1)/L2)*lnAUC0INF_MSinter)
#Expected_Y

IntraInterlnAUC0INFseq11<-data.frame(subj=Todata[[1]]$subj, Obs=Todata[[1]]$lnAUC0INF,
                                 Exp=(lnAUC0INF_EYseq1+lnAUC0INF_Y11-lnAUC0INF_Yseq1),
                                 Intra=lnAUC0INF_Intra_residual1,
                                 Stud_Intra=lnAUC0INF_Intra_residual1/lnAUC0INF_stud1,
                                 Inter=lnAUC0INF_Inter_residual1,
                                 Stud_Inter=lnAUC0INF_Inter_residual1/lnAUC0INF_inter_stud1)

IntraInterlnAUC0INFseq22<-data.frame(subj=Todata[[3]]$subj, Obs=Todata[[3]]$lnAUC0INF,
                                 Exp=(lnAUC0INF_EYseq2+lnAUC0INF_Y12-lnAUC0INF_Yseq2),
                                 Intra=lnAUC0INF_Intra_residual2,
                                 Stud_Intra=lnAUC0INF_Intra_residual2/lnAUC0INF_stud2,
                                 Inter=lnAUC0INF_Inter_residual2,
                                 Stud_Inter=lnAUC0INF_Inter_residual2/lnAUC0INF_inter_stud2)

IntraInterlnAUC0INF11<-rbind(IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
IntraInterlnAUC0INF00<-IntraInterlnAUC0INF11[ do.call(order, IntraInterlnAUC0INF11) ,]

BANOVA(RefData,TestData,TotalData,L1,L2,                                   ### used to show lm() or lme() output on screen
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,                      ### if pAUC
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,IntraInterlnpAUC00,  ### if pAUC
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,lnAUC0INF_theta1,
       lnAUC0INF_theta2,lnpAUC_theta1,lnpAUC_theta2)                                     ### if pAUC
BANOVAoutput(RefData,TestData,TotalData,L1,L2,                             ### same as BONAVA(), but is used to generate ***.stat_sum.txt
       lnCmax_MSinter,lnCmax_MSintra,lnCmax_SSinter,lnCmax_SSintra,
       lnAUC0t_MSinter,lnAUC0t_MSintra,lnAUC0t_SSinter,lnAUC0t_SSintra,
       lnAUC0INF_MSinter,lnAUC0INF_MSintra,lnAUC0INF_SSinter,lnAUC0INF_SSintra,
       lnpAUC_MSinter,lnpAUC_MSintra,lnpAUC_SSinter,lnpAUC_SSintra,        ### if pAUC; otherwise, will be ignored
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
       IntraInterlnpAUC00,IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
       IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
       IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,
       IntraInterlnpAUCseq11,IntraInterlnpAUCseq22,                        ### if pAUC; otherwise, will be ignored
       lnCmax_theta1,lnCmax_theta2,lnAUC0t_theta1,lnAUC0t_theta2,
       lnAUC0INF_theta1,lnAUC0INF_theta2,lnpAUC_theta1,lnpAUC_theta2)      ### if pAUC; otherwise, will be ignored

###
### show in console
graphics.off()
### close ODA by default
### I turn off displaying ODA plots on screen function with remarked following lines.
###  YJ (04/23/2013)
###
### if(ODAnalysis){
### windows(record = TRUE)
### .SavedPlots<-NULL
### BANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00, 
###            IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
###            IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
###            IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22, TotalData)
###       }
 }
}