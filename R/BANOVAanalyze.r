#Generalized Linear Models (GLM)
BANOVAanalyze<-function(TotalData, separateWindows=TRUE)
{
description_BANOVA()
TotalData<-data.frame (subj=as.factor(TotalData$subj), drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF, 
                   lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF) 

Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

SeqLeg<-split(RefData, list(RefData$seq))
L1<-length(SeqLeg[[1]]$seq)
L2<-length(SeqLeg[[2]]$seq)
Todata<-split(TotalData, list(TotalData$prd,TotalData$seq))

##MSinter and MSintra  for lnCmax
lnCmax_MSinter<-(summary(aov(lnCmax ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,3]
lnCmax_MSintra<-(summary(aov(lnCmax ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,3]

lnCmax_SSinter<-(summary(aov(lnCmax ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,2]
lnCmax_SSintra<-(summary(aov(lnCmax ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,2]
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

IntraInterlnCmaxseq11<-data.frame(subj=Todata[[1]]$subj, Obs=Todata[[1]]$lnCmax, 
                                 Exp=(lnCmax_EYseq1+lnCmax_Y11-lnCmax_Yseq1), 
                                 Intra=lnCmax_Intra_residual1,
                                 Stud_Intra=lnCmax_Intra_residual1/lnCmax_stud1,
                                 Inter=lnCmax_Inter_residual1,
                                 Stud_Inter=lnCmax_Inter_residual1/lnCmax_inter_stud1)

IntraInterlnCmaxseq22<-data.frame(subj=Todata[[3]]$subj, Obs=Todata[[3]]$lnCmax,
                                 Exp=(lnCmax_EYseq2+lnCmax_Y12-lnCmax_Yseq2),
                                 Intra=lnCmax_Intra_residual2,
                                 Stud_Intra=lnCmax_Intra_residual2/lnCmax_stud2,
                                 Inter=lnCmax_Inter_residual2,
                                 Stud_Inter=lnCmax_Inter_residual2/lnCmax_inter_stud2)

IntraInterlnCmax11<-rbind(IntraInterlnCmaxseq11,IntraInterlnCmaxseq22)
IntraInterlnCmax00<-IntraInterlnCmax11[ do.call(order, IntraInterlnCmax11) ,]


##MSinter and MSintra
lnAUC0t_MSinter<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,3]
lnAUC0t_MSintra<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,3]

lnAUC0t_SSinter<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,2]
lnAUC0t_SSintra<-(summary(aov(lnAUC0t ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,2]
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

IntraInterlnAUC0tseq11<-data.frame(subj=Todata[[1]]$subj, Obs=Todata[[1]]$lnAUC0t,
                                 Exp=(lnAUC0t_EYseq1+lnAUC0t_Y11-lnAUC0t_Yseq1),
                                 Intra=lnAUC0t_Intra_residual1,
                                 Stud_Intra=lnAUC0t_Intra_residual1/lnAUC0t_stud1,
                                 Inter=lnAUC0t_Inter_residual1,
                                 Stud_Inter=lnAUC0t_Inter_residual1/lnAUC0t_inter_stud1)

IntraInterlnAUC0tseq22<-data.frame(subj=Todata[[3]]$subj, Obs=Todata[[3]]$lnAUC0t,
                                 Exp=(lnAUC0t_EYseq2+lnAUC0t_Y12-lnAUC0t_Yseq2),
                                 Intra=lnAUC0t_Intra_residual2,
                                 Stud_Intra=lnAUC0t_Intra_residual2/lnAUC0t_stud2,
                                 Inter=lnAUC0t_Inter_residual2,
                                 Stud_Inter=lnAUC0t_Inter_residual2/lnAUC0t_inter_stud2)

IntraInterlnAUC0t11<-rbind(IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22)
IntraInterlnAUC0t00<-IntraInterlnAUC0t11[do.call(order, IntraInterlnAUC0t11),]


##MSinter and MSintra
lnAUC0INF_MSinter<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,3]
lnAUC0INF_MSintra<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,3]
lnAUC0INF_SSinter<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) , data=TotalData)))[[1]][[1]][2,2]
lnAUC0INF_SSintra<-(summary(aov(lnAUC0INF ~ prd*drug + Error(subj) , data=TotalData)))[[2]][[1]][3,2]
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

BANOVA(RefData, TestData, TotalData, L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00)
BANOVAoutput(RefData, TestData, TotalData,  L1, L2,
       lnCmax_MSinter, lnCmax_MSintra, lnCmax_SSinter, lnCmax_SSintra,
       lnAUC0t_MSinter, lnAUC0t_MSintra, lnAUC0t_SSinter, lnAUC0t_SSintra,
       lnAUC0INF_MSinter, lnAUC0INF_MSintra, lnAUC0INF_SSinter, lnAUC0INF_SSintra,                
       IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
       IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
       IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
##show in console
windows(record = TRUE )
BANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00, 
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
}