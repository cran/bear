RepMIXanalyze<-function(TotalData, separateWindows=TRUE)
{
description_RepMIX()
TotalData<-data.frame (subj=as.factor(TotalData$subj), drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF,
                   lnCmax=TotalData$lnCmax,lnAUC0t=TotalData$lnAUC0t,lnAUC0INF=TotalData$lnAUC0INF)

Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

SeqLeg<-split(RefData, list(RefData$seq))
SeqLeg1 <- reshape(SeqLeg[[1]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")
SeqLeg2 <- reshape(SeqLeg[[2]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")  
L1<-length(SeqLeg1$subj)
L2<-length(SeqLeg2$subj)

ref_lnCmax<-mean(RefData$lnCmax)
ref_lnAUC0t<-mean(RefData$lnAUC0t)
ref_lnAUC0INF<-mean(RefData$lnAUC0INF)

test_lnCmax<-mean(TestData$lnCmax)
test_lnAUC0t<-mean(TestData$lnAUC0t)
test_lnAUC0INF<-mean(TestData$lnAUC0INF)

RepMIX(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF)
RepMIXoutput(TotalData, L1,L2,ref_lnCmax,ref_lnAUC0t,ref_lnAUC0INF,test_lnCmax,test_lnAUC0t,test_lnAUC0INF)
}
