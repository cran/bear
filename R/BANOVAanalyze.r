#Generalized Linear Models (GLM)
BANOVAanalyze<-function(TotalData, separateWindows=TRUE)
{
description_BANOVA()
TotalData<-data.frame (subj=as.factor(TotalData$subj), drug=as.factor(TotalData$drug),seq=as.factor(TotalData$seq),
                   prd=as.factor(TotalData$prd),Cmax=TotalData$Cmax, AUC0t=TotalData$AUC0t, AUC0INF=TotalData$AUC0INF, 
                   LnCmax=log(TotalData$Cmax),LnAUC0t=log(TotalData$AUC0t),LnAUC0INF=log(TotalData$AUC0INF)) 

Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

BANOVA(RefData, TestData, TotalData)
BANOVAoutput(RefData, TestData, TotalData)

}