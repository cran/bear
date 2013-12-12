MultipleBANOVAplot<-function(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
           IntraInterlnpAUC00,IntraInterlnpAUCseq11,IntraInterlnpAUCseq22)
{
pAUC<-pAUC               ### for pAUC

if(pAUC){
BANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
           IntraInterlnpAUC00,IntraInterlnpAUCseq11,IntraInterlnpAUCseq22,multiple=TRUE)
}
else{
BANOVAplot(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
           IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
           IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
           IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
           IntraInterlnpAUC00=0,IntraInterlnpAUCseq11=0,IntraInterlnpAUCseq22=0,multiple=TRUE)
}
}