MultipleBANOVAplot<-function(IntraInterlnCmax00, IntraInterlnAUC0t00,
                   IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                   IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,TotalData,
                   IntraInterlnAUC0INF00,IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
{
BANOVAplot(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
                     IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                     IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
                     IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData, multiple=TRUE)
}