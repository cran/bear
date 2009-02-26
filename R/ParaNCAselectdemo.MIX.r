#select 3 points -> lme (demo) for replicated study
ParaNCAselectdemo.MIX<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis){
NCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis,
         Demo=TRUE, BANOVA=FALSE, replicated=FALSE, MIX=TRUE, parallel=TRUE)
}