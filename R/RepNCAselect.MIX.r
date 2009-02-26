#select 3 points (NCA -> lme) for replicated study
RepNCAselect.MIX<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis){

NCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis,
          Demo=FALSE, BANOVA=FALSE, replicated=TRUE,MIX=TRUE, parallel=FALSE)
}