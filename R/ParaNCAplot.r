#plot for parallel study
ParaNCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis){
  NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,
                  replicated=FALSE, parallel=TRUE)
  }