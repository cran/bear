###NCA method and ANOVA 
NCA.BANOVA<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis){
NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,
    SingleTdata1,xaxis, yaxis, Demo=FALSE, BANOVA=TRUE)
}