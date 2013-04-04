NCAdemo<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split){
NCA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split, 
    Demo=FALSE, BANOVA=FALSE,replicated=FALSE, MIX=FALSE)   ### switch 'Demo=FALSE', although it is demo mode here... 
                                                            ### this fix can allow user to save data point selection. --YJ
}