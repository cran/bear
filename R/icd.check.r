### check if incomplete_dataset (icd)
icd.check<-function(){

#split dataframe into sub-dataframe by subject for test data
options(warn=-1)

##                    
## Show message if this is Incomplete_Dataset.
## 
    alarm();alarm()
    cat("\n\n This could be an incomplete dataset, \n")
    cat(" i.e., 'Subj of taking R' is not exactly same as 'Subj of taking Test'.\n")
    cat(" At the moment, bear does not handel with any incomplete dataset.\n\n")
    stop(" Please restart R session again...", call.=FALSE)  
###      
}
