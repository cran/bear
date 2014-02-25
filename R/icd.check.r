### check if the dataset is the incompletes dataset (icd)
icd.check<-function(TotalSingledata, multiple=FALSE, replicated=FALSE, parallel=FALSE, NCA.only=TRUE)
{

#split dataframe into sub-dataframe by subject for test data
options(warn=-1)
###
### 1st to get SingleRdata & SingleTdata from TotalSingledata QUIETLY!
###

if(replicated){
predata<-split(TotalSingledata, list(TotalSingledata$prd,TotalSingledata$subj))

code<-NULL
presubj<-NULL
preseq<-NULL
preprd<-NULL
predrug<-NULL
pretime<-NULL
preconc<-NULL
precode<-NULL
for (j in 1:length(predata)){
   j=j
   code[[j]]<-j
 LL<-cbind(subj=predata[[j]]$subj,seq=predata[[j]]$seq,prd=predata[[j]]$prd,drug=predata[[j]]$drug,
           time=predata[[j]]$time,conc=predata[[j]]$conc,code=code[j])

 presubj[[j]]<-c(LL[,1])
 preseq[[j]]<-c(LL[,2])
 preprd[[j]]<-c(LL[,3])
 predrug[[j]]<-c(LL[,4])
 pretime[[j]]<-c(LL[,5])
 preconc[[j]]<-c(LL[,6])
 precode[[j]]<-c(LL[,7])
 
 }
setdata<-data.frame(subj=melt(presubj)$value, seq=melt(preseq)$value,prd=melt(preprd)$value,
                    drug=melt(predrug)$value, time=melt(pretime)$value, conc=melt(preconc)$value,
                    code=melt(precode)$value)                  
Singledata<-split(setdata, list(setdata$drug))

Refdata<-data.frame(subj=Singledata[[1]]$subj, seq= Singledata[[1]]$seq, prd=Singledata[[1]]$prd,
                    drug=Singledata[[1]]$drug, time=Singledata[[1]]$time, conc=Singledata[[1]]$conc,
                    code=Singledata[[1]]$code)
SingleRdata<-Refdata[ do.call(order, Refdata) ,]

Testdata<-data.frame(subj=Singledata[[2]]$subj, seq=Singledata[[2]]$seq, prd=Singledata[[2]]$prd,
                     drug=Singledata[[2]]$drug, time=Singledata[[2]]$time, conc=Singledata[[2]]$conc,
                     code=Singledata[[2]]$code)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
}
else{
if (multiple) {       ### first come with multiple-dose. -YJ
  if(parallel){
   Singledata<-split(TotalSingledata, list(TotalSingledata$drug))
   Ref<-Singledata[[1]]
   Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
    }
   else{
   Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
   Ref<-rbind(Singledata[[1]],Singledata[[4]])
   Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                    time=Ref$time, conc=Ref$conc)
    }
    SingleRdata<-Refdata[ do.call(order, Refdata) ,]
    
   if(parallel){
   Test<-rbind(Singledata[[2]])
   Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
    }
   else{
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
   }
     SingleTdata<-Testdata[ do.call(order, Testdata) ,]

}
else{        ### then followed with the single-dose
   if(parallel){
   Singledata<-split(TotalSingledata, list(TotalSingledata$drug))
   Ref<-Singledata[[1]]
   Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
    }
   else{
   Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
   Ref<-rbind(Singledata[[1]],Singledata[[4]])
   Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                       time=Ref$time, conc=Ref$conc)
   }
   SingleRdata<-Refdata[ do.call(order, Refdata) ,]
   if(parallel){
   Test<-rbind(Singledata[[2]])
   Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
    }
   else{
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                        time=Test$time, conc=Test$conc)
   }
   SingleTdata<-Testdata[ do.call(order, Testdata) ,]
}
}
##                    
## 2nd. compare SingleRdata & SingleTdata and to Show message if this is Incomplete_Dataset.
## 
R_subj_no<-SingleRdata$subj
R_subj_no<-unique(R_subj_no)
T_subj_no<-SingleTdata$subj
T_subj_no<-unique(T_subj_no)

###
### if it is icd, then stop running; AND must exclude parallel study for icd.check() since the subjects in parallel study
### will be totally different in Test and Ref. groups.
### use identical() here to compare two objects to see if they are the same. --YJ
###
### if(!identical(R_subj_no,T_subj_no) && !(parallel||replicated)) {
if(!identical(R_subj_no,T_subj_no) && !parallel) {
   cat("\n\n The Subject codes of taking Ref. product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(R_subj_no)
   cat("\n\n The Subject codes of taking Tets product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(T_subj_no)
   alarm();alarm()
   cat("\n\n The input data could be an incomplete dataset, \n")
   cat(" i.e., 'Subj of taking R' is not exactly same as 'Subj of taking Test'.\n")
   cat(" At the moment, bear cannot analyze an incomplete dataset.\n\n")
   readline(" Please Enter to go back the Top menu...\n");go2menu()}
else{
###
### it is not icd, go to analysis now
###
  if(NCA.only){  ### do 'NCA' only here
  if(parallel){
             if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
           }
           else{ 
            if (replicated){
              return(RepNCAanalyze(TotalSingledata))
             }
           else{
              if(multiple){
                 return(MultipleNCAanalyze(TotalSingledata))
                  }
               else{ 
                 return(NCAanalyze(TotalSingledata))
               } 
          }  
      }
  }
  else{   ### do 'NCA -> statistical analysis' here
  if(parallel){
            if(multiple){
            return(MultipleParaNCA.MIXanalyze(TotalSingledata))
            }
            else{
            return(ParaNCA.MIXanalyze(TotalSingledata))
             }
           }
        else{ 
         if(replicated){
              return(RepNCA.MIXanalyze(TotalSingledata))
             }
           else{
              if(multiple){
              return(MultipleNCA.BANOVAanalyze(TotalSingledata))
              }
              else{
              return(NCA.BANOVAanalyze(TotalSingledata))
             }  
          }
       }
    }
 }
###
}
