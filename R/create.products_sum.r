###
### 1. functions to create "sum_all_ref.csv" & "sum_all_test.csv"
### 2. will be called by NCAanalyze(), demomenu(), demomenu1() & RepNCAanalyze()  --YJ
### 3. create .csv files will be loaded back in NCAoutput()
### 4. can be different with replicate BE...? no! it's the same.
### 
create.products_sum<-function(Totalplot){
###
### prepare for save sum_all_by_product.csv here... YJ
TKK<-Totalplot
### dump ref data
SS_ref<-subset(TKK,TKK$drug==1)
timeXX<-SS_ref$time
timeXX<-unique(sort(timeXX,method = "sh"))
product_F<-as.factor(SS_ref$time)   
### get mean & sd by time as factor
mean.conc.ref<-tapply(SS_ref$conc,product_F, mean, na.rm=TRUE)
SD.conc.ref<-tapply(SS_ref$conc,product_F, sd, na.rm=TRUE)
xx<-cbind(mean.conc.ref,SD.conc.ref)
xx<-as.data.frame(xx)
### dump test data
SS_test<-subset(TKK,TKK$drug==2)
timeZZ<-SS_test$time
timeZZ<-unique(sort(timeZZ,method = "sh"))
product_F<-as.factor(SS_test$time)
mean.conc.test<-tapply(SS_test$conc,product_F, mean, na.rm=TRUE)
SD.conc.test<-tapply(SS_test$conc,product_F, sd, na.rm=TRUE)
yy<-cbind(mean.conc.test,SD.conc.test)
yy<-as.data.frame(yy)
sum_all_ref<-data.frame(time=formatC(timeXX,format="f",digits=3),mean_Ref=formatC(xx$mean.conc.ref,format="f",digits=3),
                        SD_Ref=formatC(xx$SD.conc.ref,format="f",digits=3))
sum_all_test<-data.frame(time=formatC(timeZZ,format="f",digits=3),mean_Test=formatC(yy$mean.conc.test,format="f",digits=3),
                         SD_Test=formatC(yy$SD.conc.test,format="f",digits=3))
write.csv(sum_all_ref,file="sum_all_ref.csv",row.names=FALSE)  
write.csv(sum_all_test,file="sum_all_test.csv",row.names=FALSE) ### <-- it's working!!  YJ
###
}