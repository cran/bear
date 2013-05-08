###
### 1. functions to create "sum_all_ref.csv" & "sum_all_test.csv"
### 2. will be called by NCAanalyze(), demomenu(), demomenu1() & RepNCAanalyze()  --YJ
### 3. create .csv files will be loaded back in NCAoutput()
### 4. can be different with replicate BE...; no! it's the same.
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
mean.conc.ref<-tapply(SS_ref$conc,product_F, mean)
SD.conc.ref<-tapply(SS_ref$conc,product_F, sd)
xx<-cbind(mean.conc.ref,SD.conc.ref)
xx<-as.data.frame(xx)
### dump test data
SS_test<-subset(TKK,TKK$drug==2)
timeZZ<-SS_test$time                                                                                                                
timeZZ<-unique(sort(timeZZ,method = "sh"))
product_F<-as.factor(SS_test$time)
mean.conc.test<-tapply(SS_test$conc,product_F, mean)
SD.conc.test<-tapply(SS_test$conc,product_F, sd)
yy<-cbind(mean.conc.test,SD.conc.test)
yy<-as.data.frame(yy)
sum_all_ref<-data.frame(time=timeXX,mean_Ref=xx$mean.conc.ref,SD_Ref=xx$SD.conc.ref)
sum_all_test<-data.frame(time=timeZZ,mean_Test=yy$mean.conc.test,SD_Test=yy$SD.conc.test)
write.csv(sum_all_ref,file="sum_all_ref.csv",row.names=FALSE)  
write.csv(sum_all_test,file="sum_all_test.csv",row.names=FALSE) ### <-- it's working!!  YJ
###
}