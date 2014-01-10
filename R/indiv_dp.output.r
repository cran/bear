###
### this function will be callled by NCAanalyze(), demomenu() and demomenu1().
### it has not been implemented on replicate crossover yet.
### works for single-dose/multiple-dose 2x2x2, parallel single-dose/multiple-dose.
### -YJ
indiv_dp.output<-function(SingleRTdata)
{
options(warn=-1)
IDP_output_ref<-IDP_output_ref
IDP_output_test<-IDP_output_test

cat("\n\n** WARNING ***: if bear crashes at the next step, please check your data or\n  disable IDP output function and try again.\n\n")
df<-SingleRTdata
con<-as.data.frame(matrix(0,ncol=max(unique(df$subj))+3, nrow=length(unique(df$time))))
x<-c("Time",c(1:max(unique(df$subj))),"Mean","SD")   ### here should use max() instead of length(); length() does not work in parallel design. -YJ
colnames(con)<-x
con["Time"]<-formatC(unique(df$time),format="f",digits=3)
for(i in 1:max(unique(df$subj))+1){          ### this need to starting from '1' not '2'!  -YJ
   con[i]<-subset(df$conc,df$subj==(i-1))    ### this works well but cannot be explained at all.  weired...  -YJ
### for(i in 1:length(unique(df$subj))){     ### this doesn't work but looks more reasonable than previous.  -YJ
###    con[i]<-subset(df$conc,df$subj==i)
}
time_F<-as.factor(df$time)                ### not 'con$time' here! --YJ
### get mean & sd by time as factor
mean.conc.ref<-tapply(df$conc,time_F,mean,na.rm=TRUE)      ### 'na.rm=TRUE' is used to exclude data with 'NA'
SD.conc.ref<-tapply(df$conc,time_F,sd,na.rm=TRUE)          ### 'na.rm=TRUE' is used to exclude data with 'NA'
xx<-cbind(mean.conc.ref,SD.conc.ref)
xx<-as.data.frame(xx)
con["Mean"]<-formatC(xx$mean.conc.ref,format="f",digits=3)
con["SD"]<-formatC(xx$SD.conc.ref,format="f",digits=3)
### the following two lines (both works well and same!) are used to trim the column with all 'NA', especially for parallel design.
### con<-con[,! apply(con,2,function(x) all(is.na(x)))]   ### works great for parallel design. delete the column with all 'NA'. -YJ
con<-con[,colSums(is.na(con)) != nrow(con)]  ### works great too for parallel design. delete the column with all 'NA'. -YJ
### above line -> If the count of NAs in a column is equal to the number of rows, it must be entirely NA.
###
if(df$drug[1]==1){
write.csv(con,IDP_output_ref,row.names=FALSE)}
else{
write.csv(con,IDP_output_test,row.names=FALSE)}
}