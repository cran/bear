##export with pdf file
NCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD,
                  replicated=FALSE,parallel=FALSE,multiple=FALSE)
{
options(warn=-1)
conc<-NULL
trt<-NULL
subj<-NULL
code<-NULL
ymax<-0
ymin<-0
cat(" Save PK plots from NCA analysis now...\n");readline(" It may take a while to complete.\n Press Enter to proceed...");cat("\n\n")

nca_plot_xfile<- nca_plot_xfile   ## to avoid "not visible binding..." error message with codetool
  
  R.split<-split(SingleRdata, list(SingleRdata$subj))
  T.split<-split(SingleTdata, list(SingleTdata$subj))

Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
s.split<-split(Totalplot,list(Totalplot$subj))
Totalplot$conc[Totalplot$conc == 0] <- NA
Totalplot <- na.omit(Totalplot)            ### Ahhh... here to remove conc. < BLLQ  --YJ

if(replicated){
     LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
                    time=Totalplot$time,  conc=Totalplot$conc, code=Totalplot$code)
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
          
     Totals.split<-split(TotalData, list(TotalData$drug))
     
     Fdata<-split(TotalData, list(TotalData$drug))
     RefData<-Fdata[[1]]
     TestData<-Fdata[[2]]
      SeqLeg<-split(RefData, list(RefData$seq))
      SeqLeg1 <- reshape(SeqLeg[[1]], idvar=c("subj", "drug","seq"), timevar =
      "prd",direction = "wide")
      SeqLeg2 <- reshape(SeqLeg[[2]], idvar=c("subj", "drug","seq"), timevar =
      "prd",direction = "wide")  
      L1<-length(SeqLeg1$subj)
      L2<-length(SeqLeg2$subj)
     
     prdcount<-length(levels(TotalData$prd))
       }
     else{ 
       if(parallel){        
        LR<-data.frame(subj=Totalplot$subj,  drug=Totalplot$drug, time=Totalplot$time,  conc=Totalplot$conc)
        Totals.split<-split(TotalData, list(TotalData$drug))
        L1<-length(Totals.split[[1]]$subj) #ref
        L2<-length(Totals.split[[2]]$subj) #test

        Totalplot.split<-split(Totalplot, list(Totalplot$subj))
        Totalplotpara<-split( Totalplot, list(Totalplot$drug))  
         if(multiple){
           SingleRdata0<-SingleRdata
           SingleRdata0$conc[SingleRdata0$conc == 0] <- NA
           SingleRdata1 <- na.omit(SingleRdata0)
           paraR.split<-split(SingleRdata1, list(SingleRdata1$subj))
         
            SingleTdata0<-SingleTdata
            SingleTdata0$conc[SingleTdata0$conc == 0] <- NA
            SingleTdata1 <- na.omit(SingleTdata0)
            paraT.split<-split(SingleTdata1, list(SingleTdata1$subj))
            Totalplot<- rbind(SingleRdata,SingleTdata)
        
        }
        else{
        
        paraR.split<-split( Totalplotpara[[1]], list(Totalplotpara[[1]]$subj))
        paraT.split<-split( Totalplotpara[[2]], list(Totalplotpara[[2]]$subj))
          }
        }
       else{
         LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
               time=Totalplot$time,  conc=Totalplot$conc)
         Totals.split<-split(TotalData, list(TotalData$drug))
         L<-length(Totals.split[[1]]$subj)
         
         }
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
     }

pdf(nca_plot_xfile, paper = "a4", bg = "white")

#0.
logo_plot_desc()
par(mai=c(1.3,2,1.3,1.8)) 
#1.#################################individual subject for test vs ref (Cp vs time)
if(parallel){
for(i in seq_along(R.split)){     ### for Ref. here; it's parallel design, no crossover!  no group, no trt, etc. -YJ
     if(multiple){
        xx1<-R.split[[i]]$time-TlastD
        yy1<-R.split[[i]]$conc     
        main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)
         }
      else{
        xx1<-R.split[[i]]$time
        yy1<-R.split[[i]]$conc     
        main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)
         }
      }                                                     
for(i in seq_along(T.split)){          ### for Test here
    if(multiple){
         xx1<-T.split[[i]]$time-TlastD
         yy1<-T.split[[i]]$conc     
         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         ###
         ggdata<-data.frame(time=xx1,conc=yy1)
         ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
         ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
         p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
         p<- p + scale_fill_discrete(guide=FALSE)
         p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
             labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
         p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                       axis.title.y=element_text(size=16,face="bold"))
         print(p)         
        }
     else{ 
         xx1<-T.split[[i]]$time
         yy1<-T.split[[i]]$conc     
         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         ###
         ggdata<-data.frame(time=xx1,conc=yy1)
         ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
         ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
         p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
         p<- p + scale_fill_discrete(guide=FALSE)
         p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
             labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
         p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                       axis.title.y=element_text(size=16,face="bold"))
         print(p)         
        }
      }                                     
}
else{
for(i in seq_along(s.split)){
    if(replicated){                    ### more than 2-way crossover!! some codes may not be applicable. -YJ
      xx1<-s.split[[i]]$time
      yy1<-s.split[[i]]$conc
      nprd<-s.split[[i]]$prd
      ndrug<-s.split[[i]]$drug
      
      main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
      ###    
      ggdata<-data.frame(time=xx1,conc=yy1,prd=nprd,drug=ndrug,trt=c(""),stringsAsFactors=F)
      for (i in seq_along(ggdata$time)) ggdata$trt[i]<-paste("Period# ",ggdata$prd[i]," ",
                                       ifelse(ggdata$drug[i]==1,"Ref.","Test"),sep="")
      ### show(ggdata);readline("")  ### works great! -YJ
      p<-ggplot(ggdata, aes(time,conc,group=trt,colour=trt,shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
      p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
          labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
      p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                    axis.title.y=element_text(size=16,face="bold"))
      p<- p + scale_colour_discrete(name="Treatment:-") +
               scale_shape_discrete(name ="Treatment:-",solid=FALSE)
      p<- p + theme(legend.title = element_text(size=14, face="bold"), legend.text=element_text(size = 12, face = "bold"))
      p<- p + theme(legend.position=c(0.85,0.85)) + theme(legend.background=element_blank()) +
              theme(legend.key=element_blank())    ### make legend has the same background as plot. -YJ
      print(p)
      }
      else{
        if(multiple){
         xx1<-s.split[[i]]$time-TlastD
         yy1<-s.split[[i]]$conc     
         main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
         ### 
         ggdata<-data.frame(time=xx1,conc=yy1,Treatment=as.factor(s.split[[i]]$drug),trt=c(""),stringsAsFactors=F)
         for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
         ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
         p<-ggplot(ggdata, aes(time,conc,group=trt,colour=trt,shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
         p<- p + scale_fill_discrete(guide=FALSE)
         p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
             labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
         p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                       axis.title.y=element_text(size=16,face="bold"))
         p<- p + scale_colour_discrete(name="Treatment:- ") +
                 scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
         p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
         p<- p + theme(legend.position="top")
         print(p)
        }
        else{
        xx1<-s.split[[i]]$time
        yy1<-s.split[[i]]$conc     
        main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")  ### try to use ggplot() here ... -YJ
        ###
        ggdata<-data.frame(time=s.split[[i]]$time,conc=s.split[[i]]$conc,Treatment=as.factor(s.split[[i]]$drug),
                           trt=c(""),stringsAsFactors=F)
        for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc,group=trt,colour=trt,shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        p<- p + scale_colour_discrete(name="Treatment:- ") +
                scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
        p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
        p<- p + theme(legend.position="top")
        print(p)
        }     
 }            
  
### if(replicated) prdcount(i,s.split, prdcount)  ### not needed any more! because it's very ugly. --YJ

  } 
}
#2.#####################################individual subject for test vs ref (lnCp vs time) -semilog plots (YJ)
if(parallel){
 for(i in seq_along(paraR.split)){
   if(multiple){  
     xx1<-paraR.split[[i]]$time-TlastD
     yy1<-paraR.split[[i]]$conc   
     main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_y_log10()
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)        
         }
      else{
        xx1<-paraR.split[[i]]$time
        yy1<-paraR.split[[i]]$conc     
        main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_y_log10()
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)        
        }
      }                                                     

for(i in seq_along(paraT.split)){
    if(multiple){  
     xx1<-paraT.split[[i]]$time-TlastD
     yy1<-paraT.split[[i]]$conc     
     main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_y_log10()
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)        
         }
      else
         {
        xx1<-paraT.split[[i]]$time
        yy1<-paraT.split[[i]]$conc     
        main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        ###
        ggdata<-data.frame(time=xx1,conc=yy1)
        ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
        ### write.csv(ggdata,file="ggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ
        p<-ggplot(ggdata, aes(time,conc)) ### ,linetype=trt))  ### OK now.  -YJ
        p<- p + scale_y_log10()
        p<- p + scale_fill_discrete(guide=FALSE)
        p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
            labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
        p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                      axis.title.y=element_text(size=16,face="bold"))
        print(p)        
        }
      }                                            
}
else{
 for(i in seq_along(Ls.split)){
  
   if(replicated){                    ### more than 2-way crossover!! some codes may not be applicable. -YJ
      xx1<-Ls.split[[i]]$time
      yy1<-Ls.split[[i]]$conc
      nprd<-Ls.split[[i]]$prd
      ndrug<-Ls.split[[i]]$drug
      main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
      ###
      ggdata<-data.frame(time=xx1,conc=yy1,prd=nprd,drug=ndrug,trt=c(""),stringsAsFactors=F)
      for (i in seq_along(ggdata$time)) ggdata$trt[i]<-paste("Period# ",ggdata$prd[i]," ",
                                       ifelse(ggdata$drug[i]==1,"Ref.","Test"),sep="")
      ### show(ggdata);readline("")  ### works great! -YJ
      p<-ggplot(ggdata, aes(time, conc, group=trt, colour=trt, shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
      p<- p + scale_y_log10()
      p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
          labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" "),size=16,face="bold"))   ### xlab, ylab works in this way. -YJ
      p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                    axis.title.y=element_text(size=16,face="bold"))
      p<- p + scale_colour_discrete(name="Treatment:-\n") +
               scale_shape_discrete(name ="Treatment:-\n",solid=FALSE)
      p<- p + theme(legend.title = element_text(size=14, face="bold"), legend.text=element_text(size = 12, face = "bold"))
      p<- p + theme(legend.position=c(0.85,0.85)) + theme(legend.background=element_blank()) +
              theme(legend.key=element_blank())    ### make legend has the same background as plot. -YJ
      print(p)
    }
    else{
      if(multiple){
       xx1<-Ls.split[[i]]$time-TlastD
       yy1<-Ls.split[[i]]$conc   
       main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
       ### 
       ggdata<-data.frame(time=xx1,conc=yy1,Treatment=as.factor(Ls.split[[i]]$drug),trt=c(""),stringsAsFactors=F)
       for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
       p<-ggplot(ggdata, aes(time,conc,group=trt,colour=trt,shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
       p<- p + scale_y_log10()
       p<- p + scale_fill_discrete(guide=FALSE)
       p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
           labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" "),size=16,face="bold"))   ### xlab, ylab works in this way. -YJ
       p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                     axis.title.y=element_text(size=16,face="bold"))
       p<- p + scale_colour_discrete(name="Treatment:- ") +
               scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
       p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
       p<- p + theme(legend.position="top")
       print(p)
        }
      else{
       xx1<-Ls.split[[i]]$time
       yy1<-Ls.split[[i]]$conc
       ###  write.csv(ggdata,file="Lggdata.csv",row.names=FALSE)    ### for testing with dataset before implemented. -YJ       
       main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
       ### lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$drug, cex = 1,
       ###    ,xlim=range(0, Ls.split[[i]]$time+ (Ls.split[[i]]$time/5)),ylim=range(1, 4*max(yy1)),
       ###    xlab = "Time", ylab = "Drug Plasma Conc. (log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
       ###    font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
       ###  xtick(xx1) #tick for x-axis
       ggdata<-data.frame(time=Ls.split[[i]]$time,conc=Ls.split[[i]]$conc,Treatment=as.factor(Ls.split[[i]]$drug),
                          trt=c(""),stringsAsFactors=F)
       for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
       p<-ggplot(ggdata, aes(time,conc,group=trt,colour=trt,shape=trt)) ### ,linetype=trt))  ### OK now.  -YJ
       p<- p + scale_y_log10()
       p<- p + scale_fill_discrete(guide=FALSE)
       p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
           labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" "),size=16,face="bold"))   ### xlab, ylab works in this way. -YJ
       p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                     axis.title.y=element_text(size=16,face="bold"))
       p<- p + scale_colour_discrete(name="Treatment:- ") +
               scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
       p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
       p<- p + theme(legend.position="top")
       print(p)
        }
      } 
      
  ### if(replicated) prdcount(i,s.split, prdcount)  ### not needed any more! because it's very ugly. --YJ
  
 }
}
#3. ###############################show all subjects in one Spaghetti plot for test
## main1<-"Spaghetti plot - Test Product"    ### cause err!
if(replicated){
xx1<-SingleTdata$time
yy1<-SingleTdata$conc
###
   ggdata<-data.frame(code=SingleTdata$code,time=xx1,conc=yy1)
   ### show(ggdata);readline("")
   ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
   main<-"Spaghetti plot - Test Product\n"
   p<-ggplot(ggdata, aes(time, conc, group=code)) ### ,linetype=trt))  ### need to fix here.  -YJ
   ### p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + theme(legend.position="none")
   p<- p + geom_line(size=0.5) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   ### log scale plot  -YJ
   main<-"Spaghetti plot - Test Product \n(as log10(Conc.) vs.Time)"
   p<-ggplot(ggdata, aes(time, conc, group=code,)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_y_log10()
   ### p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + theme(legend.position="none")
   p<- p + geom_line(colour="blue", size=0.5) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
}
else{
   if(multiple){
   xx1<-SingleTdata$time-TlastD
   yy1<-SingleTdata$conc  
   ###
   ggdata<-data.frame(subj=SingleTdata$subj,time=xx1,conc=yy1)
   ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
   main<-"Spaghetti plot - Test Product\n"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(size=1) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   ### log scale plot  -YJ
   main<-"Spaghetti plot - Test Product \n(as log10(Conc.) vs.Time)"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_y_log10()
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(colour="blue", size=1) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   }
   else{
    xx1<-SingleTdata$time
    yy1<-SingleTdata$conc 
    ###
    ggdata<-data.frame(subj=SingleTdata$subj,time=SingleTdata$time,conc=SingleTdata$conc)
    ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
    main<-"Spaghetti plot - Test Product\n"
    p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_line(size=1) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
              axis.title.y=element_text(size=16,face="bold"))
    print(p)
    ### log scale plot  -YJ
    main<-"Spaghetti plot - Test Product \n(as log10(Conc.) vs.Time)"
    p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_line(colour="blue", size=1) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
              axis.title.y=element_text(size=16,face="bold"))
    print(p)
      }
    }         

#4. ###############################show all subjects in one plot at the same time for ref
### main1<-"Spaghetti plot - Ref. Product"  ### cause err! don't know why? YJ
###
if(replicated){
   xx1<-SingleRdata$time
   yy1<-SingleRdata$conc  
   ###  
   ggdata<-data.frame(code=SingleRdata$code,time=xx1,conc=yy1)
   ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
   main<-"Spaghetti plot - Ref. Product\n"
   p<-ggplot(ggdata, aes(time, conc, group=code)) ### ,linetype=trt))  ### need to fix here.  -YJ
   ### p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + theme(legend.position="none")
   p<- p + geom_line(size=0.5) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   ### log scale plot  -YJ
   main<-"Spaghetti plot - Ref. Product \n(as log10(Conc.) vs.Time)"
   p<-ggplot(ggdata, aes(time, conc, group=code)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_y_log10()
   ### p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + theme(legend.position="none")
   p<- p + geom_line(colour="blue", size=0.5) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
}
else{
   if(multiple){
   xx1<-SingleRdata$time -TlastD
   yy1<-SingleRdata$conc  
   ###
   ggdata<-data.frame(subj=SingleRdata$subj,time=xx1,conc=yy1)
   ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
   main<-"Spaghetti plot - Ref. Product\n"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(size=1) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   ### log scale plot  -YJ
   main<-"Spaghetti plot - Ref. Product \n(as log10(Conc.) vs.Time)"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_y_log10()
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(colour="blue", size=1) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   }
   else{
   xx1<-SingleRdata$time
   yy1<-SingleRdata$conc  
   ###
   ggdata<-data.frame(subj=SingleRdata$subj,time=SingleRdata$time,conc=SingleRdata$conc)
   ### write.csv(ggdata,file="spggdata.csv",row.names=FALSE)
   main<-"Spaghetti plot - Ref. Product\n"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(size=1) + labs(list(title = main,x=xaxis,y=yaxis))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
   ### log scale plot  -YJ
   main<-"Spaghetti plot - Ref. Product \n(as log10(Conc.) vs.Time)"
   p<-ggplot(ggdata, aes(time, conc, group=subj)) ### ,linetype=trt))  ### need to fix here.  -YJ
   p<- p + scale_y_log10()
   p<- p + scale_fill_discrete(guide=FALSE)
   p<- p + geom_line(colour="blue", size=1) + labs(list(title = main,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))  ### xlab, ylab works in this way. -YJ
   p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
             axis.title.y=element_text(size=16,face="bold"))
   print(p)
  }
}             


#5. ########################## mean+*-sd plot for test and ref drug  ## lineplot.CI() is mean+/- se (not sd!) --YJ
### semilog plots here and followed by linear plots  -YJ

if(replicated){
main1<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 scale), N=",L1+L2),collapse=" ")
main2<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 scalewith SD), N=",L1+L2),collapse=" ")
xx1<-Totalplot$time
yy1<-Totalplot$conc
yy1<-na.omit(yy1)

##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
## 
##
    ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    ??? what's for? I don't know. -YJ
    dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ###, position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    ### p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + theme(legend.position="none")
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)

###
### show error bar with whiscker bar
###
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ### , position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
}
else{
 if(parallel){
 main1<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 Scale, N =",L1+L2),collapse=" ")
 main2<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 Scale with SD), N =",L1+L2),collapse=" ")
    if(multiple){                       ### very special case with this; don't work; data caused? so plot as linear (not semilog plot!  YJ)
    xx1<-Totalplot$time -TlastD
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    Ymax=max(yy1)
    Xmax=max(xx1)
##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
##     
    ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ###, position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=yaxis))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ###, position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)    
                }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)

    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ##
    ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ### , position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=0.) ###, position = pd) +  ### here we use +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)    
    }
  }
 else{
  main1<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 Scale), N =",L),collapse=" ")
  main2<-paste(c("Obs. Mean Drug Plasma Conc. \n(as log10 Scale, with SD), N =",L),collapse=" ")
    if(multiple){
       Totalplot[Totalplot$conc<1] <-NA
       xx1<-Totalplot$time-TlastD 
       yy1<-Totalplot$conc
       yy1<-na.omit(yy1)
       
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.)  ### , position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ### 
    ### show the error bar with this plot   --YJ same for ggplot() too.
    ###
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ### , position = pd) +  ### here we omit +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ### 
    ### show the error bar with this plot   --YJ same for ggplot() too.
    ###
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    p<- p + scale_y_log10()
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    ### p<- p + theme(axis.ticks=element_line(size=14))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    }   
   }
 }

###
### linear plots from here
###

if(replicated){
main1<-paste(c("Obs. Mean Drug Plasma Conc., \nN=",L1+L2),collapse=" ")
main2<-paste(c("Obs. Mean Drug Plasma Conc., \n(with SD), N=",L1+L2),collapse=" ")
xx1<-Totalplot$time
yy1<-Totalplot$conc
yy1<-na.omit(yy1)

##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
## 
## 
    ### the following 5 lines may not req. since they have been calc. previously. --YJ
    ###
    ### ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    ### dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.)  ### , position = pd) +  ### here we omit +/- 'sd' --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ### p<- p + scale_y_log10()      ### for linear plot
    ### p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + theme(legend.position="none")
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)

###
### show error bar with whiscker bar
###
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ###  p<- p + scale_y_log10()                      ### for linear plot. -YJ
    ### p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + theme(legend.position="none")
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
}
else{
 if(parallel){
 main1<-paste(c("Obs. Mean Drug Plasma Conc., \nN =",L1+L2),collapse=" ")
 main2<-paste(c("Obs. Mean Drug Plasma Conc., \n(with SD), N =",L1+L2),collapse=" ")
    if(multiple){                       ### very special case with this; don't work; data caused? so plot as linear (not semilog plot!  YJ)
    xx1<-Totalplot$time -TlastD
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    Ymax=max(yy1)
    Xmax=max(xx1)
##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
##     
    ### the following 5 lines may not req. since they have been calc. previously. --YJ
    ###
    ### ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    ### dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.)  ### , position = pd) +  ### here we omit +/- 'sd' --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ### p<- p + scale_y_log10()      ### for linear plot
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ##        
    ## show the error bar with this plot
    ## 
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ### , position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ###  p<- p + scale_y_log10()                      ### for linear plot. -YJ
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)    
                }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)

    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ### the following 5 lines may not req. since they have been calc. previously. --YJ
    ###
    ### ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    ### dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ###, position = pd) +  ### here we omit +/- 'sd' --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ### p<- p + scale_y_log10()      ### for linear plot
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ##        
    ## show the error bar with this plot
    ## 
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ###  p<- p + scale_y_log10()                      ### for linear plot. -YJ
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)    
    }
  }
 else{
  main1<-paste(c("Obs. Mean Drug Plasma Conc., \nN =",L),collapse=" ")
  main2<-paste(c("Obs. Mean Drug Plasma Conc., \n(with SD), N =",L),collapse=" ")
    if(multiple){
       Totalplot[Totalplot$conc<1] <-NA
       xx1<-Totalplot$time-TlastD 
       yy1<-Totalplot$conc
       yy1<-na.omit(yy1)
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ### the following 5 lines may not req. since they have been calc. previously. --YJ
    ###
    ### ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    ### dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.) ###, position = pd) +  ### here we omit +/- 'sd' --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ### p<- p + scale_y_log10()      ### for linear plot
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ##        
    ## show the error bar with this plot
    ## 
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ###  p<- p + scale_y_log10()                      ### for linear plot. -YJ
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    ### the following 5 lines may not req. since they have been calc. previously. --YJ
    ###
    ### ggdata<-data.frame(Treatment=Totalplot$drug,time=xx1,conc=yy1,trt=c(""),stringsAsFactors=F)
    ### for (i in 1:length(ggdata$time)){if(ggdata$Treatment[i]==1) ggdata$trt[i]<-"Ref." else ggdata$trt[i]<-"Test"}
    # The errorbars overlapped, so use position_dodge to move them horizontally; from Cookbook for R.
    ### pd <- position_dodge(.1)  # move them .05 to the left and right    
    ### dfc <- summarySE(ggdata, measurevar="conc", groupvars=c("time","trt"))
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc, ymax=conc), width=0.)  ### , position = pd) +  ### here we omit +/- 'sd' --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ### p<- p + scale_y_log10()      ### for linear plot
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main1,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    ##        
    ## show the error bar with this plot
    ## 
    p<-ggplot(dfc, aes(x=time,y=conc,group=trt,colour=trt,shape=trt)) + ### ,linetype=trt))  ### OK now.  -YJ
              geom_errorbar(aes(ymin=conc-sd, ymax=conc+sd), width=1.5) ###, position = pd) +  ### here we used +/- 'sd', not 'se'!!  --YJ
              ### geom_line(position=pd) + geom_point(position=pd)
    ###  p<- p + scale_y_log10()                      ### for linear plot. -YJ
    p<- p + scale_fill_discrete(guide=FALSE)
    p<- p + geom_point(colour="black",size=3) + geom_line(size=1) + ### scale_shape(solid = FALSE) + 
        labs(list(title = main2,x=xaxis,y=paste(yaxis,"(as log10 Scale)",sep=" ")))   ### xlab, ylab works in this way. -YJ
    p<- p + theme(plot.title=element_text(size=16,face="bold"),axis.title.x=element_text(size=16,face="bold"),
                  axis.title.y=element_text(size=16,face="bold"))
    p<- p + scale_colour_discrete(name="Treatment:- ") +
            scale_shape_discrete(name ="Treatment:- ",solid=FALSE)
    p<- p + theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size = 14, face = "bold"))
    p<- p + theme(legend.position="top")
    print(p)
    }   
   }
 }
dev.off()
graphics.off()
}