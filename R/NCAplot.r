##export with pdf file
library(sciplot)
NCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)
{
options(warn=-1)
d.split<-split(Totalplot,list(Totalplot$drug))
s.split<-split(Totalplot,list(Totalplot$subj))

LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
               time=Totalplot$time,  conc=Totalplot$conc)
LR$conc[LR$conc == 0] <- NA
LR <- na.omit(LR)
Ls.split<-split(LR, list(LR$subj))

Totals.split<-split(TotalData, list(TotalData$drug))
L<-length(Totals.split[[1]]$subj)


pdf("NCAplots.pdf")

#0.
description_plot()

#1.#################################individual subject for test vs ref (Cp vs time)

s.split<-split(Totalplot,list(Totalplot$subj))
for(i in seq_along(s.split)){
main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$drug, cex = 1,
            xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n"
             )
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
               105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
   axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  
 }

#2.#####################################individual subject for test vs ref (Cp vs time)

for(i in seq_along(Ls.split)){
main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$drug, cex = 1,
     xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
     font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n"
             )
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  
 }

#3. ###############################show all subjects in one plot at the same time for test

lineplot.CI(SingleTdata$time, SingleTdata$conc, group = SingleTdata$subj, type="l",
           xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" 
             )
            axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                       105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
            axis(1,at=0:100,tcl=-.2, labels=FALSE)
            axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
            mtext("Test",side=3,cex=2,las=0)  #要放在plot之後
 

#4. ###############################show all subjects in one plot at the same time for ref

lineplot.CI(SingleRdata$time, SingleRdata$conc, group = SingleRdata$subj, type="l",
            xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n" 
             )
            axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                        105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
            axis(1,at=0:100,tcl=-.2, labels=FALSE)
            axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
            mtext("Ref.",side=3,cex=2,las=0)  #要放在plot之後


#5. ##########################mean+*-sd plot for test and ref drug

main<-paste(c("Observed mean drug plasma concentration, N=",L),collapse=" ")
lineplot.CI(Totalplot$time, Totalplot$conc, group = Totalplot$drug, cex = 1, main=main,
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05
             )
            axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                       105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
            axis(1,at=0:100,tcl=-.2, labels=FALSE)
            axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)


dev.off()

}