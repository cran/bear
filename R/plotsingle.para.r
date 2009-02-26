#subject plot for individual person for BA and ln(BA) -->parallel study
plotsingle.para<-function(R.split, T.split, paraR.split,paraT.split,xaxis,yaxis  )
{
#for ln (BA)
 for(i in seq_along(paraR.split)){
     xx1<-paraR.split[[i]]$time
     yy1<-paraR.split[[i]]$conc     
     main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=2)
        
        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1,000,000"),
               lty = 2, xjust = 1, yjust = 1)
      }                                                     
#Test
for(i in seq_along(paraT.split)){
     xx1<-paraT.split[[i]]$time
     yy1<-paraT.split[[i]]$conc     
     main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=1)
        
        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        
        temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1,000,000"),
               lty = 1, xjust = 1, yjust = 1)
      }                                            

 #for(BA)
for(i in seq_along(R.split)){
     xx1<-R.split[[i]]$time
     yy1<-R.split[[i]]$conc     
     main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, xlim=range(xx1), ylim=range(yy1), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=2)
        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        axis(2,yaxp=c(0, 10000, 100),las=1,tcl=-.2, labels=FALSE)
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1,000,000"),
               lty = 2, xjust = 1, yjust = 1)
      }                                                    
for(i in seq_along(T.split)){
     xx1<-T.split[[i]]$time
     yy1<-T.split[[i]]$conc     
     main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, xlim=range(xx1), ylim=range(yy1), xcex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        lines(xx1,yy1, lty=1)
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        axis(2,yaxp=c(0, 10000, 100),las=1,tcl=-.2, labels=FALSE)
        temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1,000,000"),
               lty = 1, xjust = 1, yjust = 1)
      }                                  
 }