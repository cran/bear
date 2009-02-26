#subject plot for individual person for BA and ln(BA) -->replicated study
plotsingle.Rep<-function(Ls.split, s.split,xaxis,yaxis,i, prdcount )
{
#ln(BA)
 for(i in seq_along(Ls.split)){
main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$code, cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
     #  points(Ls.split[[i]]$time,  Ls.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)]
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
   prdcount(i,s.split, prdcount)
  }
 #BA
 for(i in seq_along(s.split)){
 main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
     lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$code, cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
    # points(s.split[[i]]$time, s.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)]
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
               105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
   axis(2,yaxp=c(0, 10000, 100),las=1,tcl=-.2, labels=FALSE)
    prdcount(i,s.split, prdcount)
 }
}