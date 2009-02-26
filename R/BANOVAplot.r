library(plotrix) 
BANOVAplot<-function(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
                     IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                     IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
                     IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
{

#par(mfrow=c(2,2))
##lnCmax
par(mai=c(1.3,2,1.3,1.8)) 
qqnorm(IntraInterlnCmax00$Stud_Intra,
main = "Normal Probability Plot of lnCmax (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnCmax00$Stud_Inter,
main = "Normal Probability Plot of lnCmax (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnCmaxseq11_plotlabels<-c(IntraInterlnCmaxseq11$subj)
#lnCmaxseq22_plotlabels<-c(IntraInterlnCmaxseq22$subj)
#show(IntraInterlnCmaxseq11$subj)
#show(IntraInterlnCmaxseq11)
#show(lnCmaxseq11_plotlabels)

##intra-subject residuals: lnCmax(expected value) vs studentized residuals 
x <- IntraInterlnCmax00[,3]
y <- IntraInterlnCmax00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnCmax (expected value)", ylab="studentized residuals", 
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnCmaxseq11$Exp, IntraInterlnCmaxseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnCmaxseq11$Exp,IntraInterlnCmaxseq11$Stud_Intra,IntraInterlnCmaxseq11$subj, col="black",text.pos=TRUE) 

points(IntraInterlnCmaxseq22$Exp, IntraInterlnCmaxseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnCmaxseq22$Exp,IntraInterlnCmaxseq22$Stud_Intra,IntraInterlnCmaxseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnCmax(expected value) vs studentized residuals 
x <- IntraInterlnCmax00[,3]
y <- IntraInterlnCmax00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnCmax (expected value)", ylab="studentized residuals", 
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnCmaxseq11$Exp, IntraInterlnCmaxseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnCmaxseq11$Exp,IntraInterlnCmaxseq11$Stud_Inter,IntraInterlnCmaxseq11$subj, col="black",text.pos=TRUE) 

points(IntraInterlnCmaxseq22$Exp, IntraInterlnCmaxseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnCmaxseq22$Exp,IntraInterlnCmaxseq22$Stud_Inter,IntraInterlnCmaxseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

#lnAUC0t
qqnorm(IntraInterlnAUC0t00$Stud_Intra,
main = "Normal Probability Plot of lnAUC0t (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnAUC0t00$Stud_Inter,
main = "Normal Probability Plot of lnAUC0t (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnAUC0tseq11_plotlabels<-c(IntraInterlnAUC0tseq11$subj)
#lnAUC0tseq22_plotlabels<-c(IntraInterlnAUC0tseq22$subj)
##intra-subject residuals: lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0t (expected value)", ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0tseq11$Exp, IntraInterlnAUC0tseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0tseq11$Exp,IntraInterlnAUC0tseq11$Stud_Intra,IntraInterlnAUC0tseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0tseq22$Exp, IntraInterlnAUC0tseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0tseq22$Exp,IntraInterlnAUC0tseq22$Stud_Intra,IntraInterlnAUC0tseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0t (expected value)", ylab="studentized residuals",
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0tseq11$Exp, IntraInterlnAUC0tseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0tseq11$Exp,IntraInterlnAUC0tseq11$Stud_Inter,IntraInterlnAUC0tseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0tseq22$Exp, IntraInterlnAUC0tseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0tseq22$Exp,IntraInterlnAUC0tseq22$Stud_Inter,IntraInterlnAUC0tseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)
               
#lnAUC0inf
qqnorm(IntraInterlnAUC0INF00$Stud_Intra,
main = "Normal Probability Plot of lnAUC0INF (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnAUC0INF00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnAUC0INF00$Stud_Inter,
main = "Normal Probability Plot of lnAUC0INF (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnAUC0INF00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnAUC0INFseq11_plotlabels<-c(IntraInterlnAUC0INFseq11$subj)
#lnAUC0INFseq22_plotlabels<-c(IntraInterlnAUC0INFseq22$subj)
##intra-subject residuals: lnAUC0INF(expected value) vs studentized residuals
x <- IntraInterlnAUC0INF00[,3]
y <- IntraInterlnAUC0INF00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0INF (expected value)", ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0INFseq11$Exp, IntraInterlnAUC0INFseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Intra,IntraInterlnAUC0INFseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Intra,IntraInterlnAUC0INFseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnAUC0INF(expected value) vs studentized residuals
x <- IntraInterlnAUC0INF00[,3]
y <- IntraInterlnAUC0INF00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0INF (expected value)", ylab="studentized residuals",
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0INFseq11$Exp, IntraInterlnAUC0INFseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Inter,IntraInterlnAUC0INFseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Inter,IntraInterlnAUC0INFseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##plot boxplot
par(mai=c(1,0.8,1,0.5)) 
op <- par(mfrow=c(1,3))

lnCmaxintra<-boxplot(IntraInterlnCmax00$Stud_Intra,col="lightgray", main="lnCmax (intrasubj)", ylab="studentized residuals",las=1)
  if (length(lnCmaxintra$out) > 0){ 
  z<-NULL 
   for(i in seq_along(lnCmaxintra$out)){
    z[i]<-IntraInterlnCmax00$subj[IntraInterlnCmax00$Stud_Intra==lnCmaxintra$out[i]] 
     } 
   text(lnCmaxintra$group, lnCmaxintra$out, IntraInterlnCmax00$subj[z],pos = 2 ) 
   } 

lnAUC0tintra<-boxplot(IntraInterlnAUC0t00$Stud_Intra,col="lightgray", main="lnAUC0t (intrasubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0tintra$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0tintra$out)){
    z[i]<-IntraInterlnAUC0t00$subj[IntraInterlnAUC0t00$Stud_Intra==lnAUC0tintra$out[i]] 
       } 
   text(lnAUC0tintra$group, lnAUC0tintra$out,IntraInterlnAUC0t00$subj[z],pos = 2 ) 
   } 

lnAUC0INFintra<-boxplot(IntraInterlnAUC0INF00$Stud_Intra,col="lightgray", main="lnAUC0INF (intrasubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0INFintra$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0INFintra$out)){
    z[i]<-IntraInterlnAUC0INF00$subj[IntraInterlnAUC0INF00$Stud_Intra==lnAUC0INFintra$out[i]] 
       } 
   text(lnAUC0INFintra$group, lnAUC0INFintra$out,IntraInterlnAUC0INF00$subj[z],pos = 2 ) 
   } 
   
lnCmaxinter<-boxplot(IntraInterlnCmax00$Stud_Inter,col="lightgray", main="lnCmax (intersubj)", ylab="studentized residuals",las=1)
if (length(lnCmaxinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnCmaxinter$out)){
    z[i]<-IntraInterlnCmax00$subj[IntraInterlnCmax00$Stud_Inter==lnCmaxinter$out[i]] 
       } 
   text(lnCmaxinter$group, lnCmaxinter$out,IntraInterlnCmax00$subj[z],pos = 2 ) 
   } 
   
lnAUC0tinter<-boxplot(IntraInterlnAUC0t00$Stud_Inter,col="lightgray", main="lnAUC0t (intersubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0tinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0tinter$out)){
    z[i]<-IntraInterlnAUC0t00$subj[IntraInterlnAUC0t00$Stud_Inter==lnAUC0tinter$out[i]] 
       } 
   text(lnAUC0tinter$group, lnAUC0tinter$out,IntraInterlnAUC0t00$subj[z],pos = 2 ) 
   } 
   
lnAUC0INFinter<-boxplot(IntraInterlnAUC0INF00$Stud_Inter,col="lightgray", main="lnAUC0INF (intersubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0INFinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0INFinter$out)){
    z[i]<-IntraInterlnAUC0INF00$subj[IntraInterlnAUC0INF00$Stud_Inter==lnAUC0INFinter$out[i]] 
       } 
   text(lnAUC0INFinter$group, lnAUC0INFinter$out,IntraInterlnAUC0INF00$subj[z],pos = 2 ) 
   } 

}