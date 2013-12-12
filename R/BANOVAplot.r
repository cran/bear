### this function is for outlier detection plotting. nothing to do with anova... --YJ
### library(plotrix)   <-- no need for this line! --YJ
### called by BANOVAoutput()   --YJ
BANOVAplot<-function(IntraInterlnCmax00,IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
                     IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                     IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
                     IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22,TotalData,
                     IntraInterlnpAUC00,IntraInterlnpAUCseq11,IntraInterlnpAUCseq22,
                     multiple=FALSE)
{
pAUC<-pAUC               ### for pAUC

#par(mfrow=c(2,2))
##lnCmax
par(mai=c(1.3,2,1.3,1.8))
if(multiple){
a <- "Normal Probability Plot of lnCmax_ss (intrasubj)"
b <- "Normal Probability Plot of lnCmax_ss (intersubj)"
c <- "lnCmax_ss (expected value)"
a1 <- "Normal Probability Plot of lnAUC(tau)ss (intrasubj)"
b1 <- "Normal Probability Plot of lnAUC(tau)ss (intersubj)"
c1 <- "lnAUC(tau)ss (expected value)"
 }
else{
a <- "Normal Probability Plot of lnCmax (intrasubj)"
b <- "Normal Probability Plot of lnCmax (intersubj)"
c<-  "lnCmax (expected value)" 
a1 <- "Normal Probability Plot of lnAUC0t (intrasubj)"
b1 <- "Normal Probability Plot of lnAUC0t (intersubj)"
c1<-  "lnAUC0t (expected value)" 
}

if(pAUC){
a2 <- "Normal Probability Plot of lnpAUC (intrasubj)"
b2 <- "Normal Probability Plot of lnpAUC (intersubj)"
c2 <- "lnpAUC (expected value)"
}

qqnorm(IntraInterlnCmax00$Stud_Intra,
main = a,
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnCmax00$Stud_Inter,
main = b,
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnCmaxseq11_plotlabels<-c(IntraInterlnCmaxseq11$subj)
#lnCmaxseq22_plotlabels<-c(IntraInterlnCmaxseq22$subj)
#show(IntraInterlnCmaxseq11$subj)
#show(IntraInterlnCmaxseq11);readline("...Pause here")
#show(lnCmaxseq11_plotlabels)

##intra-subject residuals: lnCmax(expected value) vs studentized residuals 
x <- IntraInterlnCmax00[,3]
y <- IntraInterlnCmax00[,5]

plot(x, y,asp=NA,col="lightgray", xlab=c, ylab="studentized residuals", 
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

plot(x, y,asp=NA,col="lightgray", xlab=c, ylab="studentized residuals", 
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
main = a1,
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnAUC0t00$Stud_Inter,
main = b1,
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnAUC0tseq11_plotlabels<-c(IntraInterlnAUC0tseq11$subj)
#lnAUC0tseq22_plotlabels<-c(IntraInterlnAUC0tseq22$subj)
##intra-subject residuals: lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,5]

plot(x, y,asp=NA,col="lightgray", xlab=c1, ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0tseq11$Exp, IntraInterlnAUC0tseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0tseq11$Exp,IntraInterlnAUC0tseq11$Stud_Intra,IntraInterlnAUC0tseq11$subj,
                   col="black",text.pos=TRUE)

points(IntraInterlnAUC0tseq22$Exp, IntraInterlnAUC0tseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0tseq22$Exp,IntraInterlnAUC0tseq22$Stud_Intra,IntraInterlnAUC0tseq22$subj,
                   col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               ### pch = 15:16, col = 4:5, xjust = 1, yjust = 1)
               pch = c(15,16), col = c(4,5), xjust = 1, yjust = 1)

##intersubject; lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,7]

plot(x, y,asp=NA,col="lightgray", xlab=c1, ylab="studentized residuals",
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

### pAUC
if(pAUC){
qqnorm(IntraInterlnpAUC00$Stud_Intra,
main = a2,
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnpAUC00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnpAUC00$Stud_Inter,
main = b2,
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnpAUC00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

#lnpAUCseq11_plotlabels<-c(IntraInterlnpAUCseq11$subj)
#lnpAUCseq22_plotlabels<-c(IntraInterlnpAUCseq22$subj)
##intra-subject residuals: lnpAUC(expected value) vs studentized residuals
x <- IntraInterlnpAUC00[,3]
y <- IntraInterlnpAUC00[,5]

plot(x, y,asp=NA,col="lightgray", xlab=c2, ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnpAUCseq11$Exp, IntraInterlnpAUCseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnpAUCseq11$Exp,IntraInterlnpAUCseq11$Stud_Intra,IntraInterlnpAUCseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnpAUCseq22$Exp, IntraInterlnpAUCseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnpAUCseq22$Exp,IntraInterlnpAUCseq22$Stud_Intra,IntraInterlnpAUCseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnpAUC(expected value) vs studentized residuals
x <- IntraInterlnpAUC00[,3]
y <- IntraInterlnpAUC00[,7]

plot(x, y,asp=NA,col="lightgray", xlab=c2, ylab="studentized residuals",
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnpAUCseq11$Exp, IntraInterlnpAUCseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnpAUCseq11$Exp,IntraInterlnpAUCseq11$Stud_Inter,IntraInterlnpAUCseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnpAUCseq22$Exp, IntraInterlnpAUCseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnpAUCseq22$Exp,IntraInterlnpAUCseq22$Stud_Inter,IntraInterlnpAUCseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)
}
if(multiple){
}
else{               
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
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Intra,
                   IntraInterlnAUC0INFseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Intra,
                   IntraInterlnAUC0INFseq22$subj, col="black",text.pos=TRUE)

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
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Inter,
                   IntraInterlnAUC0INFseq11$subj, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Inter,
                   IntraInterlnAUC0INFseq22$subj, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)
}
##plot boxplot
par(mai=c(1,0.8,1,0.5)) 
if(multiple){
op <- par(mfrow=c(1,2))
d <- "lnCmax_ss (intrasubj)"
e <- "lnCmax_ss (intersubj)"
d1<- "lnAUC(tau)ss (intrasubj)"
e1<- "lnAUC(tau)ss (intersubj)"
}
else{
op <- par(mfrow=c(1,3))
d  <- "lnCmax (intrasubj)"
e  <- "lnCmax (intersubj)"
d1 <- "lnAUC0t (intrasubj)"
e1<-  "lnAUC0t (intersubj)"
}
if(pAUC){
f1 <- "lnpAUC (intrasubj)"
g1<-  "lnpAUC (intersubj)"
}

lnCmaxintra<-boxplot(IntraInterlnCmax00$Stud_Intra,col="lightgray", main=d, ylab="studentized residuals",las=1)
  if (length(lnCmaxintra$out) > 0){ 
  z<-NULL 
   for(i in seq_along(lnCmaxintra$out)){
    z[i]<-IntraInterlnCmax00$subj[IntraInterlnCmax00$Stud_Intra==lnCmaxintra$out[i]] 
     } 
   text(lnCmaxintra$group, lnCmaxintra$out, IntraInterlnCmax00$subj[z],pos = 2 ) 
   } 

lnAUC0tintra<-boxplot(IntraInterlnAUC0t00$Stud_Intra,col="lightgray", main=d1, ylab="studentized residuals",las=1)
if (length(lnAUC0tintra$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0tintra$out)){
    z[i]<-IntraInterlnAUC0t00$subj[IntraInterlnAUC0t00$Stud_Intra==lnAUC0tintra$out[i]] 
       } 
   text(lnAUC0tintra$group, lnAUC0tintra$out,IntraInterlnAUC0t00$subj[z],pos = 2 ) 
   } 

if(pAUC){
lnpAUCintra<-boxplot(IntraInterlnpAUC00$Stud_Intra,col="lightgray", main=f1, ylab="studentized residuals",las=1)
if (length(lnpAUCintra$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnpAUCintra$out)){
    z[i]<-IntraInterlnpAUC00$subj[IntraInterlnpAUC00$Stud_Intra==lnpAUCintra$out[i]] 
       } 
   text(lnpAUCintra$group, lnpAUCintra$out,IntraInterlnpAUC00$subj[z],pos = 2 ) 
}
}
if(multiple){
}
else{
lnAUC0INFintra<-boxplot(IntraInterlnAUC0INF00$Stud_Intra,col="lightgray", main="lnAUC0INF (intrasubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0INFintra$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0INFintra$out)){
    z[i]<-IntraInterlnAUC0INF00$subj[IntraInterlnAUC0INF00$Stud_Intra==lnAUC0INFintra$out[i]] 
       } 
   text(lnAUC0INFintra$group, lnAUC0INFintra$out,IntraInterlnAUC0INF00$subj[z],pos = 2 ) 
   } 
}   
lnCmaxinter<-boxplot(IntraInterlnCmax00$Stud_Inter,col="lightgray", main=e, ylab="studentized residuals",las=1)
if (length(lnCmaxinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnCmaxinter$out)){
    z[i]<-IntraInterlnCmax00$subj[IntraInterlnCmax00$Stud_Inter==lnCmaxinter$out[i]] 
       } 
   text(lnCmaxinter$group, lnCmaxinter$out,IntraInterlnCmax00$subj[z],pos = 2 ) 
   } 
   
lnAUC0tinter<-boxplot(IntraInterlnAUC0t00$Stud_Inter,col="lightgray", main=e1, ylab="studentized residuals",las=1)
if (length(lnAUC0tinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0tinter$out)){
    z[i]<-IntraInterlnAUC0t00$subj[IntraInterlnAUC0t00$Stud_Inter==lnAUC0tinter$out[i]] 
       } 
   text(lnAUC0tinter$group, lnAUC0tinter$out,IntraInterlnAUC0t00$subj[z],pos = 2 ) 
   } 

if(pAUC){
lnpAUCinter<-boxplot(IntraInterlnpAUC00$Stud_Inter,col="lightgray", main=g1, ylab="studentized residuals",las=1)
if (length(lnpAUCinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnpAUCinter$out)){
    z[i]<-IntraInterlnpAUC00$subj[IntraInterlnpAUC00$Stud_Inter==lnpAUCinter$out[i]] 
       } 
   text(lnpAUCinter$group, lnpAUCinter$out,IntraInterlnpAUC00$subj[z],pos = 2 ) 
}
}
if(multiple){
}
else{   
lnAUC0INFinter<-boxplot(IntraInterlnAUC0INF00$Stud_Inter,col="lightgray", main="lnAUC0INF (intersubj)", ylab="studentized residuals",las=1)
if (length(lnAUC0INFinter$out) > 0){  
  z<-NULL 
   for(i in seq_along(lnAUC0INFinter$out)){
    z[i]<-IntraInterlnAUC0INF00$subj[IntraInterlnAUC0INF00$Stud_Inter==lnAUC0INFinter$out[i]] 
       } 
   text(lnAUC0INFinter$group, lnAUC0INFinter$out,IntraInterlnAUC0INF00$subj[z],pos = 2 ) 
   } 
  }
##cook's distance
 lnCmax<- lm(lnCmax ~ seq + subj:seq + prd + drug , data=TotalData)
 lnAUC0t<- lm(lnAUC0t ~ seq + subj:seq + prd + drug , data=TotalData)
 if(pAUC) lnpAUC<- lm(lnpAUC ~ seq + subj:seq + prd + drug , data=TotalData)

if(multiple){ 
 ##cook's distance 
  op <- par(mfrow=c(1,1))
  if(pAUC){cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax_ss=cooks.distance(lnCmax),
     lnAUCtau_ss=cooks.distance(lnAUC0t),lnpAUC=cooks.distance(lnpAUC))}
  else{cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax_ss=cooks.distance(lnCmax),
     lnAUCtau_ss=cooks.distance(lnAUC0t))}
  cooks<-split(cook, cook$drug)[[1]]
  cooks<-cooks[ do.call(order, cooks) ,]
  ticks <-c(levels(cooks$subj))
  n <- length(cooks$subj)
  ##lnCmax_ss
  plot(cooks$lnCmax_ss, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnCmax_ss",font.lab=2)
  axis(1, at=c(1:n), labels = ticks)    
  points(cooks$subj,cooks$lnCmax_ss) 
   ##lnAUCtau_ss
  plot(cooks$lnAUCtau_ss, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnAUC(tau)",font.lab=2)
  axis(1, at=c(1:n), labels = ticks)    
  points(cooks$subj,cooks$lnAUCtau_ss) 
  if(pAUC){
     plot(cooks$lnpAUC, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnpAUC",font.lab=2)
     axis(1, at=c(1:n), labels = ticks)    
     points(cooks$subj,cooks$lnpAUC) 
   }
 }

else{                       ### single-dose study
    op <- par(mfrow=c(1,1))
    lnAUC0INF<- lm(lnAUC0INF ~ seq + subj:seq + prd + drug , data=TotalData)

    if(pAUC){cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax=cooks.distance(lnCmax),
        lnAUC0t=cooks.distance(lnAUC0t),lnAUC0INF= cooks.distance(lnAUC0INF),lnpAUC= cooks.distance(lnpAUC))}
    else{cook<-data.frame(subj=TotalData$subj,drug=TotalData$drug,lnCmax=cooks.distance(lnCmax),
        lnAUC0t=cooks.distance(lnAUC0t),lnAUC0INF= cooks.distance(lnAUC0INF))}
        
    cooks<-split(cook, cook$drug)[[1]]
    cooks<-cooks[ do.call(order, cooks) ,]
    ticks <-c(levels(cooks$subj))
    n <- length(cooks$subj) 
    ##lnCmax
    plot(cooks$lnCmax, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnCmax",font.lab=2)
    axis(1, at=c(1:n), labels = ticks)    
    points(cooks$subj,cooks$lnCmax) 
    ##lnAUC0t
    plot(cooks$lnAUC0t, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnAUC0t",font.lab=2)
    axis(1, at=c(1:n), labels = ticks)    
    points(cooks$subj,cooks$lnAUC0t) 
    if(pAUC){
       plot(cooks$lnpAUC, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnpAUC",font.lab=2)
       axis(1, at=c(1:n), labels = ticks)    
       points(cooks$subj,cooks$lnpAUC) 
    }
     ##lnAUC0INF
    plot(cooks$lnAUC0INF, type="h", ,xaxt="n",las=1, xlab="Deleted Level of Subject", ylab="Cook's distance",main="lnAUC0INF",font.lab=2)
    axis(1, at=c(1:n), labels = ticks)    
    points(cooks$subj,cooks$lnAUC0INF) 
   }
}