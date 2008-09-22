# Sample size estimation for log transformation data
logdata <-function()
{
cat("\n")
cat("****************************************************************************\n")
cat("*                            Required data                                 *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* 1. CV stands for the coefficient of variation.                           *\n")
cat("* 2. Theta is the difference in average BA between the two formulations    *\n")
cat("*    expressed in percentage of the average reference BA.                  *\n")
cat("* 3. Theta=Ut/Ur, where Ut and Ur denote the median BA for the Test and    *\n")
cat("*    the Reference products.                                               *\n")
cat("****************************************************************************\n")
cat("\n")
cat("Enter Theta (%)(or press Enter to use default value : 95 )\n") 
#Theta <- scan(nlines=1,quiet=TRUE)
Theta <-readline()
if (substr(Theta, 1, 1) == "" || Theta<=0)  Theta<-95  else Theta<-as.numeric(Theta) 
ratio<-Theta/100 

cat("\n") 
cat("Enter CV (%)(or press Enter to use default value : 20 )\n")
CV1 <- readline()
if (substr(CV1, 1, 1) == "" || CV1<=0)  CV1<-20  else CV1<-as.numeric(CV1) 
CV<-CV1/100
 
cat("\n")
cat("Enter power (%)(or press Enter to use default value : 80 )\n") 
epower <- readline()
if (substr(epower, 1, 1) == "" || epower<=0)  epower<-80  else epower<-as.numeric(epower)
target<-epower/100

cat("\n")
cat("Enter upper acceptance limit (%)(or press Enter to use default value : 125 )\n")
Um<-readline()
if (substr(Um, 1, 1) == ""|| Um<=0)  Um<-125  else Um<-as.numeric(Um)
theta2 <- Um/100      # theta2: upper acceptance limit


cat("\n")
cat("Enter lower acceptance limit (%)(or press Enter to use default value : 80 )\n")
Lm<-readline()
if (substr(Lm, 1, 1) == ""|| Lm<=0)  Lm<-80  else Lm<-as.numeric(Lm)
theta1 <- Lm/100      # theta1: lower acceptance limit

cat("\n")
cat("Upper acceptance limit",theta2*100, "(%)\n")
cat("Lower acceptance limit",theta1*100, "(%)\n")
#######################################################################
# Sample size calculation for a standard RT/TR                        #
# 2x2x2 cross-over design (multiplicative model)                      #
#######################################################################
# R-code based on SAS-code by
# (1) B. Jones and M.G. Kenward
#     Design and Analysis of Cross-Over Trials
#     Chapman & Hall/CRC, Boca Raton (2nd Edition 2000)
# (2) S. Patterson and B. Jones
#     Bioequivalence and Statistics in Clinical Pharmacology
#     Chapman & Hall/CRC, Boca Raton (2006)
# /*** WARNING : PROGRAM OFFERED FOR USE WITHOUT ANY GUARANTEES    ***/
# /*** NO LIABILITY IS ACCEPTED FOR ANY LOSS RESULTING FROM USE OF ***/
# /*** THIS SET OF SAS INTRUCTIONS                                 ***/
# Modification of degrees of freedom according to a
# personal message by D. Hauschke (E-mail 2006-01-05)
# Tested in R-versions 2.6.2 / 2.5.1 / 1.9.1 / 1.9.0
# 2008-04-04
# Helmut Schuetz
# BEBAC - Consultancy services for Bioequivalence
# and Bioavailability Studies
# 1070 Vienna, Austria
# ----------------------------------------------
a           <- 0.05      # alpha
#theta1      <- 0.8       # theta1: lower acceptance limit
#theta2      <- 1/theta1  # theta2: upper acceptance limit
 # ratio: expected ratio T/R
 # target: target power
 # CV: intra-subject coefficients of variation
limit       <- 1000000       # upper sample size limit for search
# ----------------------------------------------
# Do not modify the code below this line unless
# you know what you are doing!
# ----------------------------------------------
for (i in CV)            # CV loop
  {
  if(i==CV[1]){
    title=paste(
      paste("--------------------------------------------------------------------------\n",
            "                               <<Estimation>>                             \n",
            "                                                                          \n",
            " Sample size estimation for a standard RT/TR 2x2x2 cross-over design      \n", 
            "(multi-plicative model).\n",
            " Expected ratio T/R =",
               format(round(ratio*100,2),nsmall=2,width=4),"%.\n\n"),
      paste(paste(format(round(target*100,2),nsmall=2,width=17),
        "%",sep="",collapse=""),"\n"),
      paste(" CV",
        paste(c(rep("    sample.size (power.)",
        length(target))),collapse=""),"\n"),
      paste("------",
        paste(c(rep(paste(rep("-",25),collapse=""),
        length(target))),collapse=""),"\n") )
    cat(title)
    }
  cat(paste(format(i*100,nsmall=1,width=4),"% ",sep=""))
  sigmaW    <- sqrt(log(1+i^2))
  s         <- sqrt(2)*sigmaW
  for (j in target)      # power loop
    {
    n       <- 6         # start value of sample size search
    repeat{
      df    <- n-2
      t1    <- qt(1-a,df)
      t2    <- -t1
      nc1   <- (sqrt(n))*((log(ratio)-log(theta1))/s)
      nc2   <- (sqrt(n))*((log(ratio)-log(theta2))/s)
      prob1 <- pt(t1,df,nc1)
      prob2 <- pt(t2,df,nc2)
      power <- prob2-prob1
      ppct  <- power*100
      if(power >= j | n > limit) break
      n     <- n+2       # increment for even sample size
      }
    if(n <= limit){
      cat(paste(format(n,width=5)," (",
        format(round(ppct,5),nsmall=3,width=8),"%) ",
        sep=""))
        cat("\n")
      } else
      cat(paste(">",format(limit,width=4)," (",
        format(round(ppct,5),nsmall=3,width=8),"%) ",
        sep=""))
      cat("\n")
      cat("--------------------------------------------------------------------------\n")   
     }
   cat("\n")
   }
  sizemenu()
}

