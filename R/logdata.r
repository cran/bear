# Sample size estimation for log transformation data
logdata <-function(Demo=FALSE)
{
cat("\n")

description_size()

if(Demo){
cat("\n")
cat("Enter Theta (%) (e.g. 95):\n")
Theta <- 95 
ratio<-Theta/100 
cat(" 95\n")

cat("\n") 
cat("Enter intra-subj (crossover) or inter-subj (paralell) CV (%)\n")
cat("(or press Enter to use default value: 20 )?\n")
CV1 <- 20 
CV<-CV1/100
cat(" 20\n")
 
cat("\n")
cat("Enter power (%) (e.g. 80 or 90):\n")
epower <- 80 
target<-epower/100
cat(" 80\n")

cat("\n")
cat("Enter lower acceptance limit (%)(e.g. 80):\n")
LL<-80 
theta1 <- LL/100       # theta1: lower acceptance limit
cat(" 80\n")
theta2 <- 1/theta1
}

else{
cat("Enter Theta (%)(or press Enter to use default value: 95 )\n") 
#Theta <- scan(nlines=1,quiet=TRUE)
Theta <-readline()
if (substr(Theta, 1, 1) == "" || Theta<=0)  Theta<-95  else Theta<-as.numeric(Theta) 
ratio<-Theta/100 

cat("\n") 
cat("Enter intra-subj (crossover) or inter-subj (paralell) CV (%)\n")
cat("(or press Enter to use default value: 20 )?\n")
CV1 <- readline()
if (substr(CV1, 1, 1) == "" || CV1<=0)  CV1<-20  else CV1<-as.numeric(CV1) 
CV<-CV1/100
 
cat("\n")
cat("Enter power (%)(or press Enter to use default value: 80 )\n") 
epower <- readline()
if (substr(epower, 1, 1) == "" || epower<=0)  epower<-80  else epower<-as.numeric(epower)
target<-epower/100

cat("\n")
cat("Enter lower acceptance limit (%)(or press Enter to use default value: 80 )\n")
Lm<-readline()
if (substr(Lm, 1, 1) == ""|| Lm<=0)  Lm<-80  else Lm<-as.numeric(Lm)
theta1 <- Lm/100      # theta1: lower acceptance limit
theta2 <- 1/theta1
}
cat("\n")
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
            "                           <<Sample Size Estimation>>                     \n",
            "                                                                          \n",   
            "  Upper acceptance limit =",theta2*100,"%\n", 
            "  Lower acceptance limit =",theta1*100,"%\n",
            "      Expected ratio T/R =", 
               format(round(ratio*100,2),nsmall=2,width=3),"%\n",
            "            Target power =",
               format(round(target*100,2),nsmall=2,width=3),"%\n",
            " Inter- or Intra-subj CV =",format(i*100,nsmall=1,width=3),"% \n\n "),   
      paste("study","    2x2x1","      2x2x2","       2x2x3","       2x2x4    ","\n"),
      paste(" design","  parallel","  crossover","   replicated","  replicated","\n"), 
      paste("-------","-----------","------------","------------","------------","\n"))  
    cat(title)
   } 
  
   cat(paste("    N")) #1 

 
  #for parallel study 
  sigmaB    <- sqrt(log(1+i^2))
  s         <- sqrt(2)*sigmaB
  for (j in target)      # power loop
    {
    n       <- 6         # start value of sample size search
    repeat{
      df    <- (2*n)-2
      t1    <- qt(1-a,df)
      t2    <- -t1
      nc1   <- (sqrt(n))*((log(ratio)-log(theta1))/s)
      nc2   <- (sqrt(n))*((log(ratio)-log(theta2))/s)
      prob1 <- pt(t1,df,nc1)
      prob2 <- pt(t2,df,nc2)
      power <- prob2-prob1
      ppct  <- power*100
      if(power >= j | n > limit) break
      n     <- n+1
      }
    if(n <= limit){
      cat(paste("     ",format(2*n,width=5)))#2
        pow<-format(round(ppct,5),nsmall=3,width=8)
        #sep=""))
      } else
      cat(paste(" >",format(limit,width=4)))
        pow<-format(round(ppct,5),nsmall=3,width=8)
     # cat("\n")
     }

#for crossover and replicated study 
  #cat(paste(format(i*100,nsmall=1,width=4),"% ",sep=""))
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
      cat(paste("      ",format(n,width=5)))#2 
        pow1<-format(round(ppct,5),nsmall=3,width=8) 
        #sep=""))
        n1<-(ceiling(n*3/4))
        y1=n1[!(n1 %% 2 == 0)]
         if(identical(y1, numeric(0))){
           z1<-n1   #3
           }else{
            z1<-n1+1  #3
            }
        cat(paste("         ",z1))
        n2<-(ceiling(n/2))
        y2=n2[!(n2 %% 2 == 0)]
         if(identical(y2, numeric(0))){
          z2<-n2    #4 
           }else{
          z2<-n2+1    #4 
            }   
        cat(paste("          ",z2))
       
      } else
      cat(paste(" >",format(limit,width=4)))
        pow1<-format(round(ppct,5),nsmall=3,width=8) 
   
    }
   cat("\n")
   cat("\n")
   cat(" Estimated power =",pow,"% (for parallel study)\n")  
   cat(" Estimated power =",pow1,"% (for crossover/replicate study)\n") 
   cat(" where 2x2x2 means 2-treatment, 2-sequence, and 2-period crossover study.\n")
   cat("-------------------------------------------------------------------------\n")  
cat("\n")
cat("**Ref.:\n")
cat(" 1. Hauschke D, Steinijans VW, Diletti E and Burke M.  Sample size         \n")
cat("    determination for bioequivalence assessment using a multiplicative model.\n")  
cat("    Journal of Pharmacokinetics and Biopharmaceutics. 20, 557-561 (1992).  \n")
cat(" 2. Julious SA. Tutorial in biostatistics: Sample sizes for clinical trials \n")
cat("    with normal data.  Statistics in Medicine. 23, 1921-1986 (2004).       \n")
cat(" 3. Hauschke D, Steinijans VW and Pigeot I.  Bioequivalence studies in drug\n") 
cat("    development methods and applications. John Wiley & Sons, New York      \n")
cat("    (2007).                                                                \n")
cat("\n")
cat(" Note: The algorithms we use here have been validated with Dr. David Dubins' \n")
cat(" FARTSSIE, which is a very excellant Excel VBA program for many sample size   \n")
cat(" estimation functions.  Website: http://individual.utoronto.ca/ddubins.    \n")
cat("\n") 
 cat("--------------------------------------------------------------------------\n")    
   }
  sizemenu()
}

