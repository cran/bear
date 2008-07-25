# Sample size estimation for raw data
rawdata<-function()
{
cat("\n")
cat("****************************************************************************\n")
cat("*                            Required data                                 *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* 1. CV(%) stands for the coefficient of variation                         *\n")
cat("* 2. Theta(%) is the difference in average BA between the two formulations *\n")
cat("*       expressed in percentage of the average reference BA.               *\n")
cat("*       Theta=((Ut-Ur)/Ur)*100, where Ut and Ur are the average BA of the  *\n")
cat("*       test and reference products.                                       *\n")
cat("* 3. n  is the initial estimate of sample size per sequence.               *\n")
cat("****************************************************************************\n")
cat("\n")
cat("Enter Theta (%)(Theta should be greater than zero.)\n")
Theta <- scan(nlines=1,quiet=TRUE)
  if(Theta < 0) {
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("                               <<Error!>>                                 \n")
  cat("                                                                          \n")
  cat("                    Theta should be greater than zero.                    \n")
  cat("--------------------------------------------------------------------------\n")
    rawdata()
    }
    
cat("\n")
cat("Enter CV (%)(e.g. 20)\n")
CV <- scan(nlines=1,quiet=TRUE)

cat("\n")
cat("Enter power (%)(e.g. 80 or 90)\n")
epower <- scan(nlines=1,quiet=TRUE)

cat("\n")
cat("Enter n (e.g. 10)\n")
n <- scan(nlines=1,quiet=TRUE)
cat("\n")

T_alpha<-qt(0.95,((2*n)-2))

  if (Theta == 0){
  T_beta<-qt(1-((1-(epower*0.01))/2),((2*n)-2))
  k<-((T_alpha+ T_beta)^2)*((CV/Theta)^2)
  i<-round(k,2)
 
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("                               <<Estimation>>                             \n")
  cat("--------------------------------------------------------------------------\n")
  cat("            Sample size n>=",i," (per sequence)                           \n")
  cat("            Total sample size",2*i," (You should consider                 \n")
  cat("            rounding up to the next even number.)                         \n")
  cat("--------------------------------------------------------------------------\n")
  cat("\n")
   go()
  }
   else{
    if (Theta > 0){
   T_beta<-qt((epower*0.01),((2*n)-2))
   k<-((T_alpha+ T_beta)^2)*((CV/((100-epower)-Theta))^2)
   i<-round(k,2)
   
   cat("\n")
   cat("--------------------------------------------------------------------------\n")
   cat("                               <<Estimation>>                             \n")
   cat("--------------------------------------------------------------------------\n")
   cat("            Sample size n>=",i," (per sequence)                           \n")
   cat("            Total sample size",2*i," (You should consider                 \n")
   cat("            rounding up to the next even number.)                         \n")
   cat("--------------------------------------------------------------------------\n")
   cat("\n")
     go()
   }
 } 
}   