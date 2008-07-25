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
cat("* 4. Please note: Theta should be greater than 0.8 and less than 1.25.     *\n")
cat("* 5. n is the initial estimate of sample size per sequence.                *\n")
cat("****************************************************************************\n")
cat("\n")
cat("Enter Theta (%)(Theta should be greater than 0.8 and less than 1.25.)\n")
Theta <- scan(nlines=1,quiet=TRUE)
  if(Theta <= 0.8 || Theta >= 1.25) {
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("                               <<Error!!>>                                \n")
  cat("                                                                          \n")
  cat("           Theta should be greater than 0.8 and less than 1.25.           \n")
  cat("--------------------------------------------------------------------------\n")
    logdata()
    }

cat("\n") 
cat("Enter CV (%)(e.g. 0.05)\n")
CV <- scan(nlines=1,quiet=TRUE)

cat("\n")
cat("Enter power (%)(e.g. 80 or 90)\n")
epower <- scan(nlines=1,quiet=TRUE)

cat("\n")
cat("Enter n (e.g. 10)\n")
n <- scan(nlines=1,quiet=TRUE)
cat("\n")

T_alpha<-qt(0.95,((2*n)-2))

  if (Theta == 1){
  T_beta<-qt(1-((1-(epower*0.01))/2),((2*n)-2))
  k<-((T_alpha+ T_beta)^2)*((CV/log(1.25))^2)
  i<-round(k,2)
  
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("                               <<Estimation>>                             \n")
  cat("--------------------------------------------------------------------------\n")
  cat("            Sample size n>=",i," (per sequence)                           \n")
  cat("            Total sample size",2*i," (you should consider                 \n")
  cat("            rounding up to the next even number)                          \n")
  cat("--------------------------------------------------------------------------\n")
  cat("\n")
    go() 
  }
   else{
    if (1 < Theta && Theta <1.25) {
    T_beta<-qt((epower*0.01),((2*n)-2))
     k<-((T_alpha+ T_beta)^2)*((CV/(log(1.25)-log(Theta)))^2)
    i<-round(k,2)
    
    cat("\n")
    cat("--------------------------------------------------------------------------\n")
    cat("                               <<Estimation>>                             \n")
    cat("--------------------------------------------------------------------------\n")
    cat("            Sample size n>=",i," (per sequence)                           \n")
    cat("            Total sample size",2*i,"(You should consider                  \n")
    cat("            rounding up to the next even number.)                         \n")      
    cat("--------------------------------------------------------------------------\n")
    cat("\n")
     go() 
    }
   else{
    if (0.8 < Theta && Theta <1) {
     T_beta<-qt((epower*0.01),((2*n)-2))
     k<-((T_alpha+ T_beta)^2)*((CV/(log(0.8)-log(Theta)))^2)
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
}
