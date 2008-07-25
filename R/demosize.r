#demo for sample estimation

demosize<-function()
{
cat("\n")
cat("****************************************************************************\n")
cat("*                            Required information                          *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* CV(%) stands for the coefficient of variation                            *\n")
cat("* Theta(%) is the difference in average BA between the two formulations    *\n")
cat("*       expressed in percentage of the average reference BA.               *\n")
cat("*       Theta=((Ut-Ur)/Ur)*100, where Ut and Ur are the average BA of the  *\n")
cat("*       test and reference formulation.                                    *\n")
cat("* n     is the initial guess of sample size number for per sequence        *\n")
cat("****************************************************************************\n")
cat("\n")
cat("Enter CV (%)(e.g. 20)\n")
cat(" 20\n")
CV <- 20
cat("\n")
cat("Enter Theta (%)(e.g. 10)\n")
cat(" 10\n")
Theta <- 10
cat("\n")
cat("Enter power (%)(e.g. 80 or 90)\n")
cat(" 80\n")
epower <- 80
cat("\n")
cat("Enter n (e.g. 10)\n")
cat(" 10\n")
n <- 10
cat("\n")

T_alpha<-qt(0.95,((2*n)-2))
T_alpha<-qt(0.8,((2*n)-2))

  if (Theta == 0){
  T_beta<-qt(1-((1-(epower*0.01))/2),((2*n)-2))
  n<-((T_alpha+ T_beta)^2)*((CV/Theta)^2)
  i<-round(n,4)
 
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("                               <<Suggestion>>                             \n")
  cat("--------------------------------------------------------------------------\n")
  cat("            n>=",i," (sample size number for per sequence)               \n")
  cat("            Total sample size",2*i," (you should consider                 \n")
  cat("            rounding up to the next even number)                          \n")
  cat("--------------------------------------------------------------------------\n")
  cat("\n")
   sizemenu()
  }
   else{
   T_beta<-qt((epower*0.01),((2*n)-2))
   n<-((T_alpha+ T_beta)^2)*((CV/((100-epower)-Theta))^2)
   i<-round(n,4)
 
   cat("\n")
   cat("--------------------------------------------------------------------------\n")
   cat("                               <<Suggestion>>                             \n")
   cat("--------------------------------------------------------------------------\n")
   cat("           n>=",i," (sample size number for per sequence)               \n")
   cat("           Total sample size",2*i," (you should consider                 \n")
  cat("            rounding up to the next even number)                          \n")
   cat("--------------------------------------------------------------------------\n")
   cat("\n")
     sizemenu()
   }
}