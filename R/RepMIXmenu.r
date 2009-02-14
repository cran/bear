# Replicated study lme menu
RepMIXmenu<-function(TotalData)
{
cat("\n")
  file.menu <- c("Statistical analysis (lme, 90%CI...)",
                 "Run demo: Statistical analysis (lme, 90%CI...)",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Statistical analysis (lme, 90%CI...) menu >> ")
    if (pick == 1){
      cat("\n")
        RepMIXdata(TotalData)
        }
    else {
    if (pick == 2){
        cat("\n")
        RepdemoMIX()
       }
    else {
    if (pick == 3){
        cat("\n")
       statmenu()
         }
   else {
    if (pick == 4){
        cat("\nThank you for using bear!  Bye now. \n\n")
             }
           }
       }
     }
}