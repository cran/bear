# Replicated study lme menu
RepMIXmenu<-function(TotalData, parallel=FALSE, multiple=FALSE)
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
        if(parallel){
          if(multiple){
         MultipleParaMIXdata(TotalData)
          }
          else{
         ParaMIXdata(TotalData)
          }
        }
        else{ 
         RepMIXdata(TotalData)
          } 
        }
    else {
    if (pick == 2){
        cat("\n")
        if(parallel){
           if(multiple){
        MultipleParademoMIX()
          }
          else{
         ParademoMIX()
          }
        }
        else{ 
        RepdemoMIX()
        } 
       }
    else {
    if (pick == 3){
        cat("\n")
       if(multiple){
       stat1menu()
       }
       else{
       statmenu()
         }
       }
   else {
    if (pick == 4){
        cat("\nThank you for using bear!  Bye now. \n\n")
             }
           }
       }
     }
}