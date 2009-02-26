# List
RepNCA.MIXmenu<-function(parallel=FALSE)
{
cat("\n")
  file.menu <- c("NCA --> Statistical analysis (lme, 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  if(parallel){
  pick <- menu(file.menu, title = " << NCA--> Statistical analysis for Parallel Study>> ")
   }
  else{
  pick <- menu(file.menu, title = " << NCA--> Statistical analysis for Replicated Crossover Study>> ")
  }  
    if (pick == 1){
      cat("\n")
          if(parallel){
        ParaNCA.MIXdata()  
          }
          else{
        RepNCA.MIXdata()
            }
       } 
    else {
    if (pick == 2){
        cat("\n")
         if(parallel){
       Parademomenu1()   
          }
          else{
       Repdemomenu1()
          }
      } 
    else {
    if (pick == 3){
        cat("\n")
       Multiplestatmenu()
         }
   else {
    if (pick == 4){
        cat("\nThank you for using bear!  Bye now. \n\n")
              }
           }
       }
    }
}
