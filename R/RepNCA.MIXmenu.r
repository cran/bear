# List
RepNCA.MIXmenu<-function(parallel=FALSE, multiple=FALSE)
{
cat("\n")
  if(parallel){
  file.menu <- c("NCA --> Statistical analysis (lm, 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")}
 else{
 file.menu <- c("NCA --> Statistical analysis (lme, 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")}
  if(parallel){
  pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Parallel Study >> ")
   }
  else{
  pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Replicated Crossover Study >> ")
  }  
    if (pick == 1){
      cat("\n")
          if(parallel){
            if(multiple){
            MultipleParaNCA.MIXdata()  
            }
            else{
            ParaNCA.MIXdata()  
            }
          }
          else{
        RepNCA.MIXdata()
            }
       } 
    else {
    if (pick == 2){
        cat("\n")
         if(parallel){
           if(multiple){
            MultipleParademomenu1()
            }
            else{       
            Parademomenu1()   
            }
          }
          else{
       Repdemomenu1()
          }
      } 
    else {
    if (pick == 3){
        cat("\n")
        if(multiple){
         Multiplestat1menu()
        }
         else{
         Multiplestatmenu()
          }
         }
   else {
    if (pick == 4){
        cat("\n   Thank you for using bear!  Bye now. \n\n")
        graphics.off()
              }
           }
       }
    }
}
