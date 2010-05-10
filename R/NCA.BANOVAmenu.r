# List of Generalized Linear Models (GLM)
NCA.BANOVAmenu<-function(multiple=FALSE)
{
cat("\n")
  file.menu <- c("NCA --> Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA--> Statistical analysis for 2x2x2 Crossover Study>> ")
  if(multiple){
   if (pick == 1){
      cat("\n")
        MultipleNCA.BANOVAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
        Multipledemomenu1()
       }
    else {
    if (pick == 3){
        cat("\n")
        Multiplestat1menu()
         }
   else {
    if (pick == 4){
        cat("\nThank you for using bear!  Bye now. \n\n")
              }
           }
       }
    }
  } 
  else{ 
    if (pick == 1){
      cat("\n")
        NCA.BANOVAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demomenu1()
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
} 
  