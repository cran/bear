# List of Generalized Linear Models (GLM)
NCA.BANOVAmenu<-function()
{
cat("\n")
  file.menu <- c("NCA --> Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Run demo for NCA (exact 3 data points)--> Statistical analysis",
                 "Run demo for NCA (ARS)--> Statistical analysis",
                 "Run demo for NCA (TTT)--> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA--> Statistical analysis>> ")
    if (pick == 1){
      cat("\n")
        NCA.BANOVAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demoNCA.BANOVA()
       }
    else {
    if (pick == 3){
        cat("\n")
       demoARS.BANOVA()
       }
    else {
    if (pick == 4){
        cat("\n")
       demoTTT.BANOVA()
       }      
    else {
    if (pick == 5){
        cat("\n")
       go()
         }
   else {
    if (pick == 6){
        cat("\nThank you for using bear!  Bye now. \n\n")
              }
           }
       }
     }
   }
  }
 }  