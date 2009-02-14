# List of Generalized Linear Models (GLM)
NCA.BANOVAmenu<-function()
{
cat("\n")
  file.menu <- c("NCA --> Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA--> Statistical analysis for 2x2x2 Crossover Study>> ")
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
       go()
         }
   else {
    if (pick == 4){
        cat("\nThank you for using bear!  Bye now. \n\n")
              }
           }
       }
    }
}
  