# List of Generalized Linear Models (GLM)
BANOVAmenu<-function(TotalData, multiple=FALSE)
{
cat("\n")
  file.menu <- c("Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Run demo: Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Statistical analysis (ANOVA(lm), 90%CI...) menu >> ")
    if (pick == 1){
      cat("\n")
        if(multiple){
         MultipleBANOVAdata(TotalData)
         }
        else{
         BANOVAdata(TotalData)
          }
        }
    else {
    if (pick == 2){
        cat("\n")
       if(multiple){
       MultipledemoBANOVA()
         }
        else{
       demoBANOVA()
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
        cat("\n  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
             }
           }
       }
     }
}