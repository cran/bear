# List of Generalized Linear Models (GLM)
NCAGLMmenu<-function()
{
cat("\n")
  file.menu <- c("NCA--> ANOVA (lm)",
                 "Run demo for NCA--> ANOVA (lm)",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA--> ANOVA (lm) menu >> ")
    if (pick == 1){
      cat("\n")
        NCAGLMdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demoNCAGLM()
       }
    else {
    if (pick == 3){
        cat("\n")
       go()
         }
   else {
    if (pick == 4){
        cat("\nBye~~ \n\n")}
           }
       }
     }
}