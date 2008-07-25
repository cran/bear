# List of Generalized Linear Models (GLM)
GLMmenu<-function(TotalData)
{
cat("\n")
  file.menu <- c("ANOVA (lm)",
                 "Run demo for ANOVA (lm)",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << ANOVA (lm) menu >> ")
    if (pick == 1){
      cat("\n")
        GLMdata(TotalData)
        }
    else {
    if (pick == 2){
        cat("\n")
       demoGLM()
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