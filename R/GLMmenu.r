# List of Generalized Linear Models (GLM)
GLMmenu<-function(TotalData)
{
cat("\n")
  file.menu <- c("Generalized Linear Models (GLM)",
                 "Demo for GLM",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Generalized Linear Models (GLM) menu >> ")
    if (pick == 1){
      cat("\n")
        GLManalyze(TotalData)
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