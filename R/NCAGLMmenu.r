# List of Generalized Linear Models (GLM)
NCAGLMmenu<-function()
{
cat("\n")
  file.menu <- c("NCA-->GLM",
                 "Demo for NCA-->GLM",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA-->GLM menu >> ")
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