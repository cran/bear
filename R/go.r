# List of bear (Bioequivalence and Bioavailability) Menu
go<-function()
{
options(warn=-1)
if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }   
cat("\n")
  file.menu <- c("Sample size estimation for ABE ",
                 "Noncompartmental Analysis (NCA)",
                 "ANOVA (lm)",
                 "NCA--> ANOVA (lm)",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << menu >> ")
    if (pick == 1){
      cat("\n")
        sizemenu()}

    else {
    if (pick == 2){
        cat("\n")
        NCAmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
        GLMmenu()
         }
   else {
    if (pick == 4){
        NCAGLMmenu()
         }
    else {
    if (pick == 5){
        cat("\nBye~~ \n\n")}
         }  
      }
   }
 }
}  