# List of Noncompartment Analysis (NCA)"
NCAmenu<-function()
{
cat("\n")
  file.menu <- c("Run NCA",
                 "Run NCA demo",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ")
    if (pick == 1){
      cat("\n")
        NCAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demomenu() 
       }
    else {
    if (pick == 3){
        cat("\n")
       go() 
       }
    else {
    if (pick == 4){
        cat("\n")
      cat("\nThank you for using bear!  Bye now. \n")}
       }      
      }
    }
 }
   