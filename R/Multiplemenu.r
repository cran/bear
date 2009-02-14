# menu for NCA non-replicated and replicated study
Multiplemenu<-function()
{
cat("\n")
  file.menu <- c("NCA for 2x2x2 crossover study",
                 "NCA for replicated study",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ")
    if (pick == 1){
      cat("\n")
        NCAmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
       Repmenu()
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