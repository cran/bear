# List of Noncompartment Analysis (NCA) for 2 seq x more than 2 periods crossover
Repmenu<-function()
{
cat("\n")
  file.menu <- c("Run NCA",
                 "Run NCA demo",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA for Replicated Crossover Study>> ")
    if (pick == 1){
      cat("\n")
       RepNCAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       Repdemomenu()
       }
    else {
    if (pick == 3){
        cat("\n")
       Multiplemenu()
       }
    else {
    if (pick == 4){
        cat("\n")
      cat("\nThank you for using bear!  Bye now. \n")}
       }
      }
    }
 }