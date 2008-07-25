# List of Noncompartment Analysis (NCA)"
NCAmenu<-function()
{
cat("\n")
  file.menu <- c("Single Dose",
                 "Run demo for Single Dose",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental Analysis (NCA) menu >> ")
    if (pick == 1){
      cat("\n")
        NCAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demoNCA() 
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