# List of Noncompartment Analysis (NCA)"
NCAmenu<-function()
{
cat("\n")
  file.menu <- c("Run NCA",
                 "Run NCA demo (lambda_z est. from the exact 3 data points)",
                 "Run NCA demo (lambda_z est. with adjusted R sq. (ARS))",
                 "Run NCA demo (lambda_z est. with Two-Times-Tmax method (TTT))",
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
       demoNCA() 
       }
    else {
    if (pick == 3){
        cat("\n")
       demoARS() 
       }
    else {
    if (pick == 4){
        cat("\n")
       demoTTT() 
       }      
    else {
    if (pick == 5){
        cat("\n")
       go() 
         }
   else {
    if (pick == 6){
        cat("\nThank you for using bear!  Bye now. \n")}
           }
        }
      }
    }
   }
 }  