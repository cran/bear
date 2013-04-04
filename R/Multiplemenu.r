# menu for NCA non-replicated and replicated study <-  Wow!  how can I guess this?  totally different.
Multiplemenu<-function(multiple=FALSE)
{
cat("\n")
 #mutiple dose menu
  if(multiple){
  file.menu <- c("NCA for 2x2x2 crossover study",
                 "NCA for parallel study", 
                 "Back to the previous step",
                 "Quit")
cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ")
    if (pick == 1){
      cat("\n")
        MultipleNCAmenu()
        }
    else {
    if (pick == 2){
       MultipleParamenu() 
       } 
    else {
    if (pick == 3){
        cat("\n")
      Multiplego() 
      }    
    else {
    if (pick == 4){
        cat("\n")
        cat("\n  Thank you for using bear!  Bye now. \n")}
        graphics.off()
          }
         } 
       }
      }
  #single dose menu
 else{
  file.menu <- c("NCA for 2x2x2 crossover study",
                 "NCA for replicated study",
                 "NCA for parallel study", 
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
       Paramenu()
      }    
    else {
    if (pick == 4){
        cat("\n")
       Singlego()
      } 
    else {
    if (pick == 5){
        cat("\n")
      cat("\n   Thank you for using bear!  Bye now. \n")
      graphics.off()
           }
         } 
       }
      }
    }
  }
}  