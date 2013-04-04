#menu for single dose
Singlego<-function(multiple=FALSE)
{
cat("\n")
  file.menu <- c("Sample size estimation for ABE ",
                 "Noncompartmental analysis (NCA)",
                 "Statistical analysis",
                 "NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
   cat("\n")
if(multiple){
       pick <- menu(file.menu, title = " << Multiple dose menu>> ")
   }
  else{
  pick <- menu(file.menu, title = " << Single dose menu >> ")
   }
    if (pick == 1){
      cat("\n")
        sizemenu()
        }
    else {
    if (pick == 2){
        if(multiple){
        Multiple1menu()
        }
        else{
        Multiplemenu()}
        }
    else {
    if (pick == 3){
        if(multiple){
        stat1menu()
        }
        else{
        statmenu()}
        }
   else {
    if (pick == 4){
        if(multiple){
        Multiplestat1menu()
        }
        else{
        Multiplestatmenu()}
        }
 else {
    if (pick == 5){
        graphics.off()
        go()
         }
  else {
    if (pick == 6){
        cat("\n  Thank you for using bear!  Bye now. \n\n")
        graphics.off()}
         }     
       }
      }
   }
 }
}