#menu for single dose
Singlego<-function(multiple=FALSE)
{

designtrace<-designtrace

cat("*** You have selected the following ->\n",designtrace,"\n")
cat("\n")
  file.menu <- c("Sample size estimation for ABE ",
                 "Noncompartmental analysis (NCA)",
                 "Statistical analysis",
                 "NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
   cat("\n")
if(multiple){
       pick <- menu(file.menu, title = " << Multiple dose menu>> ", graphics=TRUE)
   }
  else{
      pick <- menu(file.menu, title = " << Single dose menu >> ", graphics=TRUE)
   }
    if (pick == 1){
      cat("\n")
        designtrace<<-paste(designtrace,"sample size estimation,",sep=" ")
        sizemenu()
        }
    else {
    if (pick == 2){
        if(multiple){
        designtrace<<-paste(designtrace,"NCA only,",sep=" ")
        Multiple1menu()     ### this call call Multiplemenu() with 'multiple = TRUE" 
        }
        else{
        designtrace<<-paste(designtrace,"NCA only,",sep=" ")
        Multiplemenu()}
        }
    else {
    if (pick == 3){
        if(multiple){
        designtrace<<-paste(designtrace,"stat analysis only,",sep=" ")
        stat1menu()     ### this call call statmenu() with 'multiple = TRUE"
        }
        else{
        designtrace<<-paste(designtrace,"stat analysis only,",sep=" ")
        statmenu()}
        }
   else {
    if (pick == 4){
        if(multiple){
        designtrace<<-paste(designtrace,"NCA - stat analysis,",sep=" ")
        Multiplestat1menu()     ### this call call Multiplestat1menu() with 'multiple = TRUE"
        }
        else{
        designtrace<<-paste(designtrace,"NCA - stat analysis,",sep=" ")
        Multiplestatmenu()}
        }
 else {
    if (pick == 5){
        graphics.off()
        go2menu()
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