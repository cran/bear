# List of Noncompartment Analysis (NCA) for 2x2 crossover 
NCAmenu<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
cat("\n")
  file.menu <- c("Run NCA",
                 "Run NCA demo",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
if(parallel){
       pick <- menu(file.menu, title = " << NCA for Parallel Study>> ")
   }
else{
   if(replicated ){
       pick <- menu(file.menu, title = " << NCA for Replicated Crossover Study>> ")
    } 
    else{ 
       pick <- menu(file.menu, title = " << NCA for 2x2x2 Crossover Study>> ")
    }
} 
    if (pick == 1){
      cat("\n")
      if(multiple){
         if(parallel){
         MultipleParaNCAdata()
         }
       else{
          MultipleNCAdata()
          }
         } 
      else{
        if(parallel){
         ParaNCAdata()
         }
         else{
          if(replicated ){
          RepNCAdata()
          } 
          else{ 
          NCAdata()
          }
         } 
        }
       } 
    else {
    if (pick == 2){
        cat("\n")
      if(multiple){
        if(parallel){
       MultipleParademomenu()
       }
       else{
         Multipledemomenu() 
          }
         } 
       else{
         if(parallel){
          Parademomenu()
          }
          else{
           if(replicated ){
            Repdemomenu()
             } 
            else{ 
            demomenu()}
          } 
         }
        } 
    else {
    if (pick == 3){
        cat("\n")
       if(multiple){
       Multiple1menu() 
       }
       else{
       Multiplemenu() 
       }
      } 
    else {
    if (pick == 4){
        cat("\n")
      cat("\nThank you for using bear!  Bye now. \n")}
       }      
      }
    }
 }
   