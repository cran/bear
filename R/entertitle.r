entertitle<-function(Demo=FALSE){
 if (Demo){
      ##NCAanalyze or NCAGLManalyze
      cat("Enter drug dose:\n")
      cat("80000\n")
      Dose <- 80000
      cat("\n")

      cat("\nEnter the title of x-axis (time)for plot:\n")
      cat("(or press Enter to use default value - Time)\n\n")
      cat("Time\n")
      xaxis<-"Time"
      cat("\n")

      cat("\nEnter the title of y-axis (Conc.)for plot:\n")
      cat("(or press Enter to use default value - Conc.\n\n")
      cat("Conc.\n")
      yaxis<-"Conc."
      cat("\n")
        return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose))   
     }   
  
  else {
 cat("Enter drug dose:\n")
    Dose<- readline() 
    if (substr(Dose, 1, 1) == ""|| Dose<=0)  Dose<-0  else Dose<-as.numeric(Dose) 
       repeat{ 
        if (Dose==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press Enter to continue.                  \n")
         cat("*******************************************\n\n")
         cat("\n")
           Dose<- readline()
           if (substr(Dose, 1, 1) == ""|| Dose<=0)  Dose<-0  else Dose<-as.numeric(Dose)
         }     
    else{
       break
       return (Dose<-as.numeric(Dose)) 
    } 
   }   
    cat("\nEnter the title of x-axis (Time) for plot:\n")
    cat("(or press Enter to use default - Time)\n\n") 
    xaxis<-readline()
     if (substr(xaxis, 1, 1) == "")  xaxis<-"Time"  else xaxis<-xaxis
     
    cat("\nEnter the title of y-axis(Conc.) for plot:\n")
    cat("(or press Enter to use default - Conc.)\n\n") 
    yaxis<-readline()
    if (substr(yaxis, 1, 1) == "")  yaxis<-"Conc."  else yaxis<-yaxis
    cat("\n\n Please Wait.  Data analysis is Processing now. \n")
       return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose))
 }
} 