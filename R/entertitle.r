entertitle<-function(Demo=FALSE, multiple=FALSE){
 if (Demo){
      ##NCAanalyze or NCAGLManalyze
      if(multiple){
      cat("Enter drug dose:\n")
      cat("1000000\n")
      Dose <- 1000000
      cat("\n")
      
       cat("Enter Dosing Interval (Tau):\n")
       cat("24\n")
       Tau <- 24
       
       cat("Enter the Time of the Last Dose since the First Dose Was Given:\n")
       cat("120\n")
       TlastD <- 120
      }
      else{
      cat("Enter drug dose:\n")
      cat("80000\n")
      Dose <- 80000
      cat("\n")
      } 
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
   if(multiple){
    cat("Enter Tau:\n")
    Tau<-  readline() 
    if (substr(Tau, 1, 1) == ""|| Tau<=0)  Tau<-0  else Tau<-as.numeric(Tau)
       repeat{
        if (Tau==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press Enter to continue.                  \n")
         cat("*******************************************\n\n")
         cat("\n")
           Tau<- readline()
           if (substr(Tau, 1, 1) == ""|| Tau<=0)  Tau<-0  else Tau<-as.numeric(Tau)
         }
    else{
       break
       return (Tau<-as.numeric(Tau))
    }
   }

     cat("Enter Time of Last Dose:\n")
     TlastD<- readline() 
     if (substr(TlastD, 1, 1) == ""|| TlastD<=0)  TlastD<-0  else TlastD<-as.numeric(TlastD)
       repeat{
        if (TlastD==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press Enter to continue.                  \n")
         cat("*******************************************\n\n")
         cat("\n")
           TlastD<- readline()
           if (substr(TlastD, 1, 1) == ""|| TlastD<=0)  TlastD<-0  else TlastD<-as.numeric(TlastD)
         }
    else{
       break
       return (TlastD<-as.numeric(TlastD))
    }
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
 }
    if(multiple){
       return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose,Tau=Tau, TlastD=TlastD ))
       }
     else{
       return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose ))
       }
} 