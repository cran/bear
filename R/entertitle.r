entertitle<-function(Demo=FALSE, multiple=FALSE){

ODAnalysis<-ODAnalysis
lin.AUC<-lin.AUC
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
DosingTau<-DosingTau
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz
IndivDP_output<-IndivDP_output

if (Demo){
      ##NCAanalyze or NCAGLManalyze
      if(multiple){
      cat(" Input Drug Dose:\n")
      cat("1000000\n")
      Dose <- 1000000
      cat("\n")
      
       cat(" Input Dosing Interval (Tau):\n")
       cat("24\n")
       Tau <- 24
       
       cat(" Input the Time Point of the Final Dose since Time zero:\n")
       cat("120\n")
       TlastD <- 120
      }
      else{
      cat(" Input Drug Dose:\n")
      cat("80000\n")
      Dose <- 80000
      cat("\n")
      } 
      cat("\n Input the Title of x-axis (time) for Plot:\n")
      cat(" (or press Enter to use default value - Time)\n\n")
      cat("Time (x-axis)\n")
      xaxis<-"Time after Dosing (hr)"
      cat("\n")

      cat("\n Input the title of y-axis (Conc.)for plot:\n")
      cat(" (or press Enter to use default value - Conc.\n\n")
      cat("Conc.(y-axis)\n")
      yaxis<-"Drug Plasma Conc. (ng/mL)"
      cat("\n")

     }   
  else {
    ### cat(" Input Drug Dose:\n")
    Dose<- dosez
    if (Dose<=0)  Dose<-0  else Dose<-as.numeric(Dose) 
       repeat{ 
        if (Dose==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press input the Dose again.               \n")
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
    ### cat(" Input Dosing Interval (Tau):\n")
    Tau<- DosingTau 
    if (Tau<=0)  Tau<-0  else Tau<-as.numeric(Tau)
       repeat{
        if (Tau==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press input the Dosing Interval again.    \n")
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
     ### cat(" Input the Time Point of the Final Dose since Time zero:\n")
     TlastD<- Tlastz
     if (TlastD<=0)  TlastD<-0  else TlastD<-as.numeric(TlastD)
       repeat{
        if (TlastD==0 ){
         cat("\n")
         cat("*******************************************\n")
         cat(" Parameter value can not be zero or empty. \n")
         cat(" Press input it again.                     \n")
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
    
    ### cat("\n Input the title of x-axis (Time) for plot:\n")
    ### cat("(or Press Enter to use default - 'Time after Dosing')\n\n") 
    xaxis<-xlabz
    if (substr(xaxis, 1, 1) == "")  xaxis<-"Time after Dosing"  else xaxis<-xaxis
     
    ### cat("\n Input the title of y-axis (Conc.) for plot:\n")
    ### cat("(or Press Enter to use default - 'Drug Plasma Conc.')\n\n") 
    yaxis<-ylabz
    if (substr(yaxis, 1, 1) == "")  yaxis<-"Drug Plasma Conc."  else yaxis<-yaxis
    cat("\n\n Please Wait...  Processing now. \n\n")
 }
    if(multiple){
       return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose,Tau=Tau, TlastD=TlastD ))
       }
     else{
       return(list(xaxis=xaxis,yaxis=yaxis, Dose=Dose ))
       }
} 