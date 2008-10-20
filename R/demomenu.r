##demo for NCA
demomenu<-function()
{
cat("\n")
  file.menu <- c("lambda_z est. from the exact 3 data points",
                 "lambda_z est. with adjusted R sq. (ARS)",
                 "lambda_z est. with Akaike information criterion (AIC)",
                 "lambda_z est. with Two-Times-Tmax method (TTT)",
                 "lambda_z est. with TTT and ARS",
                 "lambda_z est. with TTT and AIC",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ")
    description_NCAinput()
    data(TotalSingledata)
    cat("\n\n")

     ##NCAanalyze or NCAGLManalyze
     with(entertitle.demo(), {
     description_drug()  

      Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
      Ref<-rbind(Singledata[[1]],Singledata[[4]])
      Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
      SingleRdata<-Refdata[ do.call(order, Refdata) ,]
      show(SingleRdata)
      SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
      SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
      SingleRdata1 <- na.omit(SingleRdata1)
        cat("\n\n")
      Test<-rbind(Singledata[[2]],Singledata[[3]])
      Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
      SingleTdata<-Testdata[ do.call(order, Testdata) ,]
      show(SingleTdata)
      SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
      SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
      SingleTdata1 <- na.omit(SingleTdata1)
    
    if (pick == 1){
      cat("\n")
        Totalplot<- rbind(SingleRdata,SingleTdata)
        NCAselectdemo(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        }
    
    else {
    if (pick == 2){
        cat("\n")
       Totalplot<- rbind(SingleRdata,SingleTdata)
       ARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 3){
        cat("\n")
       Totalplot<- rbind(SingleRdata,SingleTdata)
       AICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 4){
        cat("\n")
       Totalplot<- rbind(SingleRdata,SingleTdata)
       TTTdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 5){
        cat("\n")
       Totalplot<- rbind(SingleRdata,SingleTdata)
       TTTARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         }
   else {
    if (pick == 6){
        cat("\n")
       Totalplot<- rbind(SingleRdata,SingleTdata) 
       TTTAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        }
    else {
    if (pick == 7){
        cat("\n")
        NCAmenu()
         }       
     else {
    if (pick == 8){
       cat("\n")
       cat("\nThank you for using bear!  Bye now. \n")
              }      
             }
           }
        }
      }
     }
    }
   }
  })
}   