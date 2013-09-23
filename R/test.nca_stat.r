##
## will be called by NCA.BANOVAmenu(); demo fro NCA -> ANOVA
##
test.nca_stat<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{

OutputFilez()
Demo<-NULL
Demo<<-TRUE
ODAnalysis<-NULL
ODAnalysis<<-FALSE
replicated=FALSE
parallel=FALSE
multiple=FALSE
TotalSingledata<-NULL

    description_NCAinput()
    filelocxx <- system.file("extdata", "TotalSingledata.rda", package="bear")
    load(filelocxx)  ## because it is a *.rda data file
    
    cat("\n\n")
    description_drug()
    Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
    Ref<-rbind(Singledata[[1]],Singledata[[4]])
    Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
    SingleRdata<-Refdata[ do.call(order, Refdata) ,]
    
    SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
    SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
    SingleRdata1 <- na.omit(SingleRdata1)
      cat("\n\n")
    Test<-rbind(Singledata[[2]],Singledata[[3]])
    Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
    SingleTdata<-Testdata[ do.call(order, Testdata) ,]
    
    SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
    SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
    SingleTdata1 <- na.omit(SingleTdata1)
    Totalplot<- rbind(SingleRdata,SingleTdata)
    create.products_sum(Totalplot)

    cat("\n")
    show(SingleRdata)
    show(SingleTdata)
    ### ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
} 