
    How to setup bear : 
    
    Methods                           setting     Methods                           setting  
    ------------------------------------------    ------------------------------------------ 
    run demo                     no(0),yes(1).    IndivDP_output               no(0), yes(1)
 
    study design           2x2x2 crossover(0),    ----------- multiple-dose only -----------   
                                 replicate(1),    *Tlast: the time of the last dose given for  
                                  parallel(2).     the multiple-dose study; will be ignored    
                                                   when it is a single-dose study; same as     
    single-/multiple-dose      single-dose(0),     Dosing interval.                            
                             multiple-dose(1).                                                 
                                                  ------------------------------------------   
    lambda_z estimation                           *x-axis label and y-axis label will be used  
     Adjusted R sq. (ARS).................(0),     in the plots of drug plasma conc. vs. time.   
     Akaike information criterion (AIC)...(1),    ------------------------------------------    
     Two-Times-Tmax(TTT)..................(2),                                                   
     TTT and ARS..........................(3),    -------------- partial AUC  --------------     
     TTT and AIC..........................(4),     pAUC: use truncated or partial AUC?            
     Select 2-6 data points manually......(5),     (0 - No, else - Yes)                           
     Load previous selection (.RData).....(6).     pAUC_start: the starting time of pAUC          
                                                   pAUC_end: the end time of pAUC               
    trapezoidal AUC     linear-up/log-down(0),     'pAUC_start' and 'pAUC_end' will be ignored  
                                all linear(1).     if pAUC is disabled (= 0).               
                                                   'pAUC_start' must be less than 'pAUC_end'.
    BE criterion lower limit (LL as %).... 80      ------------------------------------------ 
     upper limit (UL) = 1/LL.                                                                                                              
                                                   ------------ Indiv. DP output  ----------- 
    ODA (outlier detection analysis)    no(0),      IndivDP_output: (no(0), yes(1))           
                                       yes(1).      to enable this function, the original     
                                                    dataset is required same sampling times   
   ------------------------------------------       for each subject; time zero needs to be    
   (1) User can change these settings from the      input as '0'; and if there is any missing 
       top menu. Select [# Edit the setup files]    data or conc. which is BLQ MUST be input  
       from the top menu and then scroll up this    as 'NA' (not to be deleted).              
       terminal to see more.                      ***                                         
   (2) All these settings are not completely        IDP output: individual data point output  
       applied when running with demo dataset.      as .csv files. WARNING: do not use this   
   (3) *** WARNING: Be careful to set up correct    function if you have irregular sampling   
       methods; otherwise it may cause bear         time (such as using real times) for each  
       crashed accidentally.                        subject.                                  
   ------------------------------------------       ------------------------------------------  
     
     
    
    
    
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 