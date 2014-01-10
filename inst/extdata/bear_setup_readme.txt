
    How to setup bear : (close this dialog if you don't need it)

    ----------- lambda_z estimation ----------    ------------- AUC calculation ------------  
     Methods                            Values     Methods                            Values  
    ------------------------------------------    ------------------------------------------  
     Adjusted R sq. (ARS)................. 0       linear-up/log-down trapezoidal....... 0    
     Akaike information criterion (AIC)... 1       all linear trapezoidal.............else    
     Two-Times-Tmax(TTT).................. 2      ------------------------------------------  
     TTT and ARS.......................... 3   
     TTT and AIC.......................... 4      ----------- multiple-dose only -----------  
     Select 2-6 data points manually...... 5      *Tlast: the time of the last dose given for 
     Load previous selection (.RData)..... 6       the multiple-dose study; will be ignored   
    ------------------------------------------     when it is a single-dose study; same as    
                                                   Dosing interval.                           
    -------------- BE criterion --------------    *x-axis label and y-axis label will be used 
     lower limit (LL as %).......such as..80       in the plots of drug plasma conc. vs. time.
     upper limit (UL) = 1/LL; so no need to set   ------------------------------------------  
    ------------------------------------------    
    
    ------------ Indiv. DP output  -----------   -------------- partial AUC  --------------   
     IndivDP_output: (0 - No, else - Yes)         pAUC: use truncated or partial AUC?         
     to enable this function, the original        (0 - No, else - Yes)                        
     dataset is required same sampling times      pAUC_start: the starting time of pAUC       
     for each subject; time zero needs to be      pAUC_end: the end time of pAUC              
     input as '0'; and if there is any missing    'pAUC_start' and 'pAUC_end' will be ignored 
     data or conc. which is BLQ MUST be input     if pAUC is not disabled (= 0).              
     as 'NA' (not to be deleted).                ------------------------------------------   
    ------------------------------------------       
    ------------------------------------------   (1) User can change these settings from the  
     IDP output: individual data point output        top menu. Select [# Edit the setup files]
     as .csv files. WARNING: do not use this         from the top menu and then scroll up this
     function if you have irregular sampling         terminal to see more.                    
     time (such as using real times) for each    (2) All these settings are not completely    
     subject.                                        applied when running with demo dataset.  
    ------------------------------------------   (3) *** WARNING: Be careful to set up correct
                                                     methods; otherwise it may cause bear     
                                                     crashed accidentally.                    
    