\name{RepMIXdata}

\alias{RepMIXdata}

\title{Input/Edit data  for lme function for replicated study}

\description{
  ->subject no.(subj)
  ->drug
       1:Reference
       2:Test
  ->sequence (seq)
       Sequence 1:Reference-->Test sequence
       Sequence 2:Test-->Reference sequence
   ->period (prd)
       Period 1: first treatmetn period
       Period 2: second treatmetn period
   ->Cmax
   ->AUC0t: area under the predicted plasma concentration time curve for
            test data. (time = 0 to t)
   ->AUC0INF: area under the predicted plasma concentration time curve for
             test data. (time = 0 to infinity)
   ->LnCmax: Log-transformed Cmax
   ->LnAUC0t: Log-transformed AUC0t
   ->LnAUC0INF: Log-transformed AUC0INF

}

\keyword{misc}