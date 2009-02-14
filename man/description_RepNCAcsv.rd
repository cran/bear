\name{description_RepNCAcsv}

\alias{description_RepNCAcsv}

\title{Description of NCA csv file for replicated study}

\description{

Data file should consist of
   row1: column title, such as subj, seq, prd, time, conc & etc.
column1: subject no.(subj)
column2: sequence (seq)
            -> Sequence = 1 if Ref.-->Test
            -> Sequence = 2 if Test-->Ref.
column3: period (prd)
            -> Period = 1: the 1st-treatment period
            -> Period = 2: the 2nd-treatment period

column4: treatment (drug)
column5: sampling time
column6: drug plasma/serum/blood concentration (conc)
}

\keyword{misc}