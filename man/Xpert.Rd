\name{Xpert}
\alias{Xpert}
\docType{data}
\title{ Xpert dataset }

\description{
This dataset gives the observed cross-tabulation of GeneXpertTM (Xpert) test for tuberculosis (TB) meningitis in extrapulmonary specimens (test under evaluation) and culture (reference test) for 29 studies.
}

\usage{data(Xpert)}

\format{
  A data frame with 29 observations on the following 5 variables.
  \describe{
    \item{\code{Study}}{First author and year of publication of the study. }
    \item{\code{++}}{Individuals who tested positive on both tests. }
    \item{\code{+-}}{Individuals who tested positive on the test under evaluation and negative on the reference test.}
    \item{\code{-+}}{Individuals who tested negative on the test under evaluation and positive on the reference test.}
    \item{\code{--}}{Individuals who tested negative on both tests. }
  }
}



\references{ Kohli M, Schiller I, Dendukuri N, et al. Xpert® MTB/RIF assay for extrapulmonary tuberculosis and rifampicin resistance. Cochrane Database Syst Rev. 2018;8(8):CD012768.
}
\keyword{datasets}
