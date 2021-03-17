\name{Anti_CCP}
\alias{Anti_CCP}
\docType{data}
\title{ Anti-CCP dataset }

\description{
This dataset gives the observed cross-tabulation of the Anti-cyclic citrullinated peptide (Anti-CCP) test for rheumatoid arthritis (test under evaluation) and the 1987 revised American college or Rheumatology (ACR) criteria or clinical diagnosis (reference test) for 37 studies.
}

\usage{data(Anti_CCP)}

\format{
  A data frame with 37 observations on the following 5 variables.
  \describe{
    \item{\code{Study}}{First author and year of publication of the study. }
    \item{\code{TP}}{Individuals who tested positive on both tests. }
    \item{\code{FP}}{Individuals who tested positive on the test under evaluation and negative on the reference test.}
    \item{\code{FN}}{Individuals who tested negative on the test under evaluation and positive on the reference test.}
    \item{\code{TN}}{Individuals who tested negative on both tests. }
  }
}



\references{ Nishimura K, Sugiyama D, Kogata Y, Tsuji G, Nakazawa T, Kawano S, Saigo K, Morinobu A, Koshiba M, Kuntz KM, Kamae I, Kumagai S. Meta-analysis: diagnostic accuracy of anti-cyclic citrullinated peptide antibody and rheumatoid factor for rheumatoid arthritis. Ann Intern Med. 2007 Jun 5;146(11):797-808. doi: 10.7326/0003-4819-146-11-200706050-00008. PMID: 17548411.

}
\keyword{datasets}
