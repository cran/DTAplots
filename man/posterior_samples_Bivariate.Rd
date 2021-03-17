\name{posterior_samples_Bivariate dataset}
\alias{posterior_samples_Bivariate}
\docType{data}
\title{ Samples of the posterior distributions obtained from a Bayesian bivariate meta-analysis model}


\description{This dataset provides a sample from the posterior distributions of the various parameters resulting from a Bayesian bivariate meta-analysis model, that are needed to create a summary plot in the receiver operating characteristic (ROC) space.

The posterior samples were obtained by fitting the Bivariate model using the rjags package to data from a systematic review of anti-cyclic citrullinated peptide antibody (anti-CCP) for rheumatoid arthritis.  Overall, 37 studies were meta-analyzed, a total of 3 chains were used with 14,000 burn-in iterations after which we drew 1,000 samples from the posterior distributions for the following parameters:

\dQuote{mu[1]} for the mean logit-transformed sensitivity. \cr
\dQuote{mu[2]} for the mean logit-transformed specificity. \cr
\dQuote{tau[1]} for the between-study standard deviation in the logit-transformed sensitivity. \cr
\dQuote{tau[2]} for the between-study standard deviation in the logit-transformed specificity. \cr
\dQuote{rho} for the correlation between the mean logit-transformed  sensitivity and the mean logit-transformed specificity. \cr
\dQuote{se} for the logit-transformed sensitivity in individual studies. \cr
\dQuote{sp} for the logit-transformed specificity in individual studies. \cr
\dQuote{Mean_Se} for the mean sensitivity. \cr
\dQuote{Mean_Sp} for the mean specificity. \cr}

\usage{data(posterior_samples_Bivariate)}


\format{
  A matrix with 3,000 samples from the posterior distribution for each of the 81 parameters listed in the description above.
}


\references{
Nishimura K, Sugiyama D, Kogata Y, Tsuji G, Nakazawa T, Kawano S, Saigo K, Morinobu A, Koshiba M, Kuntz KM, Kamae I, Kumagai S. Meta-analysis: diagnostic accuracy of anti-cyclic citrullinated peptide antibody and rheumatoid factor for rheumatoid arthritis. Ann Intern Med. 2007 Jun 5;146(11):797-808. doi: 10.7326/0003-4819-146-11-200706050-00008. PMID: 17548411.
}


\keyword{datasets}
