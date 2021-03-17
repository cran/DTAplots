\name{posterior_samples_HSROC dataset}
\alias{posterior_samples_HSROC}
\docType{data}
\title{ Samples of the posterior distributions obtained from a Bayesian HSROC meta-analysis model}


\description{This dataset provides a sample from the posterior distributions of the various parameters resulting from a Bayesian hierarchical summary receiver operating characteristic (HSROC) meta-analysis model, that are needed to create a SROC plot in the receiver operating characteristic (ROC) space.

The posterior samples were obtained by fitting the HSROC model using the rjags package to data from a systematic review of anti-cyclic citrullinated peptide antibody (anti-CCP) for rheumatoid arthritis.  Overall, 37 studies were meta-analyzed, a total of 3 chains were used with 14,000 burn-in iterations after which we drew 1,000 samples from the posterior distributions for the following parameters:

\dQuote{THETA} for the overall mean cut-off value for defining a positive test (threshold parameter). \cr
\dQuote{LAMBDA} for the overall difference in means (accuracy parameter). \cr
\dQuote{beta} for the logarithm of the ratio of the standard deviation of test results among patients with and without the disease. \cr
\dQuote{tau[1]} for the standard deviation of the accuracy parameter. \cr
\dQuote{tau[2]} for the standard deviation of the threshold parameter. \cr
\dQuote{se} for the individual logit-transformed sensitivity. \cr
\dQuote{sp} for the individual logit-transformed specificity. \cr
}

\usage{data(posterior_samples_HSROC)}


\format{
  A matrix with 3,000 samples from the posterior distribution for each of the 79 parameters listed in the description above.
}


\references{
Nishimura K, Sugiyama D, Kogata Y, Tsuji G, Nakazawa T, Kawano S, Saigo K, Morinobu A, Koshiba M, Kuntz KM, Kamae I, Kumagai S. Meta-analysis: diagnostic accuracy of anti-cyclic citrullinated peptide antibody and rheumatoid factor for rheumatoid arthritis. Ann Intern Med. 2007 Jun 5;146(11):797-808. doi: 10.7326/0003-4819-146-11-200706050-00008. PMID: 17548411.
}


\keyword{datasets}
