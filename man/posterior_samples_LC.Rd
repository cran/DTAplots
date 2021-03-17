\name{posterior_samples_LC dataset}
\alias{posterior_samples_LC}
\docType{data}
\title{ Samples of the posterior distributions obtained from a Bayesian latent class (LC) meta-analysis model}


\description{This dataset provides a sample from the posterior distributions of various parameters resulting from an extended Bayesian bivariate meta-analysis model which accounts for the reference test to be imperfect.  These parameters can then be used to create a summary plot in the receiver operating characteristic (ROC) space.

The posterior samples were obtained by fitting the latent class model using the rjags pacakge to data from a Cochrane Review to assess the accuracy of the GeneXpertTM (Xpert) test for tuberculosis (TB) meningitis in extrapulmonary specimens.  In each study the index test was Xpert and the reference standard was culture. Both tests are known to have sub-optimal sensitivity, therefore the true TB meningitis status is unknown.  Overall, 29 studies were meta-analyzed, a total of 3 chains were used with 14,000 burn-in iterations after which we drew 1,000 samples from the posterior distributions for the following parameters :

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
  A matrix with 3,000 samples from the posterior distribution for each of the 67 parameters listed in the description above.
}


\references{
Kohli M, Schiller I, Dendukuri N, et al. Xpert® MTB/RIF assay for extrapulmonary tuberculosis and rifampicin resistance. Cochrane Database Syst Rev. 2018;8(8):CD012768.
}


\keyword{datasets}
