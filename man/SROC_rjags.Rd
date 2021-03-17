\name{SROC_rjags}
\alias{SROC_rjags}

\title{A function to create a summary plot in Receiver Operating Characteristic (ROC) space}
\description{
This function creates : 1) a plot of the mean sensitivity and mean specificity together with prediction and credible region resulting from a bivariate meta-analysis model, or 2) a summary ROC curve resulting from a hierarchical summary receiver operating characteristic (HSROC) meta-analysis model.  It uses posterior samples of ? from these meta-analysis models obtained  with rjags package
from a meta-analysis bivariate model for diagnostic test accuracy.  It allows for the possibility that the reference test is not perfect.
}
\usage{
SROC_rjags(X, n, model = "Bivariate",
dataset = NULL, ref_std=NULL, title = "Summary plot",
xlab = "Specificity", ylab = "Sensitivity",
Se.range=c(0,1), Sp.range=c(0,1),
SROC_curve = FALSE, lwd.curve=3, col.curve="black", lty.curve="solid",
cred_region = TRUE, predict_region = TRUE,
region_level = 0.95, lty.cred.region = "solid",
lty.predict.region = "dotted", trunc_low = 0.025, trunc_up = 0.025,
col.predict.region = "black", col.cred.region = "red",
lwd.cred.region = 2.5, lwd.predict.region = 2.5,
study_col1 = "blue", study_col2 = rgb(0, 0, 1, 0.15),
study_symbol = "circles", summary.point=TRUE,
pch.summary.point = 19, cex.summary.point = 3,
col.summary.point = 1)
}

\arguments{
  \item{X}{An mcmc.list object.  See details for further explanations.}
  \item{n}{Number of studies in the meta-analysis.}
  \item{model}{specify the model used to create \code{X}.  Default is "Bivariate".  Other option is "HSROC".  See details for further explanations.}
  \item{dataset}{a matrix with the number of rows equal to the number of studies and 4 columns.  Each row consists of the entries of the 2x2 table of the index test (i.e. test under evaluation)
	vs. the reference test reported in each study.  The ordering of the columns is ++, +-, -+, --, where the first entry refers to the result of the test under evaluation and the
	second entry refers to the result of the reference test.  If set to NULL (default), individual posterior estimates of test under evaluation characteristics, i.e.  points with
	coordinate (sensitivity, (1-specificity)), will not be drawn on the SROC plot.}
  \item{ref_std}{if "dataset" is not set to NULL, then "ref_std" must be set to TRUE if the meta-analysis model assumes a perfect reference standard test or to FALSE if the meta-analysis model assumes an imperfect reference standard test.}
  \item{title}{plot title.}
  \item{xlab}{x axis label.}
  \item{ylab}{y axis label.}
  \item{Se.range}{the y-axis limits of the plot.}
  \item{Sp.range}{the x-axis limits of the plot.}
  \item{SROC_curve}{logical. If TRUE, a SROC curve is drawn.}
  \item{lwd.curve}{The line width of the SROC curve, a positive number, defaulting to 3.  See "lwd" in "par".}
  \item{col.curve}{A specification for the default color of the SROC curve.  Defaults to "black".  See "col" in "par".}
  \item{lty.curve}{The SROC curve line type.  Default to "solid".  See "lty" in "par".}
  \item{cred_region}{logical. If TRUE (default), a credible region curve is drawn on the plot.}
  \item{predict_region}{logical. If TRUE (default), a prediction region curve is drawn on the plot.}
  \item{region_level}{The credible (prediction) level required for the credible (prediction) region. Default value ie 0.95.}
  \item{lty.cred.region}{The credible region line type. Default to "solid".  See "lty" in "par".}
  \item{lty.predict.region}{The prediction region line type. Default to "dotted".  See "lty" in "par".}
  \item{trunc_low}{Quantile to determine lower limit of threshold THETA used for creating the SROC curve. Default to 0.025.  See details for further explanations.}
  \item{trunc_up}{Quantile to determine upper limit of the threshold THETA used for creating the SROC curve.  Default to 0.025. See details for further explanations.}
  \item{col.predict.region}{specification for the default plotting color of the prediction region curve. Defaults to "black".}
  \item{col.cred.region}{specification for the default plotting color of the credible region curve. Defaults to "red".}
  \item{lwd.cred.region}{The line width of the credible region curve, a positive number, defaulting to 2.5.}
  \item{lwd.predict.region}{line width of the prediction region, a positive number, defaulting to 2.5.}
  \item{study_col1}{specification for the default plotting color of the individual study point perimeter. Defaults to "blue".}
  \item{study_col2}{specification for the default plotting color of the individual study point shading. Defaults to rgb(0, 0, 1, 0.15).}
  \item{study_symbol}{specification of the type of symbol to represent the individual study point.  Either "circles" (default) or "squares".}
  \item{summary.point}{logical.  If TRUE (default), a summary point estimate of coordinate (mean specificity, mean sensitivity) is drawn. }
  \item{pch.summary.point}{An integer specifying a symbol to represent the joint mean sensitivity and mean specificity summary point estimate.  See "pch" in "par".}
  \item{cex.summary.point}{The magnification to be used for the symbol to represent the joint mean sensitivity and mean specificity summary point estimate.}
  \item{col.summary.point}{A specification for the default color.}
}
\details{If model is set to \code{Bivariate}, the function expects that the parameters of the bivariate model have been spelled exactly as follows in the jags model  :

	\dQuote{mu[1]} for the mean logit-transformed sensitivity. \cr
	\dQuote{mu[2]} for the mean logit-transformed specificity. \cr
	\dQuote{tau[1]} for the between-study standard deviation in the logit-transformed sensitivity. \cr
	\dQuote{tau[2]} for the between-study standard deviation in the logit-transformed specificity. \cr
	\dQuote{rho} for the correlation between the logit-transformed sensitivity and the mean logit-transformed specificity. \cr
	\dQuote{se} for the logit-transformed sensitivity in individual studies. \cr
	\dQuote{sp} for the logit-transformed specificity in individual studies. \cr
	\dQuote{Mean_Se} for the mean sensitivity. \cr
	\dQuote{Mean_Sp} for the mean specificity. \cr

If model is set to \code{HSROC}, the function expects that the parameters of the HSROC model have been spelled exactly as follows in the jags model  :

	\dQuote{THETA} for the overall mean cut-off value for defining a positive test (threshold parameter). \cr
	\dQuote{LAMBDA} for the overall difference in means (accuracy parameter). \cr
	\dQuote{beta} for the logarithm of the ratio of the standard deviation of test results among patients with and without the disease. \cr
	\dQuote{tau[1]} for the standard deviation of the accuracy parameter. \cr
	\dQuote{tau[2]} for the standard deviation of the threshold parameter. \cr
	\dQuote{se} for the logit-transformed sensitivity in individual studies. \cr
	\dQuote{sp} for the logit-transformed specificity in individual studies. \cr


	The default values for arguments of the function assume the  posterior densities provided are for parameters of a Bivariate model.  Therefore, it will create a plot with a summary point, credible and prediction region curves.

	If the argument \code{SROC_curve} is set to true, the function will plot Sensitivity vs Specificity at the posterior mean values of LAMBDA and beta 	across values of the posterior sample of THETA.  The \code{trunc_low} and \code{trunc_up} arguments truncate the posterior sample of THETA by (\code{trunc_low})*100 to (\code{trunc_up})*100 to ensure the SROC curve remains within the observed scope of the study points.	}

\references{
Rutter, C. M., and Gatsonis, C. A. (2001) \emph{A hierarchical regression approach to meta-analysis of diagnostic accuracy evaluations}. Statistics in Medicine, 20(19):2865-2884.

Reitsma, J.B., Glas, A.S., Rutjes, A.W.S., Scholten, R.J.P.M., Bossuyt, P.M. and Zwinderman, A.H. (2005).  \emph{Bivariate analysis of sensitivity and specificity produces informative summary measures in diagnostic reviews}.  Journal of Clinical Epideiology 58, 982-90

Harbord, R.M., Deeks, J.J, Egger, M., Whiting, P., and Sterne, J.A. (2007) \emph{A unification of models for meta-analysis of diagnostic accuracy studies}. Biostatistics 8, 239-251.

Dendukuri, N., Schiller, I., Joseph, L., and Pai, M. (2012) \emph{Bayesian meta-analysis of the accuracy of a test for tuberculosis pleuritis in the absence of a gold-standard reference}. Biometrics. doi:10.1111/j. 1541-0420.2012.01773.x

}
\author{ Ian Schiller and Nandini Dendukuri }

\value{No return value}

\examples{
##############################################################################################
# The example requires the rjags output from a bivariate meta-analysis model.
# We use the posterior_samples_Bivariate dataset which contains the results  of a
# bivariate meta-analysis of Anti_CCP dataset.

#The number of studies
data(Anti_CCP)
n=dim(Anti_CCP)[1]

#We load the samples of the posterior distribution
data(posterior_samples_Bivariate)

# Summary plot
SROC_rjags(X=posterior_samples_Bivariate, model="Bivariate",n=n, study_col1="blue",
           study_col2=rgb(0, 0, 1, 0.15), dataset=Anti_CCP[,2:5], ref_std=TRUE)


##########################################################################################
# The example requires the rjags output from a HSROC meta-analysis model.
# We use the posterior_samples_HSROC dataset which contains the results of a
# HSROC meta-analysis of the Anti_CCP dataset.

#The number of studies
data(Anti_CCP)
n=dim(Anti_CCP)[1]

#We load the samples of the posterior distribution
data(posterior_samples_HSROC)

# Summary plot
SROC_rjags(X=posterior_samples_HSROC, model="HSROC",n=n, study_col1="blue",
           study_col2=rgb(0, 0, 1, 0.15), dataset=Anti_CCP[,2:5], ref_std=TRUE,
           SROC_curve = TRUE, cred_region = FALSE, predict_region = FALSE)

##########################################################################################
# The example requires the rjags output from a Bayesian latent class meta-analysis model.
# We use the posterior_samples_LC dataset which contains the results of a
# latent class meta-analysis of the Xpert dataset.

#The number of studies
data(Xpert)
n=dim(Xpert)[1]

#We load the samples of the posterior distribution
data(posterior_samples_LC)

# Summary plot
SROC_rjags(X=posterior_samples_LC, model="Bivariate",n=n, study_col1="blue",
           study_col2=rgb(0, 0, 1, 0.15), dataset=Xpert[,2:5], ref_std=FALSE)
}

\keyword{ HSROC }
\keyword{ Bivariate }
\keyword{ Meta-analysis }
