\name{Forest}
\alias{Forest}

\title{Forest plot for sensitivity and specificity}
\description{Function to create adjacent forest plots for both sensitivity and specificity also displaying study labels and 2x2 tables}
\usage{
Forest(Data, study = NULL, level = 0.95, conf.int = "Wilson",
se.axis = NULL, sp.axis = NULL, save_plot = NULL, res = 90,
summary = NULL, digits = 2, summary_label = NULL,
study.cex=1, summary.cex=1.1,
col.headers = c("Study", "TP", "FP", "FN", "TN", "Sens (95\% CI)",
"Spec (95\% CI)", "Sens (95\% CI)", "Spec (95\% CI)"))
}

\arguments{
  \item{Data}{a data frame with the number of rows equal to the number of studies and 4 columns.  Each row consists of the entries of the 2x2 table of the index test (i.e. test under evaluation)
	vs. the reference test reported in each study.  The ordering of the columns is ++, +-, -+, --, where the first entry refers to the result of the test under evaluation and the
	second entry refers to the result of the reference test. The header of the data frame must be "TP", "FP", "FN" and "TN", respectively.}
  \item{study}{a character vector of study/labels, e.g. Author (and Year) for each study.  If NULL, the function will generate generic labels : "Study 1, Study 2, ..."}
  \item{level}{confidence level of the confidence intervals for sensitivity and specificity estimates.}
  \item{conf.int}{method used to compute the confidence interval for a biomial proportion.  Either "Normal.approx" or "Wilson" (default).}
  \item{se.axis}{x-axis range for sensitivity, based on a vector of 2 components.  The first component should be the minimum of the desired x-axis range, while the
	second component should be  the maximum of the desired x-axis range.  If NULL (default), the range is calculated from the Data (min,max) of sensitivity.}
  \item{sp.axis}{x-axis range for specificity, based on a vector of 2 components.  The first component should be the minimum of the desired x-axis range, while the
	second component should be  the maximum of the desired x-axis range.  If NULL (default), the range is calculated from the Data (min,max) of specificity.}
  \item{save_plot}{character string pointing to the directory where the plot will be saved in .jpeg format.  If NULL, no plot will be saved.}
  \item{res}{esolution (dpi) of the jpeg file.}
  \item{summary}{Default is NULL.  This will add summary point estimates that were calculated externaly.  Multiple summary points can be provided.  If provided, summary should be in the form of a matrix with with each row representing one summary point made of 6 columns.  The columns should be (order matters) :
					1- summary point estimate for individuals truly positive (i.e. sensitivity). \cr
					2- lower bound of the 95\% CI for corresponding summary point estimate. \cr
					3- upper bound of the 95\% CI for corresponding summary point estimate. \cr
					4- summary point estimate for individuals truly negative (i.e. specificity). \cr
					5- lower bound of the 95\% CI for corresponding summary point estimate. \cr
					6- upper bound of the 95\% CI for corresponding summary point estimate. \cr}

  \item{digits}{number of decimal places to display for sensitivity/specificity point estimates and respective interval limits.  Default = 2.}
  \item{summary_label}{To provide names to the summary point estimates provided by summary argument.  Default is NULL}
  \item{study.cex}{Change font size of study labels provided by argument study}
  \item{summary.cex}{Change font size of summary labels provided by argument summary_label}
  \item{col.headers}{To change the headers of the forest plot sections.}
}

\value{No return value}

\author{Ian Schiller and Nandini Dendukuri}

\examples{
# FUNCTION USED WITH DEFAULT ARGUMENTS
data(Anti_CCP)
Forest(Anti_CCP)

# CONFIDENCE INTERVAL BASED ON THE NORMAL APPROXIMATION METHOD
Forest(Anti_CCP, conf.int = "Normal.approx")

# SET THE SENSITIVITY & SPECIFICITY X-AXES FROM 0 TO 1 INSTEAD
Forest(Anti_CCP, se.axis = c(0,1), sp.axis = c(0,1)	)

# PROVIDE STUDY LABELS
Forest(Anti_CCP, study=Anti_CCP$Study)

# To display the sensitivity and specificity summary point estimates previously obtained
# from a Bayesian bivariate meta-analysis model.
data(posterior_samples_Bivariate)
Summary_Se = median(posterior_samples_Bivariate[,1])
Summary_Se_low = quantile(posterior_samples_Bivariate[,1], prob=0.025)
Summary_Se_up = quantile(posterior_samples_Bivariate[,1], prob=0.975)
Summary_Sp = median(posterior_samples_Bivariate[,2])
Summary_Sp_low = quantile(posterior_samples_Bivariate[,2], prob=0.025)
Summary_Sp_up = quantile(posterior_samples_Bivariate[,2], prob=0.975)

Forest(Anti_CCP, study=Anti_CCP$Study, summary = cbind(Summary_Se, Summary_Se_low, Summary_Se_up,
      Summary_Sp, Summary_Sp_low, Summary_Sp_up))

# Random summary point estimates generated for illustrative example
n = 5 # Simulate summary point estimates coming from 5 different models
Summary_Se = runif(n, 0.5,0.6)
Summary_Se_low = runif(n, 0.4, 0.5)
Summary_Se_up = runif(n, 0.6,0.7)
Summary_Sp =runif(n, 0.9,0.95)
Summary_Sp_low = runif(n, 0.85,0.9)
Summary_Sp_up = runif(n, 0.95,1)

Forest(Anti_CCP, study=Anti_CCP$Study, summary = cbind(Summary_Se, Summary_Se_low, Summary_Se_up,
      Summary_Sp, Summary_Sp_low, Summary_Sp_up), summary_label=c("Summary estimate 1",
      "Summary estimate 2", "Summary estimate 3", "Summary estimate 4", "Summary estimate 5"))

}

\keyword{ Forest }
\keyword{ Plot }
\keyword{ Meta-analysis }
