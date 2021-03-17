\name{Forest}
\alias{Forest}

\title{Forest plot for sensitivity and specificity}
\description{Function to create adjacent forest plots for both sensitivity and specificity also displaying study labels and 2x2 tables}
\usage{
Forest(Data, study = NULL, level = 0.95, conf.int = "Wilson",
se.axis = NULL, sp.axis = NULL, save_plot = NULL, res = 90, mean = NULL, digits = 2)
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
  \item{mean}{Default is NULL.  If data provided, it should be in the form of a vector with the following six entries with results of the meta-analysis, namely, mean sensitivity/specificity with respective credible/confidence intervals (order matters) :
					1- mean sensitivity (point estimate). \cr
					2- mean sensitivity lower bound. \cr
					3- mean sensitivity upper bound. \cr
					4- mean specificity (point estimate). \cr
					5- mean specificity lower bound. \cr
					6- mean specificity upper bound. \cr}

  \item{digits}{number of decimal places to display for sensitivity/specificity point estimates and respective interval limits.  Default = 2.}
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

# To display the summary mean sensitivity and mean specificity estimates previously obtained
# from a Bayesian bivariate meta-analysis model.
data(posterior_samples_Bivariate)
Mean_Se = median(posterior_samples_Bivariate[,1])
Mean_Se_low = quantile(posterior_samples_Bivariate[,1], prob=0.025)
Mean_Se_up = quantile(posterior_samples_Bivariate[,1], prob=0.975)
Mean_Sp = median(posterior_samples_Bivariate[,2])
Mean_Sp_low = quantile(posterior_samples_Bivariate[,2], prob=0.025)
Mean_Sp_up = quantile(posterior_samples_Bivariate[,2], prob=0.975)

Forest(Anti_CCP, study=Anti_CCP$Study, mean = c(Mean_Se, Mean_Se_low, Mean_Se_up,
      Mean_Sp, Mean_Sp_low, Mean_Sp_up))
}

\keyword{ Forest }
\keyword{ Plot }
\keyword{ Meta-analysis }
