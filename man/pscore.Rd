\name{pscore}
\alias{pscore}
\title{Propensity score estimation}
\description{
 Estimate the propensity score using a logistic regression model
}
\usage{
  pscore(formula, data, family="binomial", na.action=na.exclude,
  name.pscore="pscore", ...)
}
\arguments{
  \item{formula}{an object of class 'formula' (or one that can be coerced
    to that class): a symbolic description of a model to be fitted. The
    response given in \code{formula} must be labeled with '0' and '1'
    due to the internal use of \code{glm}.}

  \item{data}{a data frame containing response, treatment and
    covariates.}

  \item{family}{the error distribution and link function to be used in the
    model (see \code{glm}). The default is 'binomial'.}

  \item{na.action}{a function which indicates what should happen when data
    contain 'NA's. The default is 'na.exclude', i.e., data containing
    'NA' values are deleted (see \code{na.exclude}).}

  \item{name.pscore}{a string indicating the name of the estimated
    propensity score. The default is 'pscore'.}
  
  \item{...}{further arguments passed to or from other methods.}
}  
\details{
  The propensity score is the conditional probability of receiving a
  certain treatment given patient's covariates. It is generally unknown
  and has to be estimated, e.g., by using logistic
  regression. \code{pscore} can be used repeatedly and all estimated
  propensity scores are added on 'data'. But only the information of the
  propensity score estimated at last will be stored in values of the
  output object.
}
\value{
  \code{pscore} returns an object of class 'pscore' containing the
  following components:

  \item{data}{a data frame containing the input data, extended by
    column(s) including the estimated propensity score(s) labeled by
    \code{name.pscore}.}

  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last.}

  \item{model.pscore}{an object of class \code{glm} containing information
    about the propensity score model fitted at last.}

  \item{name.pscore}{a string indicating the name of the propensity
    score estimated at last.}

  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by 'name.pscore'.}

  \item{name.treat}{a string indicating the name of treatment given in
    \code{formula} as response.}

  \item{treat}{a numeric vector containing treatment labeled by
    'name.treat'.}
}
\references{
}
\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}
\note{
}
\seealso{
  \code{\link{glm}}, \link{formula}
}
\examples{
## STU1
data(stu1)
stu1.ps <- pscore(data    = stu1, 
                  formula = therapie~tgr+age)


## PRIDE
data(pride)
pride.ps <- pscore(data        = pride,
                   formula     = PCR_RSV~SEX+RSVINF+REGION+
                                 AGE+ELTATOP+EINZ+EXT,
                   name.pscore = "ps")
}
\keyword{
  models
}
