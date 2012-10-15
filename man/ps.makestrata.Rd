\name{ps.makestrata}
\alias{ps.makestrata}
\title{Propensity score stratification}

\description{

  Stratification based on the estimated propensity score

}

\usage{

 ps.makestrata(object, breaks=NULL, name.stratum.index="stratum.index",
 stratified.by=NULL, ...)
}

\arguments{
  
  \item{object}{an object of class 'pscore' or a data frame.}  

  \item{breaks}{an integer, a numeric vector or suitable \code{R}
    function, e.g \code{quantile()}. The default is 'NULL', i.e.
    stratum bounds are automatically determined (see \code{cut()}).}

  \item{name.stratum.index}{a string indicating the variable name
    containing the stratum indices.}

  \item{stratified.by}{an integer or a string indicating the
    stratification variable in data. The default is NULL, i.e. if the
    class of the input object is 'pscore', object value 'pscore' is
    automatically used.}

  \item{...}{further arguments passed to or from other methods.}
}

\details{
  
  Stratification by the estimated propensity score groups observations
  with identical or similar estimated propensity score. 
  
  If function \code{pscore()} is previously used with default settings,
  \code{stratified.by} has not to be specified. It is needed if the
  stratification variable is not labeled by 'pscore'.
  
  Several options for the argument \code{breaks} are available. The
  default is 'NULL', i.e. values of the stratification variable are
  factorized and each factor correspond to a stratum. Using an integer,
  the number of strata is specified. If a numeric vector is given, its
  values indicate the stratum bounds.  }

\value{

  \code{ps.makestrata()} returns an object of class 'stratified.pscore'
  or 'stratified.data.frame' depending on the class of the input
  object. If the class of the input object is 'pscore', the output
  object inherits all components from the input object. The following
  components are available:
  
  \item{data}{a data frame containing the input data, extended by
    column(s) including stratum indices labeled by
    \code{name.stratum.index}.}

  \item{name.stratum.index}{a string indicating the name of the variable
    containing the stratum indices, generated at last.}

  \item{stratum.index}{a numeric vector containing the stratum indices
    labeled by 'name.stratum.index'.}

  \item{intervals}{a vector of characters indicating intervals
    corresponding to \code{stratum.index}.}

  \item{stratified.by}{a string indicating the name of the
    stratification variable.}
  
  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last in \code{pscore()}.}
  
  \item{model.pscore}{an object of class \code{glm} containing
    information about the propensity score model fitted at last in
    \code{pscore()}.}
  
  \item{name.pscore}{a string indicating the name of the propensity
    score estimated at last in \code{pscore()}.}
  
  \item{pscore}{a numeric vector containing the estimated propensity
    score labeled by 'name.pscore'.}

  \item{name.treat}{a string indicating the name of treatment used.}

  \item{treat}{a numeric vector containing treatment labeled by
    'name.treat'.}
}

\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
  \link{cut}, \link{quantile}
}
\examples{
## STU1
data(stu1)
stu1.ps <- pscore(data    = stu1, 
                  formula = therapie~tgr+age)
stu1.strata <- ps.makestrata(object = stu1.ps)


## PRIDE
data(pride)
pride.ps <- pscore(data        = pride,
                   formula     = PCR_RSV~SEX+RSVINF+REGION+
                                 AGE+ELTATOP+EINZ+EXT,
                   name.pscore = "ps")
pride.strata <- ps.makestrata(object = pride.ps,
                              breaks = quantile(pride.ps$pscore,  
                                                seq(0,1,0.2)),
                              stratified.ps = "ps")
}
\keyword{
  stratification
}
