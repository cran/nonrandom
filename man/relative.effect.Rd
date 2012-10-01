\name{relative.effect}
\alias{relative.effect}
\title{Relative effects of covariates}
\description{
 Estimate the extent to which a covariate is confounding the treatment
 effect
}
\usage{
 relative.effect(formula=NULL, data, sel=NULL, resp=NULL, treat=NULL, ...)
}
\arguments{
  \item{formula}{an object of class 'formula' (or one that can be coerced
    to that class): a symbolic description of a model to be fitted.}

  \item{data}{a data frame containing response, treatment and
    covariates.}

  \item{sel}{a vector of integers or strings indicating covariates.}

  \item{resp}{an integer or a string indicating response.}

  \item{treat}{an integer or a string indicating treatment.}

  \item{...}{further arguments passed to or from other methods.}
}  
\details{
  The decision about the inclusion of covariates in the propensity score
  model is mostly difficult. A measure describing the extent to
  which a covariate is confounding the treatment effect on response can
  help to decide on it. Covariates with a large impact are potential
  candidates for the propensity score model.

  The relative effect is defined as difference between between adjusted
  and unadjusted treatment effect related to the unadjusted effect (per
  cent) and it is given on scale of the response. Therefore, treatment
  effects on response, unadjusted and adjusted for covariates, have to
  be estimated using internally \code{glm}.

  Two options are available to fit appropriate regression models. Either
  a formula is specified, typically as 'response ~ treatment +
  covariates' (\code{formula}), or \code{resp}, \code{treat} and
  \code{sel} are given to specify the response, the treatment and the
  covariates.
}
\value{
  \code{relative.effect} returns a list containing the following
  components:

  \item{unadj.treat}{the estimated unadjusted treatment effect on
    response.}

  \item{adj.treat.cov}{a vector containing the estimated adjusted
    treatment effects on response for each covariate.}

  \item{rel.treat}{a vector containing the relative effects for each
    covariate.}

  \item{name.treat}{a string indicating the name of the treatment.}

  \item{name.resp}{a string indicating the name of the response.}

  \item{name.sel}{a vector of strings indicating the names of the
    selected covariates.}

  \item{family}{the error distribution and link function used in the
    model (see \code{glm}).}
}

\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
  \code{\link{glm}}, \link{formula}
}
\examples{
## STU1
data(stu1)
stu1.effect <- 
  relative.effect(data = stu1,
                  formula = pst~therapie+tgr+age)

## PRIDE
data(pride)
pride.effect <- 
  relative.effect(data = pride,
                  sel = c(2:14),
                  resp = 15,
                  treat = 1)
}
\keyword{
  models
}
