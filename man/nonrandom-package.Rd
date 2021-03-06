\name{nonrandom-package}
\alias{nonrandom-package}
\alias{nonrandom}
\docType{package}

\title{

  A tool for a comprehensive data analysis from observational
  studies if stratification, matching and covariate adjustment by the
  propensity score is desired.

}

\description{

  \pkg{nonrandom} offers a comprehensive data analysis if
  stratification, matching and covariate adjustment by the propensity
  score should be applied. Several functions are implemented, starting
  from the selection of the propensity score model up to estimating
  propensity score based treatment effects. All functions can be applied
  separately as well as combined.

}

\details{

  \tabular{ll}{
    Package: \tab nonrandom\cr
    Type: \tab Package\cr
    Version: \tab 1.3\cr
    Date: \tab 2012-10-05\cr
    License: \tab What license is it under?\cr
  }
}

\section{The estimation of the propensity score}{The propensity score is
  the conditional probability of receiving a certain treatment given
  patient's covariates. It is generally unknown and has to be estimated,
  e.g. using logistic regression. The selection of an appropriate
  propensity score model is mostly difficult. A measure describing the
  extent to which a covariate is confounding the treatment effect on
  outcome is implemented in \code{relative.effect()}. \code{pscore}
  estimates the propensity score.  }

\section{Propensity score methods}{Propensity score methods aims to
  balance covariate distributions between treatment groups and allow for
  estimating marginal effects. Stratification (\code{ps.makestrata()})
  and matching (\code{ps.match})) by the estimated propensity score are
  the most popular methods to eliminate imbalances in covariate
  distributions. \code{ps.estimate} also offers covariate adjustment by
  the propensity score.}

\section{Balance checks}{An important, but often neglected issue in
  propensity score based data analyses is the check of covariate
  distributions between treatment groups after stratification and
  matching. Both graphical checks (\code{dist.plot} and
  \code{plot.stdf}) as well as classical statistical tests and
  standardized differences (\code{ps.balance}) can be used to examine
  covariate distributions.}

\section{Effect estimation}{After stratification and matching by the
  propensity score, treatment effects are estimated in the stratified an
  matched data, respectively. \code{ps.estimate} estimates those effects
  depending on the data structure. It offers an additional adjustment
  for residual imbalances in the stratified or matched data as well as
  the estimation of effect using traditional regression models including
  covariate adjustment by the propensity score.  }

\author{ Susanne Stampf

Maintainer:
Susanne Stampf <susanne.stampf@usb.ch>
}

\keyword{ package }

\examples{
## data on quality of life
data(stu1)

## estimate relative effects for covariates 'tgr' and 'age' regarding
## the effect of treatment 'therapie' on response 'pst'
rel.eff <- relative.effect(data    = stu1,
                           formula = pst~therapie+tgr+age)


## estimate the propensity score
ps <- pscore(data    = stu1,
             formula = therapie~tgr+age)


## stratify data
strata <- ps.makestrata(object = ps)


## match data in a ratio of 1:1 with a caliper size of 0.5
match <- ps.match(object  = ps,
                  ratio   = 1,
                  caliper = 0.5,
                  givenTmatchingC = FALSE)


## graphical check of distribution of both covariates between
## treatment groups in the matched data
bal.plot1 <- dist.plot(object = strata,
                      sel     = c("tmass"))

bal.plot2 <- dist.plot(object    = match,
                       sel       = c("alter"),
                       plot.type = 2,
                       compare   = TRUE)      


## calculate standardized differences of both covariates
## in case of matched data
bal.table <- ps.balance(object  = match,
                        sel     = c("tgr","age"),
                        method  = "stand.diff",
                        alpha   =  20)


## estimate propensity score based effects and in comparion the
## regression based treatment effect on response
ps.est <- ps.estimate(object = strata,
                      resp   = "pst",
                      regr   = pst~therapie+tgr+age)
}

