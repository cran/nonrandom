\name{ps.estimate}
\alias{ps.estimate}
\title{
  Propensity score based treatment effects
}
\description{ 
  Estimation of propensity score based treatment effects
}

\usage{
  ps.estimate(object,resp,
                        treat         = NULL,
                        stratum.index = NULL,
                        match.index   = NULL,
                        adj           = NULL,
                        weights       = "rr",
                        family        = "gaussian",
                        regr          = NULL,
                        ...)
}
\arguments{
  \item{object}{an object of class 'stratified.pscore',
    'stratified.data.frame', 'matched.pscore', 'matched.data.frame',
    'matched.data.frames' or a data frame.}
  
  \item{resp}{an integer or a string indicating response in data and in
    matched data if \code{ps.match()} is previously used.}

  \item{treat}{an integer or a string indicating treatment in data and
    in matched data if \code{ps.match()} is previously used. The default
    is 'NULL'. If the class of the input object is 'stratified.pscore'
    or 'matched.pscore', no specification is needed.}

  \item{stratum.index}{an integer or a string indicating the vector
    containing stratum indices in stratified data. The default is
    'NULL'. No specification is needed if \code{ps.makestrata()} is
    previously used.}

  \item{match.index}{an integer or a string indicating the vector
    containing matching indices in data and in matched data. The default
    is 'NULL'. No specification is needed if \code{ps.match()} is
    previously used.}


  \item{adj}{a formula or a vector of integers or strings indicating
    covariates in data and matched data. The default is NULL, i.e., no
    adjustment is done. If \code{adj} is a formula, it must be
    formulated as 'response~treatment+covariates' according to
    \code{resp} and \code{treat}. If \code{adj} is a vector, it contains
    names or integers indicating covariates for which the treatment
    effect has to be adjusted for.}

  \item{weights}{a string indicating how to weight stratum-specific
    estimates to obtain the overall estimate in case of stratified data.
    The default is 'rr' indicating stratum-specific weights proportional
    to the number of observations per stratum. In case of linear
    response, using \code{weights} = 'opt' means that weights are
    optimal in sence of variance minimizing.}

  \item{family}{a description of the error distribution and link
    function to be used in the model (see 'glm).}

  \item{regr}{a formula or a vector of integers or strings indicating
    covariates in data. The default is NULL, i.e., no
    regression model is fitted. If \code{regr} is a formula, it must be
    formulated as 'response~treatment+covariates' according to
    \code{resp} and \code{treat}. If \code{regr}, it contains names
    integers indicating covariates for which the treatment effect on
    response estimated by regression has to be adjusted.}

  \item{...}{further arguments passed to or from other methods.}
}
\details{
  Propensity score methods are used to estimate treatment effects in
  observational data. The treatment effects are estimated without
  adjustment and has to be interpreted as marginal effects. However, it
  is additionally possible to adjust for residual imbalances in strata
  or in the matched data (\code{adj}) and also to apply traditional
  regression (\code{regr}).

  The usage of \code{ps.estimate()} depends slightly on the class of the
  input object. If either \code{ps.makestrata()} or \code{ps.match()}
  are previously used, \code{treat}, \code{match.index} and
  \code{stratum.index} are not needed, contrary to the case where the
  input object is a data frame. If both \code{match.index} and
  \code{stratum.index} are specified, \code{stratum.index} will be
  ignored. If one specifies \code{adj} and \code{regr} as
  formulas, they must be identical in form of
  'response~treatment+covariates' and 'response' and 'treatment' must
  agree with \code{resp} and \code{treat}.
}
\value{
  \code{ps.estimate} returns an object of the same class as the input
  object. The number and the manner of values depends on the scale of
  \code{resp}:
  
  \item{data}{a data frame containing the input data.}
  
  \item{data.matched}{a data frame limiting 'data' only to matched
    observations. It is only available, if \code{ps.match()} is
    previously used.}

  \item{name.resp}{a string indicating the name of response.}

  \item{resp}{a numeric vector indicating response labeled by
    'name.resp'.}

  \item{name.stratum.index}{a string indicating the name of stratum
    indices used. It is only available, if \code{ps.makestrata()} is
    previously used.}

  \item{stratum.index}{a numeric vector containing stratum indices
    labeled by 'name.stratum.index'. It is only available, if
    \code{ps.makestrata()} is previously used.}

  \item{intervals}{a vector of characters indicating intervals. It is
    only available, if \code{ps.makestrata()} is previously used.}

  \item{stratified.by}{a string indicating the name of stratification
    variable. It is only available, if \code{ps.makestrata()} is
    previously used.}
  
  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last.}
  
  \item{model.pscore}{an object of class \code{glm} containing
    information about the propensity score model fitted at last.}
  
  \item{name.pscore}{a string indicating the name of propensity score
    estimated at last.}
  
  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by 'name.pscore.}

  \item{name.treat}{a string indicating the name of treatment.}

  \item{treat}{a numeric vector containing treatment labeled by
    'name.treat'.}

  \item{matched.by}{a string indicating the name of matching
    variable. It is only available, if \code{ps.match()} is previously
    used.}

  \item{name.match.index}{a string indicating the name of matching
    indices used. It is only available, if \code{ps.match()} is
    previously used.}

  \item{match.index}{a numeric vector containing matching indices
    labeled by 'name.match.index' whereas '0' indicates 'no matching
    partner found'. It is only available, if \code{ps.match()} is
    previously used.}

  \item{match.parameters}{a list of matching parameters (\code{caliper},
    \code{ratio}, \code{who.treated}, \code{givenTmatchingC},
    \code{bestmatch.first}). It is only available, if \code{ps.match()}
    is previously used.}

  \item{lr.estimation}{a list containing information about the 
    regression model applied to estimate the treatment effect on
    response.
    \describe{
      \item{effect}{the estimated conditional treatment effect based on
	regression. It is an odds ratio if response is binary.}
      
      \item{effect.marg}{the estimated marginal treatment effect based
	on regression. It is an odds ratio if response is
	binary. Conditional and marginal treatment effects are identical
	if response is assumed to be normally distributed.}

      \item{se}{the estimated standard error for 'effect'. If response
      is binary, the estimated standard error for 'effect' is given on
      the log scale.}

      \item{se.marg}{the estimated standard error for 'effect.marg'. If
      response is binary, the estimated standard error for 'effect.marg'
      is given on the log scale.}

      \item{regr.formula}{a formula describing formally the fitted
	response model.}

      \item{regr.model}{a \code{glm} object containing information of
	the response model.}
    }
  }
  \item{ps.estimation}{a list containing information about estimated
    propensity score based treatment effects on response. The list
    entries depend on the scale of response.
    \describe{
      \item{crude}{a list containing information about the crude
	treatment effect on response estimated by 'response~treatment':
	\describe{
	  \item{effect}{the estimated crude treatment effect on
	    response. It is an odds ratio if response is binary.}
	  \item{se}{the estimated standard error of 'effect'. It is on
	    log scale if response is binary.}
	}
      }
      \item{adj}{a list containing estimated propensity score based
	treatment effect on response adjusted according to \code{adj}:
	\describe{
	  \item{model}{response model applied in each stratum or in the
	    matched data to estimate adjusted propensity score based
	    treatment effects.}
	  \item{effect.str}{the estimated stratum-specific adjusted
	    treatment effects. It is an odds ratio if response is
	    binary. It is only availabe for stratified data.}
	  \item{effect}{the estimated adjusted treatment effect. It is
	    an odds ratio if response is binary.}
	  \item{se}{the estimated standard error of 'effect'. It is on
	    log scale if response is binary.}
	}
      }
      \item{unadj}{a list containing estimated unadjusted propensity
	score based treatment effect on response. The list entries
	depend on the scale of response:
	\describe{
	  \item{effect}{the estimated marginal treatment effect. It is
	    based on response rates and formulated as an odds ratio if
	    response is binary.}
	  \item{se}{the estimated standard error of 'effect'. It is on
	    log scale if response is binary.}
	  \item{p1}{a numeric indicating the estimated marginal response
	    probability in treatment labeled by the upper value. It is
	    only available if response is binary and for stratified
	    data.}
	  \item{p0}{a numeric indicating the estimated marginal response
	    probability in treatment labeled by the lower value. It is
	    only available if response is binary and for stratified
	    data.}
	  \item{p1.str}{a numeric indicating the estimated
	    stratum-specific response probabilities in treatment labeled
	    by the upper value. It is only available if response is
	    binary and for stratified data.}
	  \item{p0.str}{a numeric indicating the estimated
	    stratum-specific response probabilities in treatment labeled
	    by the lower value. It is only available if response is
	    binary and for stratified data.}
	  \item{effect.mh}{the stratified Mantel-Haenszel estimate for
	    the treatment effect on response. It is only available if
	    response is binary and for stratified data.}
	  \item{se.mh}{the estimated standard error for 'effect.mh'. It
	    is only available if response is binary and for stratified
	    data.}
	  \item{odds.str}{the estimated stratum-specific odds ratio used
	    for 'effect.mh'. It is only available if response is
	    binary and for stratified data.}
	}
      }
      \item{weights}{a string indicating the kind of weights used.}

      \item{weights.str}{a numeric vector containing stratum-specific
	weights. It is only available for stratified data.}
    }
  }
}
\references{ } \author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}
\note{
}
\seealso{
  \code{\link{glm}}, \link{formula}, \code{\link[lme4]{lmer}}
}
\examples{
## STU1
data(stu1)
stu1.ps <- pscore(data    = stu1, 
                  formula = therapie~tgr+age)
stu1.match <- ps.match(object          = stu1.ps,
                       ratio           = 2,
                       caliper         = 0.5,
                       givenTmatchingC = FALSE,
                       matched.by      = "pscore",
                       setseed         = 38902)
stu1.est <-
  ps.estimate(object  = stu1.match,
              resp    = "pst",
              adj     = "tmass",
              regr    = pst~therapie+tgr+age)

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
pride.est <-
  ps.estimate(object  = pride.strata,
              resp    = "SEVERE",
              family  = "binomial",
              adj     = "AGE",
              regr    = SEVERE ~PCR_RSV+SEX+ETHNO+FRUEHG+HERZ+
                        ELTATOP+REGION+AGE+TOBACCO+VOLLSTIL+EXT+
                        EINZ+KRANKSUM,
              weights = "rr")
}
\keyword{
  models
}
