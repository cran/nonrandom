\name{ps.estimate}
\alias{ps.estimate}

\title{
  
  Estimation of propensity score based treatment effects

}

\description{ 

  Estimation of propensity score based treatment effects

}

\usage{

  ps.estimate(object,resp,treat = NULL, stratum.index = NULL,
  match.index = NULL, adj = NULL, weights = "rr", family = "gaussian",
  regr = NULL, ...)

}

\arguments{

  \item{object}{an object of class 'stratified.pscore',
    'stratified.data.frame', 'matched.pscore', 'matched.data.frame',
    'matched.data.frames' or a data frame.}
  
  \item{resp}{an integer or a string indicating the outcome variable in
    the data and in the matched data if \code{ps.match()} is previously
    used.}

  \item{treat}{an integer or a string indicating the treatment variable
    in the data and in the matched data if \code{ps.match()} is
    previously used. If the class of the input object is
    'stratified.pscore' or 'matched.pscore', no specification is
    needed.}

  \item{stratum.index}{an integer or a string indicating the vector
    containing the stratum indices. No specification is needed if
    \code{ps.makestrata()} is previously used.}

  \item{match.index}{an integer or a string indicating the vector
    containing the matching indices. No specification is needed if
    \code{ps.match()} is previously used.}

  \item{adj}{a formula or a vector of integers or strings indicating
    covariates. The default is NULL, i.e. no additional adjustment for
    covariates is done in stratified or matched data. If \code{adj} is a
    formula, it must be formulated as 'outcome~treatment+covariates'
    according to \code{resp} and \code{treat}. If \code{adj} is a
    vector, it contains names or integers indicating covariates for
    which the treatment effect has to be adjusted for.}

  \item{weights}{a string indicating how to weight the stratum-specific
    estimates to obtain an overall estimate in case of stratified data.
    The default is 'rr' indicating stratum-specific weights proportional
    to the stratum-specific number of observations. In case of linear
    outcome, \code{weights} = 'opt' can be used, i.e. weights are
    optimal in sence of variance minimizing.}

  \item{family}{a description of the error distribution and link
    function to be used in the model (see \code{glm}).}

  \item{regr}{a formula or a vector of integers or strings indicating
    covariates in data. The default is NULL, i.e. no regression model is
    fitted. If \code{regr} is a formula, it must be formulated as
    'outcome~treatment+covariates' according to \code{resp} and
    \code{treat}. If \code{regr} is a vector, it contains names or
    integers indicating covariates for which the treatment effect on
    outcome estimated by regression has to be adjusted.}

  \item{...}{further arguments passed to or from other methods.}
}

\details{

  Propensity score methods are used to estimate treatment effects in
  observational data. The treatment effects are estimated without
  adjustment and can to be interpreted as marginal effects. However, it
  is additionally possible to adjust for residual imbalances in strata
  or in the matched data (using \code{adj}) and also to apply
  traditional regression (using \code{regr}).

  The usage of \code{ps.estimate()} depends slightly on the class of the
  input object. If either \code{ps.makestrata()} or \code{ps.match()}
  are previously used, \code{treat}, \code{match.index} and
  \code{stratum.index} are not needed, contrary to the case where the
  input object is a data frame. If both \code{match.index} and
  \code{stratum.index} are specified, \code{stratum.index} will be
  ignored. If one specifies \code{adj} and \code{regr} as formulas, they
  must be identical in form of 'outcome~treatment+covariates' and
  'outcome' and 'treatment' must agree with \code{resp} and
  \code{treat}.

}

\value{

  \code{ps.estimate} returns an object of the same class as the input
  object. The number and the manner of values depends on the scale of
  \code{resp}:
  
  \item{data}{a data frame containing the input data.}
  
  \item{data.matched}{a data frame limiting 'data' only to matched
    observations. It is only available if \code{ps.match()} is
    previously used.}

  \item{name.resp}{a string indicating the name of the outcome
  variable.}

  \item{resp}{a numeric vector indicating the outcome variable labeled
    by 'name.resp'.}

  \item{name.stratum.index}{a string indicating the name of the selected
    stratum indices. It is only available if \code{ps.makestrata()} is
    previously used.}

  \item{stratum.index}{a numeric vector containing the selected stratum
    indices labeled by 'name.stratum.index'. It is only available if
    \code{ps.makestrata()} is previously used.}

  \item{intervals}{a vector of characters indicating the interval used
    for stratification. It is only available if \code{ps.makestrata()}
    is previously used.}

  \item{stratified.by}{a string indicating the name of the selected
    stratification variable. It is only available if
    \code{ps.makestrata()} is previously used.}
  
  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last.}
  
  \item{model.pscore}{an object of class \code{glm} containing
    information about the propensity score model fitted at last.}
  
  \item{name.pscore}{a string indicating the name of propensity score
    estimated at last.}
  
  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by 'name.pscore.}

  \item{name.treat}{a string indicating the name of the treatment
  variable.}

  \item{treat}{a numeric vector containing the treatment index labeled
    by 'name.treat'.}

  \item{matched.by}{a string indicating the name of the selected
    matching variable. It is only available if \code{ps.match()} is
    previously used.}

  \item{name.match.index}{a string indicating the name of the selected
    matching indices. It is only available if \code{ps.match()} is
    previously used.}

  \item{match.index}{a numeric vector containing the selected matching
    indices labeled by 'name.match.index' whereas '0' indicates 'no
    matching partner found'. It is only available if \code{ps.match()}
    is previously used.}

  \item{match.parameters}{a list of matching parameters including
    \code{caliper}, \code{ratio}, \code{who.treated},
    \code{givenTmatchingC} and \code{bestmatch.first}. It is only
    available if \code{ps.match()} is previously used.}

  \item{lr.estimation}{a list containing information about the
    regression model based treatment effect estimates. It correspond to
    the argument \code{regr}.

    \describe{

      \item{effect}{the estimated conditional treatment effect based on
	regression. It is an odds ratio for binary outcome.}
      
      \item{effect.marg}{the estimated marginal treatment effect based
	on regression. It is an odds ratio for binary
	outcome. Conditional and marginal treatment effects are
	identical if the outcome is assumed to be normally distributed.}

      \item{se}{the estimated standard error for \code{effect}. For
      binary outcome, it is given on the log scale.}

      \item{se.marg}{the estimated standard error for
      \code{effect.marg}. For binary outcome, it is given on the log
      scale.}

      \item{regr.formula}{a formula describing formally the fitted
	regression outcome model correspond to \code{regr}.}

      \item{regr.model}{a \code{glm} object containing information of
	the fitted regression outcome model correspond to \code{regr}.}

    }
  }
  
  \item{ps.estimation}{a list containing information about the estimated
    propensity score based treatment effects. The list entries depend on
    the scale of outcome.
    
    \describe{

      \item{crude}{a list containing information about the crude
	treatment effect estimated by 'outcome~treatment':
	
	\describe{

	  \item{effect}{the estimated crude treatment effect. For binary
	  outcome, it is an odds ratio.}

	  \item{se}{the estimated standard error of 'effect'. For binary
	    outcome, it is given on log scale.}

	}
      }
      
      \item{adj}{a list containing information about the estimated
	adjusted propensity score based treatment effect corresponding
	to \code{adj}:

	\describe{
	  
	  \item{model}{outcome model applied in each stratum or in the
	    matched data to adjusted for covariates.}

	  \item{effect.str}{the estimated adjusted stratum-specific
	    treatment effects. For binary outcome, it is an odds
	    ratio. It is only availabe for stratified data.}

	  \item{effect}{the estimated adjusted overall treatment
	    effect. For binary outcome, it is an odds ratio.}

	  \item{se}{the estimated standard error of \code{effect}. For
	    binary outcome, it is given on log scale.}

	}
      }
      
      \item{unadj}{a list containing information about the estimated
	unadjusted propensity score based treatment effect. The list
	entries depend on the scale of response:
	
	\describe{
	  
	  \item{effect}{the estimated marginal treatment effect.  For
	    binary outcome, it is an odds ratio based on outcome rates.}

	  \item{se}{the estimated standard error of \code{effect}. For
	    binary outcome, it is given on log scale.}

	  \item{p1}{a numeric indicating the estimated marginal outcome
	    probability for treatment labeled by the upper value. It is
	    only available for binary outcome and stratified data.}

	  \item{p0}{a numeric indicating the estimated marginal outcome
	    probability for treatment labeled by the lower value. It is
	    only available for binary outcome and stratified data.}

	  \item{p1.str}{a numeric indicating the estimated
	    stratum-specific outcome probabilities for treatment labeled
	    by the upper value. It is only available for binary outcome
	    and stratified data.}

	  \item{p0.str}{a numeric indicating the estimated
	    stratum-specific outcome probabilities for treatment labeled
	    by the lower value. It is only available for binary outcome
	    and stratified data.}

	  \item{effect.mh}{the stratified Mantel-Haenszel estimate for
	    the treatment effect. It is only available for binary
	    outcome and stratified data.}

	  \item{se.mh}{the estimated standard error for
	    \code{effect.mh}. It is only available for binary outcome
	    and stratified data.}

	  \item{odds.str}{the estimated stratum-specific odds ratio used
	    for the stratified Mantel-Haenzsel estimator. It is only
	    available for binary outcome and stratified data.}

	}
      }

      \item{weights}{a string indicating the selected weights scheme.}

      \item{weights.str}{a numeric vector containing the stratum-specific
	weights. It is only available for stratified data.}
      
    }
  }
}

\author{Susanne Stampf \email{susanne.stampf@usb.ch}
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
