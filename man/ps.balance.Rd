\name{ps.balance}
\alias{ps.balance}
\title{
  Statistical tests and standardized differences for balance checks
}
\description{ 
  Apply statistical tests or calculate standardized differences
  for balance checks of covariate distributions between treatment groups
}
\usage{
  ps.balance(object, sel=NULL, treat=NULL, stratum.index=NULL,
  match.index=NULL,method="classical", cat.levels=10, alpha=5,
  equal=TRUE)
}
\arguments{
  \item{object}{an object of class 'stratified.pscore',
    'stratified.data.frame', 'matched.pscore', 'matched.data.frame',
    'matched.data.frames' or a data frame.}
  
  \item{sel}{a data frame or a vector of integers or strings indicating
    covariates whose distribution to be checked. The default is 'NULL',
    i.e., the completer data are selected.}
  
  \item{treat}{an integer or a string indicating treatment in data and
    matched data if \code{ps.match()} is previously used. The default is
    'NULL'. If the class of the input object is 'stratified.pscore' or
    'matched.pscore', no specification is needed.}
  
  \item{stratum.index}{an integer or a string indicating the vector
    containing stratum indices in stratified data. The default is
    'NULL'. No specification is needed if \code{ps.makestrata()} is
    previously used.}

  \item{match.index}{an integer or a string indicating the vector
    containing matching indices in data and in matched data. The default
    is 'NULL'. No specification is needed if \code{ps.match()} is
    previously used.}
  
  \item{method}{a string indicating the method used to decide about
    balance of covariate distributions between treatment groups. The
    default is 'classical', i.e., \code{ttest()} for non-categorical and
    \code{chisq.test()} for categorical covariates are applied, both in
    original data and in stratified or matched data. If 'stand.diff',
    standardized differences are calculated for covariates before and
    after stratification or matching.}
  
  \item{cat.levels}{an integer. The default is '10', i.e., covariates
    with more than ten different values are considered as
    non-categorical.}
  
  \item{alpha}{an integer indicating the significance level (per cent)
    or the cutpoint at which the decision about balance or imbalance is
    made in case of standardized differences.}
  
  \item{equal}{a logical value. The default is 'TRUE', i.e.,
    equally-sized weights are used to combine standard deviations of
    covariates in case where standardized differences are calculated. If
    'FALSE', weights are proportions of observations in strata.}
}
\details{
  Propensity score methods aims to eliminate imbalances in covariate
  distributions between treatment groups. An important issue is to
  check those after stratification or matching. Statistical tests or
  standardized differences can be used for those balance checks.

  The usage of \code{ps.balance()} depends on the class of the input
  object. If either \code{ps.makestrata()} or \code{ps.match()} are
  previously used, \code{treat}, \code{match.index} and
  \code{stratum.index} are not needed, contrary to the case where the
  input object is a data frame.
}
\value{
  \code{ps.balance()} returns an object of the same class as the input
  object. The number and the manner of the values depends on
  \code{method}:
  
  \item{data}{a data frame containing the input data.}
  
  \item{data.matched}{a data frame limiting 'data' only to matched
    observations. It is only available, if \code{ps.match()} is
    previously used.}

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
    score model fitted at last in \code{pscore()} .}
  
  \item{model.pscore}{an object of class \code{glm} containing
    information about the propensity score model fitted at last in
    \code{pscore()}.}
  
  \item{name.pscore}{a string indicating the name of propensity score
    estimated at last in \code{pscore()}.}
  
  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by 'name.pscore'.}

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
  
  \item{bal.test}{a list of elements describing results of balance
    checks in different ways.
    \describe{
      \item{balance.table}{a 2xK table describing the balance of K
        covariate distributions between treatment groups. The first
        (second) row presents results from balance check before (after)
        stratification or matching.}
  
      \item{balance.table.summary}{a 2x2 table summarizing balance
	information from 'balance.table' across all K covariates. Only
	variables with balance tests done correctly are accounted for
	this table.}
  
      \item{covariates.NA}{a vector of strings indicating names of
	covariates whose balance checks could not be done correctly.}
  
      \item{covariates.bal.before}{a vector of strings indicating names
	of covariates balanced before stratification or matching.}
  
      \item{covariates.bal.after}{a vector of strings indicating names
	of covariates balanced after stratification or matching.}
  
      \item{p.value}{a (s+1)xK table containing p-values from
	statistical tests for K covariates before (first row) and after
	(2nd, ..., (s+1)st row) stratification or matching (s is the
	number of defined strata; if matching was done, s=2). It is only
	available if \code{method}='classical'.}
 
      \item{Means.treat.[min(treat)]}{a (s+1)xK table containing means
	for K covariates before (first row) and after (2nd, ...,
	(s+1)st row) stratification or matching (s is the number of
	defined strata; if matching was done, s=2) w.r.t. treatment with
	the lower value, e.g., '0' or 'No'. It is only available if
	\code{method}='stand.diff'.}
 
      \item{Means.treat.[max(treat)]}{a (s+1)xK table containing means
	for K covoariates before (first row) and after (2nd, ...,
	(s+1)st row) stratification or matching (s is the number of
	defined strata; if matching was done, s=2) w.r.t. treatment with
	the upper value, e.g., '1' or 'Yes'. It is only available
	if \code{method}='stand.diff'.}

      \item{SDs.treat.[min(treat)]}{a (s+1)xK table containing standard
	deviations for K covariates before (first row) and after (2nd,
	..., (s+1)st row) stratification or matching (s is the number of
	defined strata; if matching was done, s=2) w.r.t. treatment with
	the lower value, e.g., '0' or 'No'. It is only available if
	\code{method}='stand.diff'.}
 
      \item{SDs.treat.[max(treat)]}{a (s+1)xK table containing standard
	deviations for K covariates before (first row) and after (2nd,
	..., (s+1)st row) stratification or matching (s is the number of
	defined strata; if matching was done, s=2 ) w.r.t. treatment
	with the upper value, e.g., '1' or 'Yes'. It is only available
	if \code{method}='stand.diff'.}

      \item{Standardized.differences}{a 2xK table containing
	standardized differences for K covariates before (first row) and
	after (second row) stratification or matching. It is only
	available if \code{method}='stand.diff'.}

      \item{method}{a vector of strings indicating which statistical
	tests ('none', 'cat' or 'non-cat') are used or which scale
	('none', 'bin' or 'non-bin') for covariates is assumed if
	standardized differences are calculated.}
      
      \item{alpha}{a numeric value defining the significance level or
	the cut point at which the decision about balance or imbalance
	is made.}
    }
  }
}
\references{
}
\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}
\note{
}
\seealso{

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
stu1.balance <- ps.balance(object  = stu1.match,
                           sel     = c("tgr","age"),
                           method  = "stand.diff",    
                           alpha   =  20) 

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
pride.balance <- ps.balance(object  = pride.strata,
                            sel     = c(2:6),
                            method  = "classical",
                            alpha   = 5)  
}
\keyword{

}
