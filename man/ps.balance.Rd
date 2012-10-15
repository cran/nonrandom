\name{ps.balance}
\alias{ps.balance}

\title{

  Statistical tests and standardized differences for balance checks

}

\description{ 

  Apply statistical tests or calculate standardized differences for
  balance checks of covariate distributions between treatment groups

}

\usage{

  ps.balance(object, sel=NULL, treat=NULL, stratum.index=NULL,
  match.index=NULL,method="classical", cat.levels=2, alpha=5,
  equal=TRUE)

}

\arguments{

  \item{object}{an object of class 'stratified.pscore',
    'stratified.data.frame', 'matched.pscore', 'matched.data.frame',
    'matched.data.frames' or a data frame.}
  
  \item{sel}{a data frame or a vector of integers or strings indicating
    covariates to be checked. The default is 'NULL', i.e. all variables
    in the data are selected.}
  
  \item{treat}{an integer or a string indicating the treatment variable
    in the data (and in the matched data if \code{ps.match()} is
    previously used). If the class of the input object is
    'stratified.pscore' or 'matched.pscore', no specification is
    needed.}
  
  \item{stratum.index}{an integer or a string indicating the vector
    containing the stratum indices in the stratified data. No
    specification is needed if \code{ps.makestrata()} is previously
    used.}

  \item{match.index}{an integer or a string indicating the vector
    containing matching indices in data and in matched data. No
    specification is needed if \code{ps.match()} is previously used.}
  
  \item{method}{a string indicating the method used to decide about
    covariate balance between treatment groups. The default is
    'classical', i.e., \code{ttest()} for continuous and
    \code{chisq.test()} for categorical covariates are applied, both in
    original data and in stratified or matched data. If 'stand.diff',
    standardized differences are calculated for covariates before and
    after stratification or matching (formulas are nicely explaind in:
    D. Yang and J.E. Dalton: 'A unified approach to measuring the effect
    size between two groups using SAS', Paper 335-2012, SAS Global Forum
    2012: Statistics and Data Analysis).}
  
  \item{cat.levels}{an integer indicating the maximal number of levels
    of selected categorical covariates to consider them as
    categorical. The default is '2', i.e., covariates with more than two
    different values are considered as continuous. For example, cov1 and
    cov2 has three and four levels, respectively. \code{cat.levels}
    should be set to 4 to consider both as categorical. Caution: If
    covariates are factors and \code{cat.levels} is not appropriately
    chosen, errors can occur since \code{t.test} can not be performed!}
  
  \item{alpha}{an integer indicating the significance level (per cent)
    or the cutpoint at which the decision about balance or imbalance is
    made in case of standardized differences.}
  
  \item{equal}{a logical value. The default is 'TRUE', i.e.
    equally-sized weights are used to combine standard deviations of
    covariates in treatment groups for calculating standardized
    differences. If 'FALSE', weights are proportions of observations in
    treatment groups within data, matched data and strata.}
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
  object. The number and the manner of the values depends on the used
  \code{method}:
  
  \item{data}{a data frame containing the input data.}
  
  \item{data.matched}{a data frame limiting 'data' only to matched
    observations. It is only available if \code{ps.match()} is
    previously used.}

  \item{name.stratum.index}{a string indicating the name of the selected
    stratum variable. It is only available if \code{ps.makestrata()} is
    previously used.}

  \item{stratum.index}{a numeric vector containing stratum indices
  labeled by 'name.stratum.index'. It is only available if
  \code{ps.makestrata()} is previously used.}

  \item{intervals}{a vector of characters indicating intervals. It is
    only available if \code{ps.makestrata()} is previously used.}

  \item{stratified.by}{a string indicating the name of stratification
    variable. It is only available if \code{ps.makestrata()} is
    previously used.}
  
  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last in \code{pscore()} .}
  
  \item{model.pscore}{an object of class \code{glm} containing
    information about the propensity score model fitted at last in
    \code{pscore()}.}
  
  \item{name.pscore}{a string indicating the name of propensity score at
  last estimated via \code{pscore()} and saved in the data.}
  
  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by 'name.pscore'.}

  \item{name.treat}{a string indicating the name of the treatment
  variable.}

  \item{treat}{a numeric vector containing the treatment variable
    labeled by 'name.treat'.}

  \item{matched.by}{a string indicating the name of the matching
    variable. It is only available if \code{ps.match()} is previously
    used.}

  \item{name.match.index}{a string indicating the name of the selected
    matching variable. It is only available if \code{ps.match()} is
    previously used.}

  \item{match.index}{a numeric vector containing the matching indices
    labeled by 'name.match.index' whereas '0' indicates 'no matching
    partner found'. It is only available if \code{ps.match()} is
    previously used.}

  \item{match.parameters}{a list of matching parameters including
    \code{caliper}, \code{ratio}, \code{who.treated},
    \code{givenTmatchingC} and \code{bestmatch.first}. It is only
    available if \code{ps.match()} is previously used.}
  
  \item{bal.test}{a list of elements describing the results for the
    performed balance checks.
    
    \describe{

      \item{balance.table}{a 2xK table describing the balance of K
        covariate distributions between treatment groups. The
        first/second row presents results from balance checks
        before/after stratification or matching. '0'/'1' indicates
        significant/non-significant differences between treatment
        groups.}
  
      \item{balance.table.summary}{a 2x2 table summarizing balance
	information from 'balance.table' across all K covariates. Only
	variables with balance tests done correctly are accounted for
	this table.}
  
      \item{covariates.NA}{a vector of strings indicating the names of
	covariates for which balance checks could not be done
	correctly.}
  
      \item{covariates.bal.before}{a vector of strings indicating the
	names of covariates balanced before stratification or matching.}
  
      \item{covariates.bal.after}{a vector of strings indicating the
	names of covariates balanced after stratification or matching.}
  
      \item{p.value}{a (s+1)xK table containing p-values from
	statistical tests for K covariates before (first row) and after
	(2nd, ..., (s+1)st row) stratification or matching (s is the
	number of defined strata; if matching, s=1). It is only
	available if \code{method}='classical'.}
 
      \item{Stand.diff}{a 2xK or (s+1)xK table containing standardized
	differences for K covariates before (first row) and after
	matching (2nd row) or stratification (2nd, ..., (s+1)st row). It
	is only available if \code{method}='stand.diff'.}

      \item{method}{a vector of strings indicating the scale of
	covariates assumed for balance checks ('none', 'bin', 'cat' or
	'num'). The value 'none' means that no balance check was
	performed and 'bin', 'cat' and 'num' indicate binary,
	categorical and continuous.}
      
      \item{alpha}{a numeric value defining the significance level or
	the cut point at which the decision about balance or imbalance
	is made.}
    }
  }
}


\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
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
pride.balance <- ps.balance(object     = pride.strata,
                            sel        = c(2:6),
                            method     = "classical",
                            cat.levels = 4, 
                            alpha      = 5)

}

