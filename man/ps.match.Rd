\name{ps.match}
\alias{ps.match}
\title{Propensity score matching}

\description{

  Matching based on the estimated propensity score }

\usage{

 ps.match(object, object.control=NULL, matched.by=NULL,
 control.matched.by=matched.by, who.treated=1, treat=NULL,
 name.match.index="match.index", ratio=1, caliper="logit", x=0.2,
 givenTmatchingC=TRUE, bestmatch.first=TRUE, setseed=FALSE,
 combine.output=TRUE)

 }

 \arguments{
   
  \item{object}{an object of class 'pscore' or a data frame.}
  
  \item{object.control}{a data frame. It is needed if \code{object} is a
    data frame including only treated or untreated observations,
    respectively.}
  
  \item{matched.by}{an integer or a string indicating the matching
    variable. The default is NULL, i.e. if the class of the input object
    is 'pscore', object value 'pscore' is automatically used.}
    
  \item{control.matched.by}{an integer or a string indicating the
    matching variable in \code{object.control}. The default is
    'matched.by'.}

  \item{who.treated}{an integer or a string indicating which value of
    \code{treat} labels the 'treated' observations.}

  \item{treat}{an integer or a string indicating the treatment variable
    in data both of \code{object} and \code{object.control}. If the
    class of the input object is 'pscore', no specification is needed.}

  \item{name.match.index}{a string indicating the name of the variable
    containing the matching indices.}
  
  \item{ratio}{an integer k indicating the matching ratio 1:k.}

  \item{caliper}{an integer or a string indicating the maximum width of
    the caliper for which matching should be done. The default is
    'logit', i.e. the maximum width of the caliper is \code{x} of the
    standard deviation of the logit of the matching variable.}

  \item{x}{a numeric value indicating the scale parameter for the
    calculation of the caliper if \code{caliper}='logit'.}

  \item{givenTmatchingC}{a logical value indicating who is matched to
    whom. The default is 'TRUE', i.e. untreated observations are matched
    to treated observations.}

  \item{bestmatch.first}{a logical value indicating how potential
    matching partners are matched. The default is 'TRUE', i.e.
    observations are matched with the best accordance regarding the
    matching variable. Otherwise, matching partners are randomly
    assigned from the pool of potential matching candidates.}

  \item{setseed}{an integer setting a random number for the matching
    process.}

  \item{combine.output}{a logical value. The default is 'TRUE', i.e. if
    \code{object} and \code{object.control} are given, 'data',
    'data.matched' and 'match.index' are combined as data frames or a
    vector, respectively. If 'FALSE', these values are returned as lists
    with entries corresponding to the input objects.}
}

\details{
  
  Matching by the estimated propensity score creates matching sets in
  which treated and untreated observations have identical or similar
  estimated propensity score. One or more untreated observations will be
  matched to each treated observation or vice versa.

  The caliper, i.e. the maximum distance between the estimated
  propensity scores of treated and untreated observations to be matched
  is generally defined as \code{x}=0.2 of the standard deviation of the
  (\code{caliper})=logit of the estimated propensity score.
  
  If function \code{pscore()} is previously used with default settings,
  \code{matched.by} has not to be specified. It is needed, if the
  matching variable in data is not labeled by 'pscore'. Also
  \code{treat} has not to be specified, contrary to the case where one
  or two data frames are given as input objects.  }

\value{

  \code{ps.match()} returns an object of class 'matched.pscore',
  'matched.data.frame' or 'matched.data.frames' depending on the
  class(es) of the input object(s) and \code{combine.output}. If the
  class of the input object is 'pscore', the output object inherits all
  components from the input object. The following components are
  available:
  
  \item{data}{a data frame containing the input data, extended by
    column(s) including the matching indices labeled by
    \code{name.match.index}. If the input object is a data frame, 'data'
    are sorted by treatment. If \code{object.control} is given and
    \code{combine.output}='FALSE', 'data' is a list of two data frames
    corresponding to the input objects.}

  \item{data.matched}{a data frame limiting 'data' only to matched
    observations.}

  \item{matched.by}{a string indicating the name of the matching
    variable.}

  \item{name.match.index}{a string indicating the name of matching
    indices generated at last.}

  \item{match.index}{a numeric vector containing the matching indices
    labeled by 'name.match.index' whereas '0' indicates 'no matching
    partner found'. If \code{combine.output}='FALSE', it is a list of
    two vectors corresponding to the input objects.}

  \item{match.parameters}{a list of matching parameters (\code{caliper},
    \code{ratio}, \code{who.treated}, \code{givenTmatchingC},
    \code{bestmatch.first}).}

  \item{formula.pscore}{a formula describing formally the propensity
    score model fitted at last in \code{pscore()}.}
  
  \item{model.pscore}{an object of class \code{glm} containing information
    about the propensity score model fitted at last  in \code{pscore()}.}
  
  \item{name.pscore}{a string indicating the name of propensity score
    estimated at last in \code{pscore()}.}
  
  \item{pscore}{a numeric vector containing the estimated propensity score
    labeled by \code{name.pscore}.}

  \item{name.treat}{a string indicating the name of the selected
  treatment variable.}

  \item{treat}{a numeric vector containing the treatment index labeled
    by \code{name.treat}.}  }

\author{Susanne Stampf \email{susanne.stampf@usb.ch}
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
}
\keyword{
  matching
}
