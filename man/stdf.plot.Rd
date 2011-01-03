\name{stdf.plot}
\alias{stdf.plot}
\title{Plot standardized differences}
\description{
  Plot standardized differences for covariates before and after
  matching.
}
\usage{
stdf.plot(object, sel = NULL, plot.alpha = TRUE, mymar = c(5, 8, 4, 2), pch.p = c(1, 5), col.p = c("black", "red"), cex.p = 1.25, line.stdf = 1, line.alpha = 4, with.legend = TRUE, legend.label = c("before", "after"), legend.cex = 1, legend.xy = NULL, ...)
}

\arguments{

  \item{object}{an object of class 'matched.pscore',
    'matched.data.frame', 'matched.data.frames' or a data frame for
    which \code{ps.balance} is previously used to compute standardized
    differences.
  }
  
  \item{sel}{a data frame or a vector of integers or strings indicating
    covariates whose computed standardized differences to be
    visualized. The default is 'NULL', i.e., standardized differences of
    covariates stored in value 'Standardized.differences' are plotted.
  }
  
  \item{plot.alpha}{a logical indicating whether alpha (cutpoint for
    balance) should be plotted.
  }
  \item{mymar}{margins to be specified on the four sides of the plot,
    see \code{par}. The default is 'c(5,8,4,2)'.
  }
  \item{pch.p}{an vector of two integers indicating the type of points
    which symbolize standardized differences before and after matching
    in the plot. The default is 'c(1,5)'.
  }
  \item{col.p}{a vector of two integers or strings indicating the color
    of points which symbolize standardized differences before and after
    matching in the plot. The default is 'c("black","red")'.
  }
  \item{cex.p}{an integer indicating the point size. The default is
    '1.25'.
  }
  \item{line.stdf}{an integer indicating the type of horizontal lines
    drawn between standardized differences before and after matching for
    all selected covariates. The default is '1'.
  }
  \item{line.alpha}{an integer indicating the type of a vertical line
    symbolizing alpha (cutpoint for decision about covariate
    balance). The default is '4'.
  }
  \item{with.legend}{a logical value indicating whether a legend is
    shown. The default is 'TRUE'.
  }
  \item{legend.label}{a vector of two strings describing the two legend
    labels. The default is c('before', 'after').
  }
  \item{legend.cex}{a numeric indicating the font size in the
    legend. The default is '1'.
  }
  \item{legend.xy}{a numeric vector of length two indicating x and y
    coordinates for the legend. The default is 'NULL', i.e., the legend
    is plotted below in the center of the plot.
  }
  \item{\dots}{further arguments for graphics.
  }
}
\details{
  The visualization of standardized differences of covariates before and
  after the matching procedure helps to decide whether the matching
  procedure was successful with respect to eliminate differences in
  covariate distribtions.
}

\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
\code{\link{plot}}
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

stdf.plot(object = stu1.balance,
          las    = 1)
}

\keyword{ plot }
