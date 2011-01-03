\name{pscore.plot}
\alias{pscore.plot}
\title{Plot the propensity score}

\description{Plot the density of the propensity score in treatment
groups}

\usage{
pscore.plot(object, par.dens = NULL, par.1 = NULL, par.0 = NULL, with.legend = FALSE, legend.cex = 0.9, legend.label = NULL, main = NULL, ylim = NULL, xlim = NULL, ...)
}

\arguments{

  \item{object}{
    an object of class 'pscore'.
  }
  \item{par.dens}{ a list of arguments internally used in \code{density}.
  }
  
  \item{par.1}{ a list of arguments internally used in \code{lines} for
    plotting the estimated propensity score in treatment group labeled by
    '1'.
  }
  \item{par.0}{ a list of arguments internally used in \code{lines} for
    plotting the estimated propensity score in treatment group labeled by
    '0'.
  }
  \item{with.legend}{a logical value indicating whether a legend is
    shown. The default is 'FALSE'. 
  }
  \item{legend.cex}{a numeric indicating the font size in the
    legend. The default is '0.9'.
  }
  \item{legend.label}{a vector of two strings describing the labels for
    treatment groups. The default is 'NULL', i.e. c('treated',
    'untreated') is used.
  }
  \item{main}{a string indicating the main title for graphics. The
    default is 'NULL'.
  }
  \item{ylim}{a vector of integers of length two indicating limits for
    the y axis. The default is 'NULL'.
  }
  \item{xlim}{a vector of integers of length two indicating limits for
    the x axis. The default is 'NULL'.
  }
  \item{\dots}{further arguments for graphics.
  }
}
\details{
  A sufficient overlap of the distribution of the (estimated) propensity
  score in treatment groups is needed to assure a meaningful
  stratification or matching by the (estimated) propensity score. To
  implement this, the density of the (estimated) propensity score is
  estimated and plotted per treatment group using \code{density()} and
  \code{lines()}, respectively.
}

\author{
  Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
\code{\link{density}}, \code{\link{lines}}
}
\examples{
data(pride)
pride.ps <- pscore(data=pride,formula=PCR_RSV~AGE+SEX+REGION)

pscore.plot(pride.ps,
            main="PRI.De study: Density estimation for the estimated PS",
            cex.main=1.75,
            cex.lab=1.5,
            cex.axis=1.25,
            with.legend=TRUE,
            legend.cex=1.5,
            legend.label=c("exposed", "unexposed"),
            par.1=list(lwd=2),
            par.0=list(lwd=2, lty=2))

}

\keyword{ plots }
\keyword{ density }
