\name{plot.stdf}
\alias{plot.stdf}
\title{
  Graphical check for standardized differences of covariates 
}
\description{
  Plot standardized differences of covariates
}
\usage{
  \method{plot}{stdf}(x, sel = NULL, plot.alpha = TRUE,
  mymar = c(5,8,4,2), pch.p = c(1,5), col.p = c("black", "red"),
  cex.p = 1.25, line.stdf = 1, line.alpha = 4, with.legend = TRUE,
  legend.label = c("before", "after"), legend.cex = 1, legend.xy = NULL,
  ...)
}

\arguments{

  \item{x}{an object of class 'bal.matched.data.frame',
  'bal.matched.data.frames' or 'bal.matched.pscore'. The previous use of
  \code{ps.balance(..., method='stand.diff', ...)} is needed.}

  \item{sel}{a vector of strings indicating covariates to be
    checked. The default is NULL, i.e. all variables checked in
    \code{ps.balance()} are selected.}

  \item{plot.alpha}{a logical value indicating whether a vertical line
  for the significance level chosen in \code{ps.balance()} should be
  plotted. The default is TRUE.}

  \item{mymar}{a graphical parameter. The default is c(5,8,4,2).}

  \item{pch.p}{a vector of two integers indicating the symbols. The
    default is c(1,5).}

  \item{col.p}{a vector of two strings indicating symbol colors in the
    plot. The default is c("black", "red").}

  \item{cex.p}{a numeric indicating the size of symbols in the plot. The
    default is 1.25.}

  \item{line.stdf}{an integer indicating the type of line that connects
  the sympols. The default is 1.}

  \item{line.alpha}{an integer indicating the line type presenting the
  significance level. The default is 4.}

  \item{with.legend}{a logical value indicating whether the legend is
    given. The default is TRUE.}

  \item{legend.label}{a vector of two strings indicating the labels. The
    default is c("before", "after").}

  \item{legend.cex}{a numeric indicating the size of the legend
    font. The default is 1.}

  \item{legend.xy}{a vector of two integers indicating x- and
  y-coordinates for legend position. The default is NULL.}

   \item{...}{further arguments for graphics.}
  
}

\details{

  Standardized difference are proposed to check the balance of
  covariates after matching. The function \code{plot.stdf()} offers a
  graphical check of it presenting standardized differences of
  covariates between treatment groups before and after matching.
  
  The usage of \code{plot.stdf()} requires the previous use of
  \code{ps.balance(..., method='stand.diff', ...)} and is only available
  when matching is done before via \code{ps.match()}.}

\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
  \link{plot}
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

stu1.bal.match <- ps.balance(object = stu1.match,
                             sel    = c("tgr","age"),
                             method = "stand.diff",
                             alpha  = 20)

plot.stdf(x          = stu1.bal.match,
          main       = "Stu1 study: Standardized differences",
          cex.axis   = 1.2,
          legend.cex = 1.3,
          cex.main   = 1.5,
          las        = 1,
          col.p      = c("black", "gray"))
}

\keyword{
  plots
}

