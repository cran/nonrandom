\name{plot.pscore}
\alias{plot.pscore}

\title{

  Graphical check for propensity score distributions in treatment
  groups

}

\description{

  Plot propensity score density in treatment groups

}

\usage{

\method{plot}{pscore}(x, par.dens=NULL, par.1=NULL, par.0=NULL,
  with.legend=FALSE, legend.cex=0.9, legend.label=NULL,
  main=NULL, ylim=NULL, xlim=NULL, ...)

}

\arguments{

  \item{x}{an object of class 'pscore'.}
  
  \item{par.dens}{a list of parameters needed for function
  \code{density()} internally used. The default is NULL, i.e. default
  parameters in \code{density()} are used.}
  
  \item{par.1}{a list of parameters needed for function \code{lines()}
  internally used for the presentation of the propensity score density
  for individuals labeled by treat='1'. The default is NULL,
  i.e. default parameter in \code{lines()} are used.}

  \item{par.0}{a list of parameters needed for function \code{lines()}
  internally used for the presentation of the propensity score density
  for individuals labeled by treat='0'. The default is NULL,
  i.e. 'lty=2' and remaining default parameter in \code{lines()} are
  used.}

  \item{with.legend}{a logical value for showing a legend.}

  \item{legend.cex}{a numeric value indicating the cex of the legend
    font.}

  \item{legend.label}{a vector of two strings labeling treated and
    untreated individuals. The default is NULL, i.e. c('treated',
    'untreated') is used.}
  
  \item{main}{a string indicating the title of the plot.}
 
  \item{ylim}{a vector of two numerics indicating the limits of the
    y-axis.}

  \item{xlim}{a vector of two numerics indicating the limits of the
    x-axis.}

  \item{...}{further arguments for graphics.}
}

\details{

  Propensity score methods aim to eliminate imbalances in covariate
  distributions between treatment groups. Therefore, individuals from
  both treatments are matched together or individuals are stratified
  based on their propensity score. To do so, the graphical check of
  propensity score distributions for treated and untreated individuals
  is useful.
  
  The use of \code{plot.pscore()} requires the use of \code{(pscore)}.
}

\author{Susanne Stampf \email{susanne.stampf@usb.ch}
}

\seealso{
  \link{plot}
}
\examples{
## STU1
data(stu1)
stu1.ps <- pscore(data    = stu1, 
                  formula = therapie~tgr+age)

plot.pscore(x    = stu1.ps,
            main = "PS distribution",
            xlab = "",
            par.1=list(col="red"),
            par.0=list(lwd=2),
            par.dens=list(kernel="gaussian"))
}

\keyword{
  plots
}

