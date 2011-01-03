\name{dist.plot}
\alias{dist.plot}
\title{
  Graphical balance check for covariate distributions in treatment
  groups
}
\description{
  Plot covariate disitribution in treatment groups
}
\usage{
  dist.plot(object,sel=NULL, treat=NULL, stratum.index=NULL,
  match.index=NULL, plot.type=1, compare=FALSE, cat.levels=10, 
  plot.levels=5, label.match=NULL, label.stratum=c("Stratum","Orginal"),
  with.legend=TRUE, legend.title=NULL, legend.cex=0.9, myoma=c(2,2,2,2),
  mymar=c(2,4,1,2), width=0.5, xlim=NULL, ylim=NULL, col=NULL, las=1,
  font.main=2, font=1, main=NULL, main.cex=1.2, sub.cex=0.9,
  bar.cex=0.8, ...)
}

\arguments{
  \item{object}{an object of class 'pscore', 'stratified.pscore',
    'stratified.data.frame', 'matched.pscore', 'matched.data.frame',
    'matched.data.frames' or a data frame. If object class is 'pscore',
    arguments \code{stratum} or \code{match.index} are ignored.}
  
  \item{sel}{a data frame or a vector of integers or strings indicating
    covariates whose distribution to be plotted. The default is
    'NULL', i.e., complete data are selected.}
  
  \item{treat}{an integer or a string indicating treatment in 'data' and
    'data.matched' if \code{ps.match()} is previously used,
    respectively. The default is 'NULL'. If the class of the input
    object is 'stratified.pscore' or 'matched.pscore', no specification
    is needed.}

  \item{stratum.index}{an integer or a string indicating the vector
    containing stratum indices in stratified data. The default is
    'NULL'. No specification is needed if \code{ps.makestrata()} is
    previously used.}

  \item{match.index}{an integer or a string indicating the vector
    containing matching indices in data and in the matched data. The
    default is 'NULL'. No specification is needed if \code{ps.match()}
    is previously used.}

  \item{plot.type}{an integer specifying the type of plot. The default
    is '1', i.e., means for non-categorical and frequencies for
    categorical covariates are plotted as barplots separated by
    treatment. If \code{plot.type} is set to '2', histograms are
    plotted.}

  \item{compare}{a logical value indicating whether covariate
    distribution in the original data are plotted as well. The default
    is 'FALSE'.}

  \item{cat.levels}{an integer. The default is '10', i.e., covariates
    with more than ten different values are considered as
    non-categorical.}

  \item{plot.levels}{an integer. The default is '5', i.e., five
    cutpoints are used to define classes for histograms for
    non-categorical covariates. The classification still depends on the
    data structure such that the number of classes can differ from the
    statement in \code{plot.levels}.}  
  
  \item{label.match}{a vector of two strings describing the labels for
    original and matched data. The default is 'NULL', i.e.,
    c('Original', 'Matched') is used.}

  \item{label.stratum}{a vector of two strings describing the labels for
    stratum-specific data. The default is c('Original', 'Stratum').}

  \item{with.legend}{a logical value indicating whether a legend is
    shown. The default is 'TRUE'.}
  
  \item{legend.title}{a string indicating the legend title. The default
    is 'NULL', i.e, either categories of categorical covariates or
    treatment labels for non-categorical covariates are given if
    \code{plot.type}='1'. For \code{plot.type}='2', treatment labels are
    shown.}

  \item{legend.cex}{a numeric indicating the font size in the
    legend. The default is '0.9'.}

  \item{myoma}{the size of outer margins, see \code{par}. The default is
    'c(2,2,2,2)'.}

  \item{mymar}{margins to be specified on the four sides of the plot,
    see \code{par}. The default is 'c(2,4,1,2)' if \code{plot.type}='1'
    and 'c(5,4,4,2)+0.1' if \code{plot.type}='2'.}

  \item{width}{an integer indicating bar widths. The default is '0.5'.}

  \item{xlim}{a vector of integers of length two indicating limits for
    the x axis. The default is 'NULL'.}

  \item{ylim}{a vector of integers of length two indicating limits for
    the y axis. The default is 'NULL'. It is only meaningful if
    \code{plot.type}='2'.}
  
  \item{col}{a vector of colors for bars or bar components. The default
    is 'NULL'.}

  \item{las}{a integer indicating the style of axis labels, see
    \code{par}. The default is '1'.}

  \item{font.main}{an integer indicating the font to be used for plot
    main titles, see \code{par}. The default is '2'.}
  
  \item{font}{an integer specifying the font to use for text, see
    \code{par}. The default is '1'.}

  \item{main}{a string indicating the main title for graphics. The
    default is 'NULL'.}

  \item{main.cex}{a numeric indicating the font size of main title. The
    default is '1.2'.}

  \item{sub.cex}{a numeric indicating the font size of sub titles. The
    default is '0.9'.}

  \item{bar.cex}{a numeric indicating the font size of bar titles. The
    default is '0.8'.}
  
  \item{...}{further arguments for graphics.}
}
\details{
  Propensity score methods aims to eliminate imbalances in covariate
  distributions between treatment groups. An important issue is to
  check those after stratification or matching.
  
  The usage of \code{dist.plot()} depends on the class of the input
  object. If either \code{ps.makestrata()} or \code{ps.match()} are
  previously used, \code{treat}, \code{match.index} and
  \code{stratum.index} are not needed, contrary to the case where the
  input object is a data frame.
}
\value{
  \code{dist.plot()} returns a list with information about the
  plots. The number and the manner of the list entries depends both on
  \code{plot.type} and on the type of covariates to be plotted:

  \item{name.sel}{a string containing names of selected covariates.}

  \item{sel}{a data frame containing selected covariates labeled by
    'name.sel'.}
  
  \item{name.treat}{a string indicating the name of treatment used.}

  \item{treat}{a numeric vector containing treatment labeled by
    'name.treat'.}

  \item{name.stratum.index}{a string indicating the name of stratum
    indices used.}

  \item{stratum.index}{a numeric vector containing stratum indices
    labeled by 'name.stratum.index'.}
    
  \item{name.match.index}{a string indicating the name of matching
    indices used.}
 
  \item{match.index}{a numeric vector containing matching indices
    labeled by 'name.match.index'.}
     
  \item{var.cat}{a string indicating the names of categorical
    variables.}
  
  \item{var.noncat}{a string indicating the names of non-categorical
    variables.}

  \item{mean}{a list of length two including means of non-categorical
    covariates separated by treatment. If \code{compare}='FALSE', list
    elements are matrices with means separated by treatment (rows) and
    strata or matched/unmatched data (columns), respectively. If
    \code{compare}='TRUE', the list elements are again lists including
    means before (first list element) and after (second list element)
    stratification or matching. The order of list elements is in
    accordance to the order of 'var.noncat'. It is only available if
    \code{plot.type}=1.}
  
  \item{frequency}{a list with length according to the number of
    categorical covariates whereas the list elements depend on the
    specification of \code{compare}. If \code{compare}='FALSE', list
    elements contain standardized frequency tables separated by
    treatment (rows) and by strata or by matched/unmatched data
    (columns). The order and the number of list elements is
    w.r.t. 'var.cat'. If \code{compare}='TRUE', there are two list
    elements which are again lists of frequency tables. The first list
    element contains lists of frequency tables separated by treatment
    before stratification or matching and the second list element
    includes lists of frequency tables separated by treatment and strata
    or matched/unmatched data. The order of list elements is in
    accordance to the order of 'var.cat'. It is only available if
    \code{plot.type}=1.}

  \item{breaks.noncat}{a list with length according to the number of
    non-categorical covariates. The list entries are numerics indicating
    the cutpoints of histogram classes. It is only available if
    \code{plot.type}=2.}

  \item{x.cat}{a list with length according to the number of categorical
    covariates. It contains frequencies w.r.t. treatment with the lower
    value, e.g., '0' or 'No', before stratification or matching. It is
    only available if \code{plot.type}=2 and \code{compare}='TRUE'.}

  \item{y.cat}{a list with length according to the number of categorical
    covariates. It contains frequencies w.r.t. treatment with the upper
    value, e.g., '1' or 'Yes', before stratification or matching. It is
    only available if \code{plot.type}=2 and \code{compare}='TRUE'.}
  
  \item{x.s.cat}{a list with length according to the number of
    categorical covariates. It contains frequencies w.r.t. strata
    (columns) and treatment with the lower value, e.g., '0' or 'No'
    after stratification. If \code{match.index} is used, frequencies are
    given w.r.t. treatment with the lower value and w.r.t. the original
    data (first column) and the matched data (second column). It is only
    available if \code{plot.type}=2.}

  \item{y.s.cat}{a list with length according to the number of
    categorical covariates. It contains frequencies w.r.t. strata
    (columns) and treatment with the upper value, e.g., '1' or 'Yes'
    after stratification. If \code{match.index} is used, frequencies are
    given w.r.t. treatment with the upper value and w.r.t. the original
    data (first column) and the matched data (second column). It is only
    available if \code{plot.type}=2.}

  \item{x.noncat}{a list with length according to the number of
    non-categorical covariates. It contains frequencies in histogram
    classes w.r.t. treatment with the lower value, e.g., '0' or 'No',
    before stratification or matching. It is only available if
    \code{plot.type}=2 and \code{compare}='TRUE'.}

  \item{y.noncat}{a list with length according to the number of
    non-categorical covariates. It contains frequencies in histogram
    classes w.r.t. treatment with the upper value, e.g., '1' or 'Yes',
    before stratification or matching. It is only available if
    \code{plot.type}=2 and \code{compare}='TRUE'.}

  \item{x.s.noncat}{a list with length according to number of
    non-categorical covariates. It contains lists with frequencies in
    histogram classes w.r.t. strata and treatment with the lower value,
    e.g., '0' or 'No', after stratification. If \code{match.index} is
    used, frequencies in histogram classes are given w.r.t. treatment
    with the lower value and w.r.t. the original data and the matched
    dats. It is only available if \code{plot.type}=2.}

  \item{y.s.noncat}{a list with length according to number of
    non-categorical covariates. It contains lists with frequencies in
    histogram classes w.r.t. strata and treatment with the upper value,
    e.g., '1' or 'Yes', after stratification. If \code{match.index} is
    used, frequencies in histogram classes are given w.r.t. treatment
    with the upper value and w.r.t. the original data and the matched
    dats. It is only available if \code{plot.type}=2.}
}

\author{Susanne Stampf \email{susta@imbi.uni-freiburg.de}
}

\seealso{
  \link{barplot}
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
stu1.plot <- 
    dist.plot(object      = stu1.match, 
              sel         = c("age"),
              compare     = TRUE,
              plot.type   = 2,
              with.legend = FALSE)

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
pride.plot <- 
   dist.plot(object    = pride.strata,      
             sel       = c("REGION", "AGE"),
             plot.type = 1)                  ## default

}
\keyword{
  plots
}

