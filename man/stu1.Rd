\name{stu1}
\alias{stu1}
\docType{data}
\title{Data example stu1}

\description{

  Data on quality of life in n=646 breast cancer patients in an
  observational treatment study of the German Breast Cancer Study Group
  (GBSG) }

\usage{data(stu1)
}

\format{
  A data frame with 648 observations on the following 9 variables.
  \describe{
    \item{klinik}{clinic center}
    \item{idnr}{patient id}
    \item{tmass}{tumor size in mm}
    \item{alter}{age}
    \item{therapie}{therapy:
      \describe{
       \item{1:}{mastectomy} 
       \item{2:}{breast conservation}
     }
   }
   \item{tgr}{tumor size:
      \describe{
       \item{1:}{<= 10 mm} 
       \item{2:}{>  10 mm}
     }
   }
   \item{age}{age:
      \describe{
       \item{1:}{<= 55 yrs} 
       \item{2:}{>  55 yrs}
     }
   }
   \item{ewb}{emotional status}
   \item{pst}{physical status}
 }
}

\examples{
data(stu1)
}
\keyword{datasets}
