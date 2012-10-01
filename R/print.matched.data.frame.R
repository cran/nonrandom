
print.matched.data.frame <- function(object,
                                     ...){
  
  cat("\n Matched by: ", object$matched.by, "\n")
  
  cat("\n Matching parameter:\n")

  print(matrix(c(round(object$match.parameter$caliper,3),
                 object$match.parameter$ratio,
                 object$match.parameter$who.treated),
               nrow=3,ncol=1,
               dimnames=
               list(c("Caliper size:",
                      "Ratio:",
                      "Who is treated?:"), c(""))))
  

  cat("\n Matching information:\n")

  print(matrix(c(object$match.parameter$givenTmatchingC,
                 object$match.parameter$bestmatch.first),
               nrow=2,ncol=1,
               dimnames=
               list(c("Untreated to treated?:",
                      "Best match?:"), c(""))))

  df.treat <- as.vector(object$data[names(object$data)==object$name.treat])
  
  cat("\n Matching data:\n")

  print(matrix(c(length(object$match.index[object$match.index>0]),
                 length(object$match.index[object$match.index>0 &
                                           df.treat==object$match.parameter$who.treated]),
                 length(object$match.index[object$match.index>0 &
                                           df.treat!=object$match.parameter$who.treated]),
                 length(object$match.index[object$match.index==0]),
                 length(unique(object$match.index))-1,
                 sum(as.numeric(table(object$match.index[object$match.index>0])) !=
                     (object$match.parameter$ratio +1))),
               nrow=6,ncol=1,
               dimnames=
               list(c("Number of matched obs:",
                      "Number of matched treated obs:",
                      "Number of matched untreated obs:",
                      "Number of dropped obs:",
                      "Number of matching sets:",
                      "Number of incomplete matching sets:"), c(""))))
  
}


