
print.matched.pscore <- function(x,
                                 ...){

  cat("\n Matched by: ", x$matched.by, "\n")
  
  cat("\n Matching parameter:\n")

  print(matrix(c(round(x$match.parameters$caliper,3),
                 x$match.parameters$ratio,
                 x$match.parameters$who.treated),
               nrow=3,ncol=1,
               dimnames=
               list(c("Caliper size:",
                      "Ratio:",
                      "Who is treated?:"), c(""))))
  

  cat("\n Matching information:\n")

  print(matrix(c(x$match.parameters$givenTmatchingC,
                 x$match.parameters$bestmatch.first),
               nrow=2,ncol=1,
               dimnames=
               list(c("Untreated to treated?:",
                      "Best match?:"), c(""))))
  

  cat("\n Matching data:\n")

  print(matrix(c(length(x$match.index[x$match.index>0]),
                 length(x$match.index[x$match.index>0 &
                                           x$treat==x$match.parameters$who.treated]),
                 length(x$match.index[x$match.index>0 &
                                           x$treat!=x$match.parameters$who.treated]),
                 length(x$match.index[x$match.index==0]),
                 length(unique(x$match.index))-1,
                 sum(as.numeric(table(x$match.index[x$match.index>0])) !=
                     (x$match.parameters$ratio +1))),
               nrow=6,ncol=1,
               dimnames=
               list(c("Number of matched obs:",
                      "Number of matched treated obs:",
                      "Number of matched untreated obs:",
                      "Number of dropped obs:",
                      "Number of matching sets:",
                      "Number of incomplete matching sets:"), c(""))))
  
}


