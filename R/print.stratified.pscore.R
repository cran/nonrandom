
print.stratified.pscore <- function(object,
                                ...){

  cat("\n Stratified by: ", object$stratified.by, "\n")

  cat("\n Strata information: \n\n")

  df <-
    data.frame(cbind(object$intervals,
                     as.numeric(table(object$stratum.index)),
                     round(as.numeric(table(object$stratum.index))/dim(object$data)[1],3)))
  
  colnames(df) <- c("  Strata bounds","    n","    n (per cent)")

  print(df)

}


