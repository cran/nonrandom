
summary.stratified.pscore <- function(object,
                                      ...){

  df <-
    data.frame(cbind(object$intervals,
                     as.numeric(table(object$stratum.index)),
                     round(as.numeric(table(object$stratum.index))/dim(object$data)[1],3)*100))

  colnames(df) <- c("  Strata bounds","    n","    n (per cent)")
  
  sum.str <- list(str.var  = object$stratified.by,
                  str.info = df)

  class(sum.str) <- "summary.stratified.pscore"

  sum.str

}


