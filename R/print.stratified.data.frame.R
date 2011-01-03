
print.stratified.data.frame <- function(x,
                                        ...){

  cat("\n Stratified by: ", x$stratified.by,"\n")

  cat("\n Strata information: \n\n")

  df <-
    data.frame(cbind(x$intervals,
                     as.numeric(table(x$stratum.index)),
                     round(as.numeric(table(x$stratum.index))/dim(x$data)[1],3)*100
                     )
               )
  
  colnames(df) <- c("  Strata bounds","    n","    n (per cent)")


  print(df)

}


