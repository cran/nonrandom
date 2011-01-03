
print.relative.effect <- function(x,
                                  ...)
{

  cat("\n Treatment:",x$name.treat)
  cat("\n Outcome:",x$name.resp)
  cat("\n Covariates: ",x$name.sel, "\n\n")
  
  cat("\n Unadjusted treatment effect: ", round(x$unadj.treat,4),"\n", sep="")
  
  cat("\n Adjusted and relative effects: \n\n")
  
  rel.eff.tab <- matrix(c(as.numeric(x$adj.treat.cov),
                          as.numeric(x$rel.eff.treat)),
                        nrow=length(x$name.sel),
                        ncol=2,
                        dimnames=list(x$name.sel,
                          c("adj. treatment effect", "rel. effect")))

  if (dim(rel.eff.tab)[1] != 1)
  
    print(rel.eff.tab[order(x$rel.eff.treat, decreasing=TRUE),])

  else

    print(format(rel.eff.tab))
}
