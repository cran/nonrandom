print.est.stratified.pscore <- function(x,
                                        ...)
{
  cat("\n Effect estimation for treatment/exposure on outcome \n")
  cat("\n Treatment/exposure:", x$name.treat)
  cat("\n Outcome:", x$name.resp)
  
  if( x$family == "binomial" ){
    cat("\n Effect measure: odds ratio ('or')\n\n")
    col.n <- c("or","SE[log[or]]","[95%-CI[or]]")
  }else{
    cat("\n Effect measure: difference ('effect')\n\n")
    col.n <- c("effect","SE[effect]","[95%-CI[effect]]")
  }
  
  if ( !is.list(x$lr.estimation) ){
    lr.eff.c <- lr.se.c <- NULL
    lr.eff.m <- lr.se.m <- NULL
    lr.ci.c <- lr.ci.m <- NULL
    
  }else{
    if( x$family == "binomial" ){
      lr.eff.c <- round(x$lr.estimation$effect,3)
      lr.se.c  <- round(x$lr.estimation$se,4)
      lr.eff.m <- round(x$lr.estimation$effect.marg,3)
      lr.se.m  <- round(x$lr.estimation$se.marg,4)

      lr.ci.c <- round(c(exp(log(lr.eff.c) - qnorm(0.975)*lr.se.c),
                         exp(log(lr.eff.c) + qnorm(0.975)*lr.se.c)),3)

      lr.ci.m <- round(c(exp(log(lr.eff.m) - qnorm(0.975)*lr.se.m),
                         exp(log(lr.eff.m) + qnorm(0.975)*lr.se.m)),3)
    }else{
      lr.eff.c <- round(x$lr.estimation$effect,3)
      lr.se.c  <- round(x$lr.estimation$se,4)
      
      lr.ci.c <- round(c(lr.eff.c - qnorm(0.975)*lr.se.c,
                         lr.eff.c + qnorm(0.975)*lr.se.c),3)
    }
  }
    
  if ( !is.list(x$ps.estimation$adj) ){    
    ps.adj.eff <-
      ps.adj.se <-
        ps.adj.ci <- NULL
  }else{
    ps.adj.eff <- round(x$ps.estimation$adj$effect,3)
    ps.adj.se  <- round(x$ps.estimation$adj$se,4)
    
    if( x$family == "binomial" ){
      ps.adj.ci <- round(c(exp(log(ps.adj.eff) - qnorm(0.975)*ps.adj.se),
                           exp(log(ps.adj.eff) + qnorm(0.975)*ps.adj.se)),3)     
    }else{
      ps.adj.ci <- round(c(ps.adj.eff - qnorm(0.975)*ps.adj.se,
                           ps.adj.eff + qnorm(0.975)*ps.adj.se),3)     
    }
  }
  
  if ( x$family=="binomial" ){

    crude <- round(x$ps.estimation$crude$effect,3)
    crude.se <- round(x$ps.estimation$crude$se,4)
    crude.ci <- round(c(exp(log(crude)-qnorm(0.975)*crude.se),
                        exp(log(crude)+qnorm(0.975)*crude.se)),3)
    
    rr <- round(x$ps.estimation$unadj$effect,3)
    rr.se <- round(x$ps.estimation$unadj$se,4)  
    mh <- round(x$ps.estimation$unadj$effect.mh,3)
    mh.se <- round(x$ps.estimation$unadj$se.mh,4)

    rr.ci <- round(c(exp(log(rr)-qnorm(0.975)*rr.se),
                     exp(log(rr)+qnorm(0.975)*rr.se)),3)
    mh.ci <- round(c(exp(log(mh)-qnorm(0.975)*mh.se),
                     exp(log(mh)+qnorm(0.975)*mh.se)),3)
  }else{
    crude <- round(x$ps.estimation$crude$effect,3)
    crude.se <- round(x$ps.estimation$crude$se,4)
    crude.ci <- round(c(crude-qnorm(0.975)*crude.se,
                        crude+qnorm(0.975)*crude.se),3)
    
    diff <- round(x$ps.estimation$unadj$effect,3)
    diff.se <- round(x$ps.estimation$unadj$se,4) 
    diff.ci <- round(c(diff-qnorm(0.975)*diff.se,
                       diff+qnorm(0.975)*diff.se),3)
  }

  if ( x$family=="binomial" ){    
    eff <- c(" -----",
             paste("",crude,sep=""),
             "",
             paste("",rr,sep=""),paste("",mh,sep=""),paste("",ps.adj.eff,sep=""),
             "",
             paste("",lr.eff.c,sep=""),paste("",lr.eff.m,sep=""),
             "")    
    se <- c(" -----------",
            paste("",crude.se,"", sep=""),
            "",
            paste("",rr.se,"", sep=""),paste("",mh.se,"", sep=""),paste("",ps.adj.se,"", sep=""),
            "",
            paste("",lr.se.c,"", sep=""),paste("",lr.se.m,"", sep=""),
            "")
    ci <- c(" ------------",
            paste("[",crude.ci[1],",",crude.ci[2], "]",sep=""),
            "",
            paste("[",rr.ci[1],",",rr.ci[2],"]",sep=""),paste("[",mh.ci[1],",",mh.ci[2],"]",sep=""),
            paste("[",ps.adj.ci[1],",",ps.adj.ci[2],"]",sep=""),
            "",
            paste("[",lr.ci.c[1],",",lr.ci.c[2],"]",sep=""),paste("[",lr.ci.m[1],",",lr.ci.m[2],"]",sep=""),
            "")
    df <- data.frame(cbind(eff, se, ci),
                     row.names=c("",
                       "Crude",
                       "Stratification", "  Outcome rates", "  MH", "  Adjusted",
                       "Regression", "  Conditional", "  Marginal", " "))
    colnames(df) <- col.n
  }else{
    eff <- c(" ------",
             paste("",crude,sep=""),
             "",
             paste("",diff,sep=""),paste("",ps.adj.eff,sep=""),paste("",lr.eff.c,sep=""),
             "")  
    se <- c(" ----------",
            paste("",crude.se,"", sep=""),
            "",
            paste("",diff.se,"", sep=""),paste("",ps.adj.se,"", sep=""),paste("",lr.se.c,"", sep=""),
            "")
    ci <- c(" ----------------",
            paste("[",crude.ci[1],",",crude.ci[2], "]",sep=""),
            "",
            paste("[",diff.ci[1],",",diff.ci[2],"]",sep=""),paste("[",ps.adj.ci[1],",",ps.adj.ci[2],"]",sep=""),
            paste("[",lr.ci.c[1],",",lr.ci.c[2],"]",sep=""),
            "")    
    df <- data.frame(cbind(eff, se, ci),
                     row.names=c("",
                       "Crude",
                       "Stratification", " Unadjusted", " Adjusted",
                       "Regression", " "))
    colnames(df) <- col.n 
  }
    cat("\n Table of effect estimates:\n\n")  
    print(format(df))  
}
