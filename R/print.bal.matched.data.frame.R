print.bal.matched.data.frame <- function(x,
                                         ...)
{  
  cat("\n Summary of balance check: \n\n")
  print(x$bal.test$balance.table.summary)

  if ( length(x$bal.test$covariates.NA)==0 ){
    cat("\n\n Covariates not completely tested: ---\n")
  }else{
    cat("\n\n Covariates not completely tested:\n")
    cat(x$bal.test$covariates.NA, "\n")
  }

  cat("\n\n Detailed balance check (overall): \n\n")  
  print(x$bal.test$balance.table)
  
  if (is.null(x$bal.test$p.value)){   
    cat(paste("\n\n Detailed balance check:\n [Standardized differences (cut point: ",
              x$bal.test$alpha, ")]\n\n", sep=""))
    
    if (dim(x$bal.test$Standardized.difference)[2]==1){
      str.val <-
        t(t(round(x$bal.test$Standardized.difference[2:dim(x$bal.test$Standardized.difference)[1],],3)))
    }else{
      str.val <-
        round(x$bal.test$Standardized.difference[2:dim(x$bal.test$Standardized.difference)[1],],3)
    }   
    print(format(data.frame(rbind(round(x$bal.test$Standardized.difference[1,],3),
                                  rep("-----", times=dim(x$bal.test$Standardized.difference)[2]),
                                  str.val,
                                  rep("",times=dim(x$bal.test$Standardized.difference)[2]),
                                  rep("----", times=dim(x$bal.test$Standardized.difference)[2]),
                                  x$bal.test$method),
                            row.names=c("Before",
                              "------",
                              "After",
                              "",
                              "---------",
                              "Scale"))))
    cat("\n")    
  }else{    
    cat(paste("\n\n Detailed balance check:\n [p-values from tests (significance level: ",
              x$bal.test$alpha/100, ")]\n\n", sep=""))
    
    if (dim(x$bal.test$p.value)[2]==1){
      str.val <- t(t(x$bal.test$p.value[2:dim(x$bal.test$p.value)[1],]))
    }else{
      str.val <- x$bal.test$p.value[2:dim(x$bal.test$p.value)[1],]
    }    
    print(format(data.frame(rbind(x$bal.test$p.value[1,],                                  
                                  rep("-----", times=dim(x$bal.test$p.value)[2]),
                                  str.val,
                                  rep("",times=dim(x$bal.test$p.value)[2]),
                                  rep("----", times=dim(x$bal.test$p.value)[2]),
                                  x$bal.test$method),
                            row.names=c("Before",                              
                              "------",
                              "After",
                              "",
                              "---------",
                              "Test"))))
    cat("\n")  
  }
}



