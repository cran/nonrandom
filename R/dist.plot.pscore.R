
dist.plot.pscore <- function(object,
                             sel           = NULL,
                             treat         = NULL,
                             stratum.index = NULL,
                             match.index   = NULL,
                             plot.type     = 1,
                             compare       = FALSE,
                             ...)
{ 
  data <- object$data

  ## ########
  ## find sel
  if(is.null(sel)){
    sel <- data
  }else{
    sel <- find.sel(data = data,
                    sel  = sel)
  }
  
  ## ##########
  ## find treat
  if (!is.character(treat) & !is.numeric(treat) & !is.null(treat)){
    stop("Argument 'treat' must be either numeric or a string.")
  }else{
    if (is.null(treat)){   
      name.treat <- object$name.treat
      treat      <- object$treat
    }else{
      A <- find.treat(data  = data,
                      treat = treat)
      treat      <- A[[1]]
      name.treat <- A[[2]]
    }
  }

  
  ## ###################
  ## check stratum.index  
  stratum.index <- rep(1, times=dim(data)[1])
  name.stratum.index <- ""

  
  ## #################
  ## check match.index  
  match.index <- rep(1, times=dim(data)[1])
  name.stratum.index <- ""


  
  ## ###############################################
  ## check plot.type & calculate value to be plotted
  if (!is.numeric(plot.type)){    
    stop("Argument 'plot.type' must be numeric.")
  }else{
    if (plot.type == 1){
      dist.plot.bars(sel        = sel,
                     treat      = treat,
                     name.treat = name.treat,
                     index      = stratum.index,
                     name.index = name.stratum.index,
                     compare    = FALSE,
                     match.T    = FALSE,
                     ...)
    }else{
      if(plot.type == 2)
        dist.plot.hist(sel        = sel,
                       treat      = treat,
                       name.treat = name.treat,
                       index      = stratum.index,
                       name.index = name.stratum.index,
                       compare    = FALSE,
                       match.T    = FALSE,
                       ...)
      else
        stop("Argument 'plottype' must be either 1 or 2.")
    }
  }
}

