



#' @title Fixed Effect of \link[nlme]{lme} Object
#' 
#' @param x \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @note
#' We already have `nlme:::coef.lme`
#' 
#' Function [coef0.lme] is actually `nlme:::fixef.lme`.
#'  
#' @export
coef0.lme <- function(x) x$coefficients$fixed







# 'glmmPQL' inherits from 'lme'
# ggplot2::fortify has function(model, data, ...) 
#' @importFrom nlme getData getGroupsFormula
#' @importFrom stats formula predict
#' @export
fortify.lme <- function(model, data = getData(model), level = 0L, ...) { # nlme:::getData.lme
  z <- if (inherits(model, what = 'glmmPQL')) {
    predict(model, level = level, type = 'response') # ?MASS:::predict.glmmPQL
  } else predict(model, level = level) # ?nlme:::predict.lme
  
  if (!is.symbol(yvar <- formula(model)[[2L]])) stop('only symbol-endpoint accepted, yet')
  ynm <- as.character.default(yvar)
  if (!is.symbol(gvar <- getGroupsFormula(model)[[2L]])) stop('only symbol group name supported')
  gnm <- as.character.default(gvar)
  xnm <- all.vars(model$call$fixed[[3L]])
  
  ret <- data.frame(z, data[[gnm]], data[xnm])
  names(ret)[1:2] <- c(ynm, gnm)
  attr(ret, which = 'ynm') <- ynm
  attr(ret, which = 'gnm') <- gnm
  attr(ret, which = 'xnm') <- xnm
  return(ret)
}




#' @title autoplot objects from \pkg{nlme}
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param level ..
#' 
#' @param ... ..
#' 
#' @examples
#' # not written yet
#'
#' @importFrom nlme getData
#' @importFrom stats formula
#' @export
autoplot.lme <- function(object, level = 0L, ...) {
  
  data <- if (inherits(object, what = 'glmmPQL')) { # ?MASS::glmmPQL
    eval(object$call$data)
  } else getData(object) # nlme:::getData.lme
  
  if (!inherits(data, what = 'groupedData')) {
    stop('input (', class(data)[1L], ') must have a `groupedData` data')
  }
  
  attr(data, which = 'outer') <- NULL
  p_dat <- autoplot.groupedData(data, ...) + 
    labs(title = deparse1(object$call$data))
  
  # below: very preliminary
  dd <- fortify.lme(object, level = level, ...)
  ynm <- attr(dd, which = 'ynm', exact = TRUE)
  xnm <- attr(dd, which = 'xnm', exact = TRUE)
  gnm <- attr(dd, which = 'gnm', exact = TRUE)
  time_nm <- as.character.default(formula(data)[[3L]][[2L]])
  xnm <- setdiff(xnm, y = time_nm)
  p_dat + 
    geom_line(mapping = aes(x = dd[[time_nm]], y = dd[[ynm]], colour = dd[[xnm]], group = dd[[xnm]]), size = 1) +
    labs(colour = xnm)
  
}












